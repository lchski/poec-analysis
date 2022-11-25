library(tidyverse)

library(pdftools)

library(lubridate)
library(rvest)
library(glue)

proceeding_links <- read_html("https://publicorderemergencycommission.ca/public-hearings/") %>%
  html_elements(xpath = "//a[starts-with(@href, 'https://publicorderemergencycommission.ca/public-hearings/day')]")

proceedings <- tibble(
  url = proceeding_links %>% html_attr("href"),
  text = proceeding_links %>% html_text2()
) %>%
  extract(text, c("day", "date_raw"), regex = "Day ([0-9]{1,2}) - ([A-Za-z]* [0-9]{1,2}, [0-9]{4})", convert = TRUE) %>%
  mutate(date = mdy(date_raw)) %>%
  filter(! is.na(date)) %>%
  mutate(
    transcript_filename = glue("POEC-Public-Hearings-Volume-{day}-{date_raw %>% str_remove_all(., ',') %>% str_replace_all(., ' ', '-')}.pdf"),
    transcript_url = glue("https://publicorderemergencycommission.ca/files/documents/Transcripts/{transcript_filename}")
  )

transcripts_raw <- proceedings %>%
  filter(date != today()) %>% # transcripts aren't ready day-of (understandably!)
  select(day, date, transcript_url) %>%
  mutate(transcript = map(transcript_url, pdf_text))

transcripts <- transcripts_raw %>%
  select(-transcript_url, -date) %>%
  group_by(day) %>%
  unnest(c(transcript)) %>%
  mutate(page = row_number()) %>%
  select(day, page, transcript)

lines_raw <- transcripts %>%
  mutate(text = str_split(transcript, "\\n")) %>%
  group_by(day, page) %>%
  unnest(c(text)) %>%
  select(-transcript) %>%
  mutate(text = str_squish(text)) %>%
  filter(text != "") %>%
  mutate(line = row_number()) %>%
  ungroup() %>%
  select(day, page, line, text)

transcripts %>%
  write_csv("data/out/transcripts.csv")

lines_raw %>%
  write_csv("data/out/lines-raw.csv")

lines <- lines_raw %>%
  mutate(
    line_type = case_when(
      text == fixed("INTERNATIONAL REPORTING INC.") ~ "page_footer",
      str_detect(text, "^[0-9]{1,2} --- Upon commencing") ~ "proceedings_start",
      str_detect(text, "^[0-9]{1,2} --- Upon") ~ "time_marker",
      str_detect(text, "^[0-9]{1,2} --- [A-Z]") ~ "section_header",
      str_detect(text, "^[0-9]{1,2} [A-ZÈÉ\\.eci/ \\-'’(?:van)(?:den)(?:Mme)]*:") ~ "speaker_start",
      TRUE ~ "other"
    ),
    line_type = case_when(# corrections based on errors found during speaker standardization
      day == 28 & page == 197 & str_detect(text, fixed("7 MR. VIGNEAULT")) ~ "other",
      day == 27 & page == 294 & str_detect(text, fixed("18 And Canada:")) ~ "other",
      day == 26 & page == 84 & str_detect(text, fixed("20 And:")) ~ "other",
      day == 25 & page == 41 & str_detect(text, fixed("11 And:")) ~ "other",
      day == 24 & page == 114 & str_detect(text, fixed("26 added:")) ~ "other",
      day == 15 & page == 231 & str_detect(text, fixed("10 And:")) ~ "other",
      day == 13 & page == 103 & str_detect(text, fixed("6 in:")) ~ "other",
      day == 6 & page == 121 & str_detect(text, fixed("20 And:")) ~ "other",
      day == 1 & page == 73 & str_detect(text, fixed("18 SHEPPARD:")) ~ "section_header",
      day == 1 & page == 94 & str_detect(text, fixed("10 BY MR. DAN SHEPPARD:")) ~ "section_header",
      TRUE ~ line_type
    )
  ) %>%
  mutate(text = case_when(# correct speaker misattributions / typos
    day == 10 & page == 216 & str_detect(text, fixed("6 SUPT. NATALIA RODRIGUEZ: Well, the protesters")) ~ "6 SUPT. ROBERT DRUMMOND: Well, the protesters", # see misattribution: https://publicorderemergencycommission.ca/files/documents/Transcripts/POEC-Public-Hearings-Volume-10-October-26-2022.pdf#page=216
    day == 23 & page == 210 ~ str_replace(text, fixed("D/COMM: MICHAEL DUHEME:"), "D/COMM. MICHAEL DUHEME:"),
    TRUE ~ text
  )) %>%
  filter(line_type == "speaker_start") %>%
  mutate(speaker = str_remove(text, "^[0-9]{1,2} ")) %>%
  separate(speaker, into = c("speaker"), sep = ":", extra = "drop") %>%
  mutate(speaker = str_squish(speaker)) %>%
  mutate(
    speaker_standardized = str_to_lower(speaker),
    speaker_standardized = str_remove(speaker_standardized, "^m\\. |^ms\\. |^mr\\.? |^jmr\\. |^\\. ms\\. |^m?me "),
    speaker_standardized = str_squish(speaker_standardized),
    speaker_standardized = str_replace_all(speaker_standardized, c(
      "acting sup\\." = "acting supt.",
      "^comm " = "comm. ",
      "^d/comm " = "d/comm. ",
      "^dm " = "dm. ",
      "^adm " = "adm. ",
      "^dsg " = "dsg. ",
      "^mayer" = "mayor",
      "^ministre " = "minister ",
      "^supt " = "supt. "
    )),
    speaker_standardized = str_replace_all(speaker_standardized, c(
      "cynthia termorhuizen|cynthia termoshuizen" = "cynthia termorshuizen",
      "alexandra haine" = "alexandra heine",
      "allision" = "allison",
      "alysssa" = "alyssa",
      "antione d’ailly|antoine d’ailly" = "antoine d'ailly",
      "bath sheba|bath-shéba" = "bath-sheba",
      "brandon miller|brendon miller" = "brendan miller",
      "brendan van niejenuis" = "brendan van niejenhuis",
      "carsonn pardy" = "carson pardy",
      "cafra zwibel" = "cara zwibel",
      "chris barber" = "christopher barber",
      "christpher diana|christoper diana" = "christopher diana",
      "collen mckeown" = "colleen mckeown",
      "comm\\. brenda luckie" = "comm. brenda lucki",
      "comimssion rouleau|commisioner rouleau|commissaire rouleau|commissiare rouleau|commission rouleau|^commissioner$|commissioner roleau|commissioner rousseau|commmissioner rouleau|member rouleau|the commissioner" = "commissioner rouleau",
      "conseiller|counsillor" = "councillor",
      "^craig abrams$|^supt\\. craig adams$" = "supt. craig abrams",
      "dan sheppard" = "daniel sheppard",
      "david migcovsky|david migicovksy" = "david migicovsky",
      "^dominic rochon$" = "adm. dominic rochon",
      "^dm\\. ian freeman" = "adm. ian freeman",
      "dr\\. robert stewart" = "dm. robert stewart",
      "emelie taman|emily taman" = "emilie taman",
      "ds\\.? jacqueline bodgen|ds jacqueline bogden" = "jacqueline bogden", # remove title, though maybe we'll re-add for consistency with DMs / ADMs? (same with Charette, Thomas, ...?)
      "eric brosseau" = "eric brousseau",
      "guillame sirois-gingras|guillaume sirois gingras" = "guillaume sirois-gingras",
      "hana laura yanamoto" = "hana laura yamamoto",
      "heather patterson" = "heather paterson",
      "interim chief bell" = "interim chief steve bell",
      "interlocuteur inconnu|interlocuteur non identifié|unidentified male spee?aker|unknown speaker" = "unidentified speaker",
      "jean-simon shoenholz|jean-simon-schoenholz" = "jean-simon schoenholz",
      "jeffrey hutchinsob|jeffrey hutchinson" = "jeffery hutchinson",
      "jeffery leon" = "jeffrey leon",
      "jessica barrows" = "jessica barrow",
      "jinan kuburski|jinian kubursi" = "jinan kubursi",
      "johm mather" = "john mather",
      "kathleen turner" = "kathleen tanner",
      "keving mchale" = "kevin mchale",
      "^la greffière|^clerk" = "the clerk",
      "lauren peerce|lorne peirce" = "lauren pearce",
      "maggie hope-braun" = "margaret hope-braun",
      "manday england" = "mandy england",
      "marie-helene chayer" = "marie-hélène chayer",
      "jason honner" = "alan honner",
      "jimmy willet$" = "jimmy willett",
      "mike morris" = "michael morris",
      "ministeromar alghabra" = "minister omar alghabra",
      "natalia rodgriguez|natalia rodiguez|natalia rodriguiez|nathalia rodriguez" = "natalia rodriguez",
      "^paul cham$" = "paul champ",
      "rob kitteredge|rob kittridge" = "rob kittredge",
      "swerge arpin" = "serge arpin",
      "shontana chaudhury" = "shantona chaudhury",
      "steeve chartrand" = "steeve charland",
      "stephen armstong" = "stephen armstrong",
      "steven aylward" = "stephen aylward",
      "steven kanellakos" = "steve kanellakos",
      "sujit choudhury" = "sujit choudhry",
      "the register" = "the registrar",
      "tom mcrae" = "thomas mcrae",
      "^st-pierre$" = "nicolas st-pierre",
      "supt\\. bernier" = "supt. robert bernier"
    ))
  )

# TODO idea: speaker type, counsel, lawyer, admin, witness (based on TOC entries?)


# page_header = line 1?
# time type: recessing, resuming, breaking, adjourning at ... 


