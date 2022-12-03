library(tidyverse)

library(pdftools)

library(lubridate)
library(rvest)

speaker_annotations <- read_csv("data/indices/speakers-standardized-annotations.csv") %>%
  separate(speaker_title, into = c("speaker_title", "speaker_title_abbr"), sep = "\\(", fill = "right") %>%
  mutate(
    speaker_title = str_squish(speaker_title),
    speaker_title_abbr = str_remove(speaker_title_abbr, "\\)$")
  ) %>%
  separate(speaker_affiliation, into = c("speaker_affiliation", "speaker_affiliation_abbr"), sep = "\\(", fill = "right") %>%
  mutate(
    speaker_affiliation = str_squish(speaker_affiliation),
    speaker_affiliation_abbr = str_remove(speaker_affiliation_abbr, "\\)$")
  )


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
    transcript_filename = str_glue("POEC-Public-Hearings-Volume-{day}-{date_raw %>% str_remove_all(., ',') %>% str_replace_all(., ' ', '-')}.pdf"),
    transcript_url = str_glue("https://publicorderemergencycommission.ca/files/documents/Transcripts/{transcript_filename}")
  )

rm(proceeding_links)

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
  arrange(day, page, line) %>%
  mutate(
    line_id = str_glue("{str_pad(day, 2, 'left', '0')}-{str_pad(page, 3, 'left', '0')}-{str_pad(line, 2, 'left', '0')}"),
    page_type = case_when(
      line == 1 & text == "Public Hearing Audience publique" ~ "title",
      line == 1 & str_detect(text, "^(?: ?ROUNDTABLE DISCUSSION )?[0-9]{1,3}") ~ "testimony",
      line == 1 & str_detect(text, "^[IV]{1,3}") ~ "front_matter",
      line == 1 ~ "other",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(text = case_when(# correct speaker misattributions / typos before any more processing occurs
    day == 10 & page == 216 & str_detect(text, fixed("6 SUPT. NATALIA RODRIGUEZ: Well, the protesters")) ~ "6 SUPT. ROBERT DRUMMOND: Well, the protesters", # see misattribution: https://publicorderemergencycommission.ca/files/documents/Transcripts/POEC-Public-Hearings-Volume-10-October-26-2022.pdf#page=216
    day == 23 & page == 210 ~ str_replace(text, fixed("D/COMM: MICHAEL DUHEME:"), "D/COMM. MICHAEL DUHEME:"),
    line_id == "04-259-05" & text == "3 Good afternoon, Mr. Ayotte. May name is Rob" ~ "3 MR. ROB KITTREDGE: Good afternoon, Mr. Ayotte. May name is Rob", # missing speaker intro, per debugging/unexpected-testimony-line-type
    line_id == "04-265-09" & text == "7 Good afternoon, Commissioner. Mr. Ayotte, my" ~ "7 MR. ANDREW GIBBS: Good afternoon, Commissioner. Mr. Ayotte, my", # missing speaker intro, per debugging/unexpected-testimony-line-type
    line_id == "05-100-08" & text == "6 Good afternoon, Councillor Deans and" ~ "6 MS. REBECCA JONES: Good afternoon, Councillor Deans and", # missing speaker intro, per debugging/unexpected-testimony-line-type
    line_id == "05-185-15" & text == "13 Councillor Deans, am I correct that your evidence" ~ "13 MS. ALYSSA TOMKINS: Councillor Deans, am I correct that your evidence", # missing speaker intro, per debugging/unexpected-testimony-line-type
    line_id == "05-295-18" & text == "16 Good evening, sir, my name is Alan Honner and I’m" ~ "16 MR. ALAN HONNER: Good evening, sir, my name is Alan Honner and I’m", # missing speaker intro, per debugging/unexpected-testimony-line-type
    line_id == "15-197-12" & text == "10 Mr. Marazzo, Tom Curry for former Chief Sloly." ~ "10 MR. TOM CURRY: Mr. Marazzo, Tom Curry for former Chief Sloly.", # missing speaker intro, per debugging/unexpected-testimony-line-type
    line_id == "16-359-26" & text == "24 --- Upon recessing at 7:32 p.m." ~ "24 --- Upon adjourning at 7:32 p.m.", # transcript reads "recessing" but context indicates it's adjournment (it was a late night!)
    line_id == "32-183-12" & text == "10 --- Upon recessing at 4:59 p.m." ~ "10 --- Upon adjourning at 4:59 p.m.", # last time_marker of day, context indicates adjournment
    day == 6 & page >= 117 & page <= 126 & str_detect(text, fixed("MR. CHRISTOPHER DEANS: ")) ~ str_replace(text, fixed("MR. CHRISTOPHER DEANS: "), "MR. CHRISTOPHER DIANA: "), # context indicates last name of "DEANS" was typo for "DIANA" (appears nowhere else in testimony)
    day == 27 & page >= 13 & page <= 18 & str_detect(text, fixed("MR. GORDON CAMPBELL: ")) ~ str_replace(text, fixed("MR. GORDON CAMPBELL: "), "MR. GORDON CAMERON: "), # context indicates last name of "CAMPBELL" was typo for "CAMERON" (appears nowhere else in testimony)
    day == 30 & page >= 323 & page <= 328 & str_detect(text, "^[0-9]{1,2} ` ") ~ str_remove(text, fixed("` ")), # speaker attribution lines have a "` " at the start
    line_id == "20-034-04" & text == "2 ] MAYOR JIMMY WILLETT: This is my understanding of" ~ str_remove(text, fixed("] ")), # erroneous "]" in speaker line
    line_id == "28-168-20" & text == "18 MINISTER MARCO MENDICINO: I think that’s fair," ~ "18 MINISTER MARCO MENDICINO: I think that’s fair, yes.", # see next correction, of line 28-168-21
    line_id == "28-168-21" & text == "19 yes. MS. CARA ZWIBEL: Okay, thank you." ~ "19 MS. CARA ZWIBEL: Okay, thank you.", # "yes" from previous line (28-168-20) cuts into this one (see also previous correction, adding the "yes" to the previous line)
    TRUE ~ text
  )) %>%
  group_by(day, page) %>%
  fill(page_type, .direction = "down") %>%
  mutate(
    line_type = case_when(
      line == 1 ~ "page_header",
      text == fixed("INTERNATIONAL REPORTING INC.") ~ "page_footer",
      str_detect(text, "^[0-9]{1,2} C E R T I F I C A T I O N") ~ "transcript_certification",
      str_detect(text, "^[0-9]{1,2} --- Upon commencing") ~ "proceedings_start",
      str_detect(text, "^[0-9]{1,2} ---? ?Upon adjourning") ~ "proceedings_end",
      str_detect(text, "^[0-9]{1,2} ---? ?Upon") ~ "time_marker",
      str_detect(text, "^[0-9]{1,2} --- [A-Z]") ~ "section_header",
      str_detect(text, "^[0-9]{1,2} [A-ZÈÉÇ\\.eci/ \\-'’(?:van)(?:den)(?:Mme)(?:Del)]*:") ~ "speaker_start",
      TRUE ~ "other"
    ),
    line_type = case_when(# corrections based on errors found during speaker standardization
      day == 30 & page == 220 & str_detect(text, fixed("15 Finance:")) ~ "other",
      day == 30 & page == 167 & str_detect(text, fixed("15 ---EXAMINATION IN-CHIEF BY MS. SHANTONA CHAUDHURY:")) ~ "section_header",
      day == 28 & page == 197 & str_detect(text, fixed("7 MR. VIGNEAULT")) ~ "other",
      day == 27 & page == 294 & str_detect(text, fixed("18 And Canada:")) ~ "other",
      day == 26 & page == 84 & str_detect(text, fixed("20 And:")) ~ "other",
      day == 25 & page == 41 & str_detect(text, fixed("11 And:")) ~ "other",
      day == 24 & page == 114 & str_detect(text, fixed("26 added:")) ~ "other",
      day == 15 & page == 231 & str_detect(text, fixed("10 And:")) ~ "other",
      day == 13 & page == 103 & str_detect(text, fixed("6 in:")) ~ "other",
      day == 6 & page == 121 & str_detect(text, fixed("20 And:")) ~ "other",
      day == 1 & page == 73 & str_detect(text, fixed("18 SHEPPARD:")) ~ "section_header", # multi-line section header
      day == 1 & page == 94 & str_detect(text, fixed("10 BY MR. DAN SHEPPARD:")) ~ "section_header", # multi-line section header
      line_id == "01-073-18" & text == "17 HEALTH MEASURES IMPLEMENTED IN CANADA PRESENTED BY MR. DAN" ~ "section_header", # multi-line section header, per debugging/unexpected-testimony-line-type
      line_id == "01-088-18" & text == "17 CHALLENGES RELATING TO PUBLIC HEALTH MEASURES PRESENTED BY MR." ~ "section_header", # multi-line section header, per debugging/unexpected-testimony-line-type
      line_id == "01-103-03" & text == "1 ÉTIENNE LACOMBE" ~ "section_header", # multi-line section header, per debugging/unexpected-testimony-line-type
      line_id == "01-109-08" & text == "6 GOVERNMENT IN RELATIONS TO THE EMERGENCIES ACT PRESENTED BY MR." ~ "section_header", # multi-line section header, per debugging/unexpected-testimony-line-type
      line_id == "01-109-09" & text == "7 ÉTIENNE LACOMBE" ~ "section_header", # multi-line section header, per debugging/unexpected-testimony-line-type
      line_id == "35-102-04" & text == "2 PROTESTS AND EMERGENCIES:" ~ "section_header", # multi-line section header, per debugging/speaker_standardized_count
      line_id == "34-007-19" & text == "17 EMERGENCIES:" ~ "section_header", # multi-line section header, per debugging/speaker_standardized_count
      line_id == "32-009-26" & text == "25 STAKE IN PUBLIC PROTESTS, AND THEIR LIMITS" ~ "section_header", # multi-line section header, per debugging/unexpected-testimony-line-type
      line_id == "32-092-22" & text == "19 FINANCIAL INTELLIGENCE" ~ "section_header", # multi-line section header, per debugging/unexpected-testimony-line-type
      line_id == "33-007-20" & text == "19 THE ROLE OF SOCIAL MEDIA" ~ "section_header", # multi-line section header, per debugging/unexpected-testimony-line-type
      line_id == "33-091-05" & text == "2 GOODS AND SERVICES, CRITICAL INFRASTRUCTURE AND TRADE CORRIDORS" ~ "section_header", # multi-line section header, per debugging/unexpected-testimony-line-type
      line_id == "26-101-08" & text == "6 And Plan B:" ~ "other",
      line_id == "08-037-25" & text == "23 line:" ~ "other",
      line_id == "14-220-21" & text == "19 Ms. JESSICA BARROW: And you had testified that" ~ "speaker_start", # "Ms." instead of "MS." threw off the parser
      line_id == "14-220-27" & text == "25 Ms. JESSICA BARROW: Okay. Thank you." ~ "speaker_start", # "Ms." instead of "MS." threw off the parser
      TRUE ~ line_type
    ),
    line_type = case_when(
      page_type == "testimony" & line_type == "other" & str_detect(text, "^[0-9]{1,2}") ~ "testimony",
      TRUE ~ line_type
    )
  ) %>%
  extract(text, into = c("page_header"), "[0-9]{1,3} (.*)", remove = FALSE) %>%
  mutate(page_header = case_when(
    page_type == "testimony" & line_type == "page_header" ~ page_header,
    TRUE ~ NA_character_
  )) %>%
  mutate(
    line_type = case_when(
      page_type == "testimony" & line == 2 & ! is.na(lag(page_header)) & str_detect(text, "^Cr-|^Ct\\.-|^En\\-|^In-|^Re-|^\\(") ~ "page_subheader",
      TRUE ~ line_type
    ),
    page_subheader = case_when(
      line_type == "page_subheader" ~ text,
      TRUE ~ NA_character_
    )
  ) %>%
  fill(page_header, page_subheader, .direction = "downup") %>%
  ungroup() %>%
  mutate(speaker = if_else(line_type == "speaker_start", str_remove(text, "^[0-9]{1,2} "), NA_character_)) %>%
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
      "^supt " = "supt. ",
      "^dpm " = "deputy pm ",
      "^premier ministre" = "prime minister"
    )),
    speaker_standardized = str_replace_all(speaker_standardized, c(
      "cynthia termorhuizen|cynthia termoshuizen" = "cynthia termorshuizen",
      "alexandra haine" = "alexandra heine",
      "allision" = "allison",
      "alysssa" = "alyssa",
      "alyssa tompkins" = "alyssa tomkins",
      "antione d’ailly|antoine d’ailly" = "antoine d'ailly",
      "bath sheba|bath-shéba" = "bath-sheba",
      "brandon miller|brendon miller" = "brendan miller",
      "brendan van niejenuis" = "brendan van niejenhuis",
      "carsonn pardy" = "carson pardy",
      "cafra zwibel" = "cara zwibel",
      "chris barber" = "christopher barber",
      "christpher diana|christoper diana" = "christopher diana",
      "chrystia freeman" = "chrystia freeland",
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
      "eva chipuik" = "eva chipiuk",
      "graham reader" = "graham reeder",
      "guillame sirois-gingras|guillaume sirois gingras" = "guillaume sirois-gingras",
      "hana laura yanamoto" = "hana laura yamamoto",
      "heather patterson" = "heather paterson",
      "interim chief bell" = "interim chief steve bell",
      "interlocuteur inconnu|interlocuteur non identifié|unidentified male spee?aker|unknown speaker" = "unidentified speaker",
      "^jean-simon shoenholz$|^jean-simon-schoenholz$|^jean jean-simon schoenholz$" = "jean-simon schoenholz",
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
      "robert mckinnon" = "robert mackinnon",
      "swerge arpin" = "serge arpin",
      "shontana chaudhury|shantona chsudhury" = "shantona chaudhury",
      "steeve chartrand" = "steeve charland",
      "stephen armstong" = "stephen armstrong",
      "steven aylward" = "stephen aylward",
      "steven kanellakos" = "steve kanellakos",
      "sujit choudhury" = "sujit choudhry",
      "the register|^jason theriault$" = "the registrar",
      "tom mcrae" = "thomas mcrae",
      "^st-pierre$" = "nicolas st-pierre",
      "supt\\. bernier" = "supt. robert bernier",
      "chief jum ramer" = "chief jim ramer",
      "^christian leuprecht$|dr\\. christian leuprecht" = "prof. christian leuprecht",
      "dr\\. dax d'orazio" = "dr. dax d’orazio",
      "dr\\. denis baker" = "dr. dennis baker",
      "^david morin$" = "dr. david morin",
      "dr\\. jocelyn stacy" = "dr. jocelyn stacey",
      "dr\\. jack lindsay|^jack lindsay$" = "prof. jack lindsay", # https://www.brandonu.ca/ades/faculty-and-staff/
      "michael willams" = "michael williams",
      "patrick lelond" = "patrick leblond",
      "^richard moon$" = "prof. richard moon",
      "prof\\. vanessa macdonnell" = "dr. vanessa macdonnell",
      "dr\\. jean-francois gaudreault-desbiens" = "prof. jean-françois gaudreault-desbiens"
    ))
  ) %>%
  mutate(
    section_header = case_when(
      line_type == "section_header" ~ str_remove(text, "^[0-9]{1,2} --- "),
      TRUE ~ NA_character_
    ),
    section_header = str_remove(section_header, ":$")
  ) %>%
  group_by(day) %>%
  fill(section_header, .direction = "down") %>%
  ungroup() %>%
  select(line_id, day:text, page_type, line_type, page_header, page_subheader, section_header, speaker, speaker_standardized)

# extend `line_type==transcript_certification` to lines after the `transcript_certification` for a day's testimony
# NB: I don't love this approach of rewriting a variable based on its own value (since it can cause effects if the script isn't re-run top-to-bottom), but it's good enough for now
lines <- lines %>% left_join(
    lines %>%
      filter(line_type == "transcript_certification") %>%
      select(day, end_page = page, end_line = line)
  ) %>%
  mutate(line_type = case_when(
    lead(line_type) == "transcript_certification" & str_detect(text, "^[0-9]{1,2}[:blank:]*$") ~ "transcript_certification", # set blank line before proceedings end as also "transcript_certification"
    page >= end_page & line >= end_line & line_type != "page_footer" ~ "transcript_certification",
    TRUE ~ line_type
  )) %>%
  select(-end_page, -end_line)




testimony <- lines %>%
  filter(
    page_type == "testimony",
    line_type %in% c("section_header", "speaker_start", "testimony", "proceedings_start", "proceedings_end", "time_marker")
  ) %>%
  select(-page_type, -page_header, -page_subheader) %>%
  separate(text, into = c("transcript_line_number", "text_clean"), sep = " ", remove = FALSE, convert = TRUE, extra = "merge") %>%
  mutate(interjection_id = case_when(
    line_type != "testimony" ~ str_glue("{str_pad(day, 2, 'left', '0')}-{str_pad(page, 3, 'left', '0')}-{str_pad(transcript_line_number, 2, 'left', '0')}"),
    TRUE ~ NA_character_
  )) %>%
  fill(interjection_id, .direction = "down") %>%
  select(everything(), text) %>%
  mutate(# put a placeholder value of "none" in the speaker vars for non-speech lines
    speaker = if_else(line_type %in% c("speaker_start", "testimony"), speaker, "none"),
    speaker_standardized = if_else(line_type %in% c("speaker_start", "testimony"), speaker_standardized, "none")
  ) %>%
  group_by(day) %>%
  fill(speaker, speaker_standardized, .direction = "down") %>% # per day, fill speaker value down; because we have placeholder values, this should only fill into testimony lines
  ungroup() %>%
  mutate(# revert the placeholder values to NA
    speaker = if_else(speaker == "none", NA_character_, speaker),
    speaker_standardized = if_else(speaker_standardized == "none", NA_character_, speaker_standardized)
  ) %>%
  mutate(text_clean = case_when(# remove the speaker intro from speaker_start lines
    line_type == "speaker_start" ~ str_remove(text_clean, str_glue("^{speaker} ?: ?")),
    TRUE ~ text_clean
  )) %>%
  mutate(# clean section header formatting (remove ---, remove :)
    text_clean = if_else(line_type == "section_header", str_remove(text_clean, "^-{2,3} ?"), text_clean),
    text_clean = if_else(line_type == "section_header", str_remove(text_clean, ":$"), text_clean)
  ) %>%
  mutate(# clean time marker formatting (remove ---)
    text_clean = if_else(line_type %in% c("proceedings_start", "proceedings_end", "time_marker"), str_remove(text_clean, "^-{2,3} ?"), text_clean)
  )

testimony %>%
  write_csv("data/out/testimony.csv")

testimony_with_combined_interjections <- testimony %>%
  group_by(interjection_id) %>%
  summarize(text_clean_combined = paste0(text_clean, collapse = " ")) %>% # combine values in `text_clean` column by interjection block (group of rows with shared interjection_id)
  left_join(
    testimony %>%
      select(line_id:line, transcript_line_number, interjection_id, line_type, section_header, speaker, speaker_standardized) %>% # drop text columns, reorganize
      group_by(interjection_id) %>%
      slice(1), # take just the first line for each interjection_id—since we're only looking for the metadata applicable to the whole interjection block, whatever's on the first line of that block should suffice
    by = "interjection_id") %>%
  filter(! is.na(interjection_id))

testimony_with_combined_interjections %>%
  write_csv("data/out/testimony-with-combined-interjections.csv")

# TODO idea: filter out is.na(text_clean) ? when it's testimony only, maybe?


# TODO idea: speaker type, counsel, lawyer, admin, witness (based on TOC entries?)


# time type: recessing, resuming, breaking, adjourning at ... 



# TODO: speaker errors, "line", "and plan b"
