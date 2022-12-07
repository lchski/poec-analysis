library(tidyverse)

library(pdftools)

library(lubridate)
library(rvest)

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

proceedings %>%
  write_csv("data/source/proceedings.csv")

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
  write_csv("data/source/transcripts.csv")

lines_raw %>%
  write_csv("data/source/lines-raw.csv")
