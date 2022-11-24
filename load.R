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
  extract(text, c("day", "date"), regex = "Day ([0-9]{1,2}) - ([A-Za-z]* [0-9]{1,2}, [0-9]{4})", convert = TRUE) %>%
  filter(! is.na(date)) %>%
  mutate(
    transcript_filename = glue("POEC-Public-Hearings-Volume-{day}-{date %>% str_remove_all(., ',') %>% str_replace_all(., ' ', '-')}.pdf"),
    transcript_url = glue("https://publicorderemergencycommission.ca/files/documents/Transcripts/{transcript_filename}")
  )

transcripts <- proceedings %>%
  filter(mdy(date) != today()) %>% # transcripts aren't ready day-of (understandably!)
  mutate(transcript = map(transcript_url, pdf_text))
