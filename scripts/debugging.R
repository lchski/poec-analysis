# ==== Verifying proceedings_end is always on last page ====
end_lines <- lines %>%
  filter(line_type == "proceedings_end") %>%
  select(day, end_page = page, end_line = line)

end_lines %>%
  left_join(
    lines %>%
      group_by(day) %>%
      summarize(max_page = max(page))
  ) %>%
  mutate(end_is_max = end_page == max_page) %>%
  count(end_is_max)

rm(end_lines)



# ==== Confirm there are no duplicate names (typos, title inconsistencies etc) ====
lines %>%
  count(speaker_standardized) %>%
  View("speaker_standardized_count")



# ==== Find non-speech lines followed by non-speaker_start lines ====
# After a heading, or time marker, etc, the speaker should always be identified
unexpected_line_type_following_non_speech <- testimony %>%
  group_by(day) %>%
  mutate(next_line_type = lead(line_type), next_text = lead(text), next_text_clean = lead(text_clean), next_line_id = lead(line_id)) %>%
  filter(! line_type %in% c("speaker_start", "testimony") & lead(line_type) != "speaker_start") %>%
  select(-transcript_line_number, -text_clean, -speaker, -speaker_standardized)

unexpected_line_type_following_non_speech %>%
  filter(next_line_type == "testimony", ! is.na(next_text_clean)) %>%
  write_csv("data/out/unexpected-testimony-line-type.csv")

# When non-speech is followed by non-speech - all good.
# When non-speech is followed immediately by testimony - likely problem. (Usually, multi-line section_header. Sometimes, speaker ID is missing from original transcript.)
