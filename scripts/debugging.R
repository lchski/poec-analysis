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



# ==== Check for possibly missed speaker names (speaker_start) ====
# We expect there'll be at least *some* responsive lines here, but a direct read
# (or in context, e.g., line `28-197-09`) should indicate whether it’s actually a
# missed speaker_start or not.
lines %>%
  filter(line_type == "testimony") %>%
  filter(str_detect(text, "[A-Z]{3,} ?:")) %>%
  write_csv("data/out/possible-speaker-lines.csv") # as of 2022-12-03 (all public hearings loaded), 12 lines, all of them expected



# ==== Identify speakers we haven't annotated yet ====
lines %>%
  count(speaker_standardized) %>%
  left_join(speaker_annotations) %>%
  filter(is.na(speaker_proper)) %>%
  select(speaker_standardized) %>%
  write_csv("data/out/unannotated-speakers.csv")



# ==== Confirm there are no duplicate names (typos, title inconsistencies etc) ====
lines %>%
  count(speaker_standardized) %>%
  View("speaker_standardized_count")



# ==== Find possible misattributions (witness on multiple days with low counts) ====
witnesses_with_multiple_days_of_testimony <- testimony_with_combined_interjections %>%
  count(speaker_standardized, day) %>%
  count(speaker_standardized) %>%
  filter(n > 1) %>%
  left_join(speaker_annotations %>% select(speaker_standardized, speaker_group)) %>%
  filter(speaker_group == "witness") %>%
  pull(speaker_standardized)

testimony_with_combined_interjections %>%
  count(speaker_standardized, day) %>%
  filter(speaker_standardized %in% witnesses_with_multiple_days_of_testimony)

rm(witnesses_with_multiple_days_of_testimony)



# ==== Find possible misattributions (same speaker twice in a row in combined testimony) ====
testimony_with_combined_interjections %>%
  mutate(
    next_speaker_standardized = lead(speaker_standardized),
    next_line_id = lead(line_id)
  ) %>%
  filter(speaker_standardized == next_speaker_standardized) %>%
  select(interjection_id, line_type, speaker_standardized, next_speaker_standardized, text_clean_combined, line_id, next_line_id) %>%
  write_csv("data/out/possible-speaker-misattributions-double-speaker.csv")




# ==== Find non-speech lines followed by non-speaker_start lines ====
# After a heading, or time marker, etc, the speaker should always be identified
unexpected_line_type_following_non_speech <- testimony %>%
  group_by(day) %>%
  mutate(next_line_type = lead(line_type), next_text = lead(text), next_text_clean = lead(text_clean), next_line_id = lead(line_id)) %>%
  filter(! line_type %in% c("speaker_start", "testimony") & lead(line_type) != "speaker_start") %>%
  select(-transcript_line_number, -text_clean, -speaker, -speaker_standardized)

unexpected_line_type_following_non_speech %>%
  filter(next_line_type == "testimony", ! is.na(next_text_clean)) %>%
  write_csv("data/out/unexpected-testimony-line-type.csv") # expect 0 entries if all good

# When non-speech is followed by non-speech - all good.
# When non-speech is followed immediately by testimony - likely problem. (Usually, multi-line section_header. Sometimes, speaker ID is missing from original transcript.)
