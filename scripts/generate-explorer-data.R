source("load.R")

# testimony_with_combined_interjections %>%
#   group_by(day) %>%
#   group_walk(~ write_csv(.x, str_glue("data/out/poec-explorer/testimony/volume-{.y$day}.csv")))

testimony_with_combined_interjections %>%
  rename(
    intervention_id = interjection_id,
    text = text_clean_combined
  ) %>%
  left_join(speaker_annotations %>% select(-speaker_notes), by = "speaker_standardized") %>%
  write_csv("data/out/poec-explorer/testimony/testimony.csv")

proceedings %>%
  write_csv("data/out/poec-explorer/testimony/proceedings.csv")

speaker_annotations %>%
  select(-speaker_notes) %>%
  filter(! is.na(speaker_proper)) %>%
  write_csv("data/out/poec-explorer/testimony/speakers.csv")
