source("load.R")

# testimony_with_combined_interjections %>%
#   group_by(day) %>%
#   group_walk(~ write_csv(.x, str_glue("data/out/poec-explorer/testimony/volume-{.y$day}.csv")))

speaker_annotations_for_web <- speaker_annotations %>%
  mutate(# slugify upfront to save processing cost
    speaker_id_web = str_to_lower(speaker_proper),
    speaker_id_web = stringi::stri_trans_general(speaker_id_web, "Latin-ASCII"),
    speaker_id_web = str_remove_all(speaker_id_web, "[\\.]"),
    speaker_id_web = str_replace_all(speaker_id_web, "[^a-z0-9]", "-")
  ) %>%
  select(-speaker_notes)

testimony_with_combined_interjections %>%
  rename(
    intervention_id = interjection_id,
    text = text_clean_combined
  ) %>%
  left_join(speaker_annotations_for_web, by = "speaker_standardized") %>%
  write_csv("data/out/poec-explorer/testimony/testimony.csv", na = "")

proceedings %>%
  write_csv("data/out/poec-explorer/testimony/proceedings.csv", na = "")

speaker_annotations_for_web %>%
  filter(! is.na(speaker_proper)) %>%
  write_csv("data/out/poec-explorer/testimony/speakers.csv", na = "")
