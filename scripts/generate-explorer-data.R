library(jsonlite)

source("load.R")

speaker_annotations_for_web <- speaker_annotations %>%
  mutate(# slugify upfront to save processing cost
    speaker_id_web = str_to_lower(speaker_proper),
    speaker_id_web = stringi::stri_trans_general(speaker_id_web, "Latin-ASCII"),
    speaker_id_web = str_remove_all(speaker_id_web, "[\\.]"),
    speaker_id_web = str_replace_all(speaker_id_web, "[^a-z0-9]", "-")
  ) %>%
  mutate(across(everything(), .fns = ~ str_replace_all(.x, fixed("??"), "?TBC?"))) %>% # TODO: actually fix these
  select(-speaker_notes)



testimony_for_web <- testimony_with_combined_interjections %>%
  rename(
    intervention_id = interjection_id,
    text = text_clean_combined
  ) %>%
  left_join(speaker_annotations_for_web, by = "speaker_standardized")

testimony_for_web %>%
  write_csv("data/out/poec-explorer/testimony/testimony.csv", na = "")



proceedings %>%
  left_join(
    read_csv("data/indices/proceeding-annotations.csv") %>%
      mutate(proceeding_phase = replace_na(proceeding_phase, "factual")),
    by = "day") %>%
  left_join((# add details about each day's testimony (number of pages, speakers, etc), TODO: add words etc
    testimony_for_web %>%
      group_by(day) %>%
      summarize(
        pages_of_testimony = max(page) - min(page)
      ) %>%
      mutate(
        speakers = map(day, ~ (
          testimony_for_web %>%
            filter(day == .x, ! is.na(speaker_proper)) %>%
            count(speaker_standardized, sort = TRUE, name = "n_interventions") %>%
            left_join(speaker_annotations_for_web, by = "speaker_standardized")
        )),
        n_speakers = map_int(speakers, nrow),
        n_witnesses = map_int(speakers, ~ (
          .x %>%
            filter(speaker_group == "witness") %>%
            nrow()
        ))
      )
  )) %>%
  write_json("data/out/poec-explorer/testimony/proceedings.json", pretty = TRUE)



speaker_annotations_for_web %>%
  filter(! is.na(speaker_proper)) %>%
  left_join((
    testimony_for_web %>%
      group_by(speaker_standardized) %>%
      summarize(
        n_interventions = n(),
        n_speaking_days = n_distinct(day),
      )
  )) %>%
  write_csv("data/out/poec-explorer/testimony/speakers.csv", na = "")

