source("load.R")

testimony_with_combined_interjections %>%
  group_by(day) %>%
  group_walk(~ write_csv(.x, str_glue("data/out/poec-explorer/testimony/volume-{.y$day}.csv")))

