#### EXTRACT FINAL FEEDBACK ####

data_collected <- list.files(data_collected_dir, pattern = "\\.csv$", full.names = TRUE) |>
  purrr::map_df(readr::read_csv, col_types = readr::cols(.default = readr::col_character()))

feedback <- data_collected |>
  filter(screen_id == "final_feedback_question") |>
  select(participant_id, prolific_pid, final_feedback) |>
  distinct(participant_id, .keep_all = TRUE)

data_processed <- readr::read_csv(
  file.path(data_processed_dir, "data_processed.csv"),
  col_types = readr::cols(.default = readr::col_character())
)

surviving_participants <- data_processed |>
  distinct(participant_id) |>
  pull(participant_id)

participant_feedback <- feedback |>
  mutate(
    survived_exclusion = if_else(participant_id %in% surviving_participants, "yes", "no")
  ) |>
  select(participant_id, prolific_pid, final_feedback, survived_exclusion)

#### WRITE OUTPUT ####

readr::write_csv(participant_feedback, file.path(output_dir, "participant_feedback.csv"))
