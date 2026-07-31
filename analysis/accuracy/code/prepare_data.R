#### LOAD AND PREPARE DATA ####

df <- read_csv(data_path, show_col_types = FALSE)

#### CALCULATE ACCURACY ####

accuracy_per_participant <- df |>
  group_by(participant_id) |>
  summarise(
    n_trials = n(),
    accuracy = mean(is_correct, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(participant_id)

overall_accuracy <- tibble(
  participant_id = "Overall",
  n_trials = nrow(df),
  accuracy = mean(df$is_correct, na.rm = TRUE)
)

accuracy_summary <- bind_rows(accuracy_per_participant, overall_accuracy)

#### SAVE TO ARTIFACTS ####

saveRDS(accuracy_per_participant, file.path(artifacts_dir, "accuracy_per_participant.rds"))
saveRDS(overall_accuracy, file.path(artifacts_dir, "overall_accuracy.rds"))
saveRDS(accuracy_summary, file.path(artifacts_dir, "accuracy_summary.rds"))
