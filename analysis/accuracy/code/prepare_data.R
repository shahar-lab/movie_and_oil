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

#### CALCULATE LEARNING CURVE (PER TRIAL_IN_BLOCK, PER CONDITION) ####

# CI computed as t-distribution based 90% CI over per-participant means, per trial_bin/condition
# trial_bin groups trial_in_block into bins of 10 trials (1, 11, 21, 31, 41), labelled by bin start
learning_curve_trials <- df |>
  mutate(condition = if_else(video_present == TRUE, "With Video", "Without Video")) |>
  select(condition, participant_id, trial_in_block, is_correct) |>
  mutate(trial_bin = floor((trial_in_block - 1) / 10) * 10 + 1)

learning_curve_participant <- learning_curve_trials |>
  group_by(condition, participant_id, trial_bin) |>
  summarise(participant_accuracy = mean(is_correct, na.rm = TRUE), .groups = "drop")

learning_curve_summary <- learning_curve_participant |>
  group_by(condition, trial_bin) |>
  summarise(
    n_participants = n(),
    mean_accuracy  = mean(participant_accuracy, na.rm = TRUE),
    se_accuracy    = sd(participant_accuracy, na.rm = TRUE) / sqrt(n_participants),
    ci_mult        = qt(0.95, df = pmax(n_participants - 1, 1)),
    ci_lower       = mean_accuracy - ci_mult * se_accuracy,
    ci_upper       = mean_accuracy + ci_mult * se_accuracy,
    .groups = "drop"
  ) |>
  select(-ci_mult)

#### CALCULATE LEARNING CURVE FOR HIGH-ACCURACY SUBSET (ACCURACY >= 0.55) ####

high_accuracy_participants <- accuracy_per_participant |>
  filter(accuracy >= 0.55) |>
  pull(participant_id)

learning_curve_trials_filtered <- learning_curve_trials |>
  filter(participant_id %in% high_accuracy_participants)

learning_curve_participant_filtered <- learning_curve_trials_filtered |>
  group_by(condition, participant_id, trial_bin) |>
  summarise(participant_accuracy = mean(is_correct, na.rm = TRUE), .groups = "drop")

learning_curve_summary_filtered <- learning_curve_participant_filtered |>
  group_by(condition, trial_bin) |>
  summarise(
    n_participants = n(),
    mean_accuracy  = mean(participant_accuracy, na.rm = TRUE),
    se_accuracy    = sd(participant_accuracy, na.rm = TRUE) / sqrt(n_participants),
    ci_mult        = qt(0.95, df = pmax(n_participants - 1, 1)),
    ci_lower       = mean_accuracy - ci_mult * se_accuracy,
    ci_upper       = mean_accuracy + ci_mult * se_accuracy,
    .groups = "drop"
  ) |>
  select(-ci_mult)

#### SAVE TO ARTIFACTS ####

saveRDS(accuracy_per_participant, file.path(artifacts_dir, "accuracy_per_participant.rds"))
saveRDS(overall_accuracy, file.path(artifacts_dir, "overall_accuracy.rds"))
saveRDS(accuracy_summary, file.path(artifacts_dir, "accuracy_summary.rds"))
saveRDS(learning_curve_summary, file.path(artifacts_dir, "learning_curve_summary.rds"))
saveRDS(learning_curve_trials, file.path(artifacts_dir, "learning_curve_trials.rds"))
saveRDS(learning_curve_summary_filtered, file.path(artifacts_dir, "learning_curve_summary_filtered.rds"))
saveRDS(learning_curve_trials_filtered, file.path(artifacts_dir, "learning_curve_trials_filtered.rds"))
