#### BUILD PROCESSED DATA ####

df_to_process <- read_csv(
  data_raw_path,
  col_types = cols(.default = col_character())
)

df_to_process <- df_to_process |>
  mutate(
    participant_id = as.character(participant_id),
    rt             = as.numeric(rt)
  )

# Participant phase, step 1: remove entire participant if they exceed the window-exit threshold
# A participant who left the window more than window_exit_max times is flagged as unreliable
# for their whole dataset, not just the offending trials.
window_exit_subjects <- df_to_process |>
  group_by(participant_id) |>
  summarise(n_exits = sum(window_status != "ok")) |>
  filter(n_exits > window_exit_max) |>
  pull(participant_id)

after_window_exit <- df_to_process |>
  filter(!participant_id %in% window_exit_subjects)

# Participant phase, step 2: remove participants whose RT-out-of-bounds percentage is too high
# Computed on the survivors of step 1, so this criterion is attributable to RT quality alone.
rt_trim_subjects <- after_window_exit |>
  group_by(participant_id) |>
  summarise(pct = 100 * mean(rt < rt_min | rt > rt_max, na.rm = TRUE)) |>
  filter(pct > rt_trim_pct_max) |>
  pull(participant_id)

after_rt_trim_pct <- after_window_exit |>
  filter(!participant_id %in% rt_trim_subjects)

# Trial phase: drop individual bad-RT trials for the participants who survived both checks above
after_rt_bounds <- after_rt_trim_pct |>
  filter(rt >= rt_min & rt <= rt_max)

df_processed <- after_rt_bounds

#### VALIDATION ####

cat("Preprocessing summary:\n")
cat("  Input rows: ", nrow(df_to_process), "\n")
cat("  Participants removed for window exits: ", length(window_exit_subjects), "\n")
cat("  After window exit removal: ", nrow(after_window_exit), " (removed ", nrow(df_to_process) - nrow(after_window_exit), ")\n")
cat("  Participants removed for RT-trim-%: ", length(rt_trim_subjects), "\n")
cat("  After RT-trim-% removal: ", nrow(after_rt_trim_pct), " (removed ", nrow(after_window_exit) - nrow(after_rt_trim_pct), ")\n")
cat("  After RT filtering: ", nrow(df_processed), " (removed ", nrow(after_rt_trim_pct) - nrow(df_processed), ")\n")
cat("  Final participants: ", n_distinct(df_processed$participant_id), "\n")

#### WRITE OUTPUT ####

write_csv(df_processed, file.path(data_processed_dir, "data_processed.csv"))
