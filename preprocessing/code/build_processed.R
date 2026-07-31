#### BUILD PROCESSED DATA ####

df <- readr::read_csv(
  data_raw_path,
  col_types = readr::cols(.default = readr::col_character())
)

df <- df |>
  dplyr::mutate(
    participant_id = as.character(participant_id),
    rt_ms          = as.numeric(rt_ms)
  )

# Participant phase: remove entire participant if they have window exits
# If a participant left the window at ANY point after fullscreen began, their entire
# dataset is removed. This is intentional participant-level filtering, not trial-level:
# one window exit flags the entire participant as unreliable.
window_exit_subjects <- df |>
  dplyr::filter(window_status != "ok") |>
  dplyr::distinct(participant_id) |>
  dplyr::pull(participant_id)

after_window_exit <- df |>
  dplyr::filter(!participant_id %in% window_exit_subjects)

# Trial phase: filter remaining data by RT bounds
after_rt_bounds <- after_window_exit |>
  dplyr::filter(rt_ms >= rt_min_ms & rt_ms <= rt_max_ms)

df_processed <- after_rt_bounds

#### VALIDATION ####

cat("Preprocessing summary:\n")
cat("  Input rows: ", nrow(df), "\n")
cat("  After window exit removal: ", nrow(after_window_exit), " (removed ", nrow(df) - nrow(after_window_exit), ")\n")
cat("  After RT filtering: ", nrow(df_processed), " (removed ", nrow(after_window_exit) - nrow(df_processed), ")\n")
cat("  Final participants: ", dplyr::n_distinct(df_processed$participant_id), "\n")

#### WRITE OUTPUT ####

readr::write_csv(df_processed, file.path(data_processed_dir, "data_processed.csv"))
