#### LOAD AND PREPARE DATA ####

df <- read_csv(data_path, show_col_types = FALSE)

#### FILTER DATA ####

df_analysis <- df |>
  filter(can_stay == 1) |>
  filter(!is.na(reward_previous_trial)) |>
  mutate(
    reward_binary = if_else(reward_previous_trial > 0, 1, 0),
    reward_label = if_else(reward_binary == 1, "Reward", "No Reward")
  )

#### AGGREGATE TO SUBJECT LEVEL ####

df_agg <- df_analysis |>
  mutate(participant_id = as.character(participant_id)) |>
  group_by(participant_id, reward_label, video_present) |>
  summarise(
    avg_stay = mean(stay_card, na.rm = TRUE),
    .groups = "drop"
  )
