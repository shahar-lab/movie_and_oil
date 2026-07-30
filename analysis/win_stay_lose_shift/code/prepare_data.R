#### LOAD AND PREPARE DATA ####

df <- read_csv(data_path, show_col_types = FALSE) |>
  group_by(participant_id) |>
  mutate(
    # ASSUMED[no column in raw]: recreate can_stay from preprocessing logic
    can_stay = if_else(
      is.na(lag(chosen_card)),
      0,
      if_else(
        card_up == lag(chosen_card) | card_down == lag(chosen_card),
        1, 0
      )
    )
  ) |>
  ungroup()

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
  group_by(participant_id, reward_label, video_present) |>
  summarise(
    avg_stay = mean(stay_card, na.rm = TRUE),
    .groups = "drop"
  )
