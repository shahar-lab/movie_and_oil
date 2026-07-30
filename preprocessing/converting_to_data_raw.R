rm(list = ls())
library(tidyverse)

data <- list.files("data/collected", pattern = "\\.csv$", full.names = TRUE) |>
  map_df(read_csv, col_types = cols(.default = col_character()))

data_raw <- data %>%
  select(
    prolific_pid,
    participant_id,
    screen_id,
    card_up,
    card_down,
    chosen_card,
    chosen_side,
    rt_ms,
    outcome,
    p_win_chosen,
    p_win_up,
    p_win_down,
    coins_delta,
    coins_total,
    block,
    trial,
    trial_in_block,
    video_present
  ) %>%
  filter(screen_id == "main_trial")

data_raw <- data_raw %>%
  group_by(participant_id) %>%
  mutate(
    is_correct = if_else(
      p_win_chosen == pmax(p_win_up, p_win_down),
      1, 0
    ),
    reward_previous_trial = lag(coins_delta),
    stay_card = if_else(
      chosen_card == lead(chosen_card),
      1, 0
    ),
    stay_key = if_else(
      chosen_side == lead(chosen_side),
      1, 0
    )
  ) %>%
  ungroup()

write_csv(data_raw, "data/data_raw.csv")
