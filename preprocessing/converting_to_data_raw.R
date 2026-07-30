rm(list = ls())
library(tidyverse)
install.packages("here")
library(here)

project_root <- here::here()

data <- list.files(file.path(project_root, "data", "collected"), pattern = "\\.csv$", full.names = TRUE) |>
  map_df(read_csv, col_types = cols(.default = col_character()))

data_raw <- data |>
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
  ) |>
  filter(screen_id == "main_trial")

data_raw <- data_raw |>
  group_by(participant_id) |>
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
    ),
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

#### VALIDATION ####

cat("Row counts:\n")
cat("  Input rows: ", nrow(data), "\n")
cat("  Output rows: ", nrow(data_raw), "\n")

cat("\nMissing values per column:\n")
print(colSums(is.na(data_raw)))

cat("\nVariable types:\n")
str(data_raw)

#### WRITE OUTPUT ####

write_csv(data_raw, file.path(project_root, "data", "raw", "data_raw.csv"))
