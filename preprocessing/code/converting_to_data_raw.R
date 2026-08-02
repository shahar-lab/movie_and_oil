#### CONVERT COLLECTED DATA TO RAW FORMAT ####

data <- list.files(data_collected_dir, pattern = "\\.csv$", full.names = TRUE) |>
  purrr::map_df(readr::read_csv, col_types = readr::cols(.default = readr::col_character()))

data_raw <- data |>
  select(
    #participant
    prolific_pid,
    participant_id,

    #block variables
    block,
    trial,
    trial_in_block,
    video_present,

    #trial variables
    offer_up = card_up,
    offer_down = card_down,
    choice_card = chosen_card,
    choice_key = chosen_side,
    rt = rt_ms,
    reward = outcome,

    #enviorment variables
    p_win_chosen,
    p_win_up,
    p_win_down,
    coins_delta,
    coins_total,

    #validation variables
    screen_id,
    window_status
  ) |>
  filter(screen_id == "main_trial")

data_raw <- data_raw |>
  group_by(participant_id) |>
  mutate(
    is_correct = if_else(p_win_chosen == pmax(p_win_up, p_win_down), 1, 0),
    reward_oneback = lag(reward),
    stay_card = if_else(choice_card == lag(choice_card), 1, 0),
    stay_key = if_else(choice_key == lag(choice_key), 1, 0)
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

readr::write_csv(data_raw, file.path(data_raw_dir, "data_raw.csv"))
