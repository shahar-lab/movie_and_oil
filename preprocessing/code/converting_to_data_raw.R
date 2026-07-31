#### CONVERT COLLECTED DATA TO RAW FORMAT ####

data <- list.files(data_collected_dir, pattern = "\\.csv$", full.names = TRUE) |>
  purrr::map_df(readr::read_csv, col_types = readr::cols(.default = readr::col_character()))

data_raw <- data |>
  dplyr::select(
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
    video_present,
    window_status
  ) |>
  dplyr::filter(screen_id == "main_trial")

data_raw <- data_raw |>
  dplyr::group_by(participant_id) |>
  dplyr::mutate(
    is_correct = dplyr::if_else(
      p_win_chosen == pmax(p_win_up, p_win_down),
      1, 0
    ),
    reward_previous_trial = dplyr::lag(coins_delta),
    stay_card = dplyr::if_else(
      chosen_card == dplyr::lead(chosen_card),
      1, 0
    ),
    stay_key = dplyr::if_else(
      chosen_side == dplyr::lead(chosen_side),
      1, 0
    ),
    can_stay = dplyr::if_else(
      is.na(dplyr::lag(chosen_card)),
      0,
      dplyr::if_else(
        card_up == dplyr::lag(chosen_card) | card_down == dplyr::lag(chosen_card),
        1, 0
      )
    ),
    can_stay_side = dplyr::if_else(
      is.na(dplyr::lag(card_up)) | is.na(dplyr::lag(card_down)),
      0,
      dplyr::if_else(
        (card_up != dplyr::lag(card_up)) | (card_down != dplyr::lag(card_down)),
        1, 0
      )
    )
  ) |>
  dplyr::ungroup()

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
