#### LOAD AND PREPARE DATA ####

df <- read_csv(data_path, show_col_types = FALSE)

#### FILTER DATA ####

df_analysis <- df |>
  group_by(participant_id) |>
  filter(
    !is.na(lag(offer_up)),
    !(lag(offer_up) %in% c(offer_up, offer_down)) & !(lag(offer_down) %in% c(offer_up, offer_down))
  ) |>
  ungroup() |>
  filter(!is.na(reward_oneback)) |>
  mutate(reward_oneback = factor(reward_oneback, levels = c("loss", "win")))

#### AGGREGATE TO SUBJECT LEVEL ####

df_agg <- df_analysis |>
  mutate(participant_id = as.character(participant_id)) |>
  group_by(participant_id, reward_oneback, video_present) |>
  summarise(
    avg_stay_key = mean(stay_key, na.rm = TRUE),
    .groups = "drop"
  )

#### CREATE COLORBLIND-SAFE PALETTE ####

unique_participants <- sort(unique(df_agg$participant_id))
palette_8 <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A",
               "#66A61E", "#E6AB02", "#A6761D", "#F0027F")
participant_palette <- setNames(
  palette_8[seq_along(unique_participants)],
  unique_participants
)
