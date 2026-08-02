#### LOAD AND PREPARE DATA ####

df <- read_csv(data_path, show_col_types = FALSE)

#### FILTER DATA ####

df_analysis <- df |>
  group_by(participant_id) |>
  filter(offer_up == lag(choice_card) | offer_down == lag(choice_card)) |>
  ungroup() |>
  filter(!is.na(reward_oneback)) |>
  mutate(reward_oneback = factor(reward_oneback, levels = c("loss", "win")))

#### AGGREGATE TO SUBJECT LEVEL ####

df_agg <- df_analysis |>
  mutate(participant_id = as.character(participant_id)) |>
  group_by(participant_id, reward_oneback, video_present) |>
  summarise(
    avg_stay = mean(stay_card, na.rm = TRUE),
    .groups = "drop"
  )

#### CREATE COLORBLIND-SAFE 8-COLOR PALETTE ####

unique_participants <- sort(unique(df_agg$participant_id))
palette_8 <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A",
               "#66A61E", "#E6AB02", "#A6761D", "#F0027F")
participant_palette <- setNames(
  palette_8[seq_along(unique_participants)],
  unique_participants
)
