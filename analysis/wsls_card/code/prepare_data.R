#### LOAD AND PREPARE DATA ####

df <- read_csv(data_path, show_col_types = FALSE)
cat("LOADED FROM:", data_path, "\n")
cat("Total rows loaded:", nrow(df), "| Unique participants:", n_distinct(df$participant_id), "\n")
cat("Participant IDs:", paste(sort(unique(df$participant_id)), collapse = ", "), "\n")

#### FILTER DATA ####

# Step-by-step filtering to see where data is lost
df_step1 <- df |>
  group_by(participant_id) |>
  filter(offer_up == lag(choice_card) | offer_down == lag(choice_card))
cat("After offer-matches-previous-choice filter:", nrow(df_step1), "rows\n")

df_step2 <- df_step1 |>
  ungroup() |>
  filter(!is.na(reward_oneback)) |>
  mutate(reward_oneback = factor(reward_oneback, levels = c("loss", "win")))

df_analysis <- df_step2

cat("After filtering reward_oneback NA:", nrow(df_analysis), "| Unique participants:", n_distinct(df_analysis$participant_id), "\n")

#### AGGREGATE TO SUBJECT LEVEL ####

df_agg <- df_analysis |>
  mutate(participant_id = as.character(participant_id)) |>
  group_by(participant_id, reward_oneback, video_present) |>
  summarise(
    avg_stay = mean(stay_card, na.rm = TRUE),
    .groups = "drop"
  )

cat("Aggregated data points:", nrow(df_agg), "\n")
cat("Unique participants in agg data:", n_distinct(df_agg$participant_id), "\n")
cat("\nParticipants appearing in each condition:\n")
cat("With video:", paste(sort(unique(df_agg$participant_id[df_agg$video_present == TRUE])), collapse = ", "), "\n")
cat("Without video:", paste(sort(unique(df_agg$participant_id[df_agg$video_present == FALSE])), collapse = ", "), "\n")
cat("\nData structure:\n")
print(table(df_agg$video_present, df_agg$reward_oneback))

#### CREATE COLORBLIND-SAFE PALETTE ####

unique_participants <- sort(unique(df_agg$participant_id))
palette_base <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A",
                  "#66A61E", "#E6AB02", "#A6761D", "#F0027F")
n_participants <- length(unique_participants)
palette_extended <- if (n_participants <= length(palette_base)) {
  palette_base[seq_len(n_participants)]
} else {
  c(palette_base, grDevices::hcl.colors(n_participants - length(palette_base), "viridis"))
}
participant_palette <- setNames(palette_extended, unique_participants)
