#### LOAD AND PREPARE DATA ####

df <- read_csv(data_path, show_col_types = FALSE)
cat("LOADED FROM:", data_path, "\n")
cat("Total rows loaded:", nrow(df), "| Unique participants:", n_distinct(df$participant_id), "\n")
cat("Participant IDs:", paste(sort(unique(df$participant_id)), collapse = ", "), "\n")

#### FILTER DATA ####

# Step 1: remove trials without a previous trial
df_step1 <- df |>
  arrange(participant_id, trial) |> 
  group_by(participant_id) |>
  filter(
    !is.na(lag(offer_up)),
    !is.na(lag(offer_down))
  ) |>
  ungroup()

cat("After removing first trials:", nrow(df_step1), "rows\n")


# Step 2: keep trials in which neither previous offer
# appears in the current trial
df_step2 <- df |>
  arrange(participant_id, trial) |>
  group_by(participant_id) |>
  filter(
    !is.na(lag(offer_up)),
    !is.na(lag(offer_down)),
    lag(offer_up) != offer_up,
    lag(offer_up) != offer_down,
    lag(offer_down) != offer_up,
    lag(offer_down) != offer_down
  ) |>
  ungroup()

cat("After offer-change filter:", nrow(df_step2), "rows\n")


# Step 3: prepare the final analysis data
df_analysis <- df_step2 |>
  filter(!is.na(reward_oneback)) |>
  mutate(
    reward_oneback = factor(
      reward_oneback,
      levels = c("loss", "win")
    )
  )

cat("Final analysis data:", nrow(df_analysis), "rows\n")
#### AGGREGATE TO SUBJECT LEVEL ####

df_agg <- df_analysis |>
  mutate(participant_id = as.character(participant_id)) |>
  group_by(participant_id, reward_oneback, video_present) |>
  summarise(
    avg_stay_key = mean(stay_key, na.rm = TRUE),
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
