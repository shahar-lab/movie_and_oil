#### EXCLUSION SUMMARY: DATA AND CRITERIA ####

# Load both datasets under report-local names, so this script never overwrites the objects
# converting_to_data_processed.R left in the global environment
df_raw_report <- read_csv(
  data_raw_path,
  col_types = cols(.default = col_character())
) |>
  mutate(
    participant_id = as.character(participant_id),
    rt             = as.numeric(rt),
    video_present  = as.logical(video_present)
  )

df_processed_report <- read_csv(
  file.path(data_processed_dir, "data_processed.csv"),
  col_types = cols(.default = col_character())
) |>
  mutate(
    participant_id = as.character(participant_id),
    rt             = as.numeric(rt),
    video_present  = as.logical(video_present)
  )

# Recompute the two participant-level exclusion criteria directly from df_raw_report, so this
# report is self-contained and derived from data rather than copy-pasted counts.
# Named *_table (not *_subjects) to avoid shadowing the character-vector objects that
# converting_to_data_processed.R leaves in the global env for manuscript_paragraph.R.
window_exit_table <- df_raw_report |>
  group_by(participant_id) |>
  summarise(n_exits = sum(window_status != "ok")) |>
  filter(n_exits > window_exit_max)

after_window_exit_report <- df_raw_report |>
  filter(!participant_id %in% window_exit_table$participant_id)

# Compare the unrounded pct to rt_trim_pct_max (matches converting_to_data_processed.R);
# round only for display, after filtering.
rt_trim_table <- after_window_exit_report |>
  group_by(participant_id) |>
  summarise(pct_outside_bounds = 100 * mean(rt < rt_min | rt > rt_max, na.rm = TRUE)) |>
  filter(pct_outside_bounds > rt_trim_pct_max) |>
  mutate(pct_outside_bounds = round(pct_outside_bounds, 2))

both_criteria_survivors <- setdiff(
  unique(df_raw_report$participant_id),
  c(window_exit_table$participant_id, rt_trim_table$participant_id)
)

df_raw_report <- df_raw_report |>
  mutate(row_id = row_number())

df_processed_report <- df_processed_report |>
  mutate(row_id = row_number())

# Rows present in raw but not in processed, restricted to participants who survived both
# participant-level exclusions: the trial-level RT filter only applies to them, and the
# participants excluded above are accounted for by the two criterion tables instead
removed_rows <- df_raw_report |>
  filter(participant_id %in% both_criteria_survivors) |>
  anti_join(
    df_processed_report,
    by = c("prolific_pid", "participant_id", "block", "trial", "rt")
  )

#### EXCLUSION SUMMARY: PER-PARTICIPANT TABLE ####

exclusion_summary <- removed_rows |>
  group_by(participant_id) |>
  summarise(
    n_removed = n(),
    n_removed_with_video = sum(video_present == TRUE, na.rm = TRUE),
    .groups = "drop"
  ) |>
  full_join(
    df_raw_report |>
      filter(participant_id %in% both_criteria_survivors) |>
      group_by(participant_id) |>
      summarise(
        n_trials_raw = n(),
        .groups = "drop"
      ),
    by = "participant_id"
  ) |>
  mutate(
    n_removed = replace_na(n_removed, 0),
    n_removed_with_video = replace_na(n_removed_with_video, 0)
  ) |>
  mutate(
    pct_removed = ifelse(
      n_trials_raw > 0,
      round(100 * n_removed / n_trials_raw, 2),
      0
    ),
    pct_removed_with_video = ifelse(
      n_removed > 0,
      round(100 * n_removed_with_video / n_removed, 2),
      0
    )
  ) |>
  select(
    participant_id,
    n_trials_raw,
    n_removed,
    pct_removed,
    n_removed_with_video,
    pct_removed_with_video
  ) |>
  arrange(participant_id)

total_raw           <- sum(exclusion_summary$n_trials_raw)
total_removed       <- sum(exclusion_summary$n_removed)
total_pct_removed   <- round(100 * total_removed / total_raw, 2)
total_removed_video <- sum(exclusion_summary$n_removed_with_video)
total_pct_video     <- round(100 * total_removed_video / total_removed, 2)

overall_summary <- tibble(
  participant_id         = "OVERALL",
  n_trials_raw           = total_raw,
  n_removed              = total_removed,
  pct_removed            = total_pct_removed,
  n_removed_with_video   = total_removed_video,
  pct_removed_with_video = total_pct_video
)

exclusion_table <- bind_rows(
  exclusion_summary,
  overall_summary
)

#### EXCLUSION SUMMARY: TABLE GROBS ####

table_for_pdf <- exclusion_table |>
  mutate(
    Participant_ID = participant_id,
    Trials_Raw = n_trials_raw,
    Trials_Removed = n_removed,
    Pct_Removed = as.character(pct_removed),
    Removed_With_Video = n_removed_with_video,
    Pct_Video = as.character(pct_removed_with_video)
  ) |>
  select(
    Participant_ID,
    Trials_Raw,
    Trials_Removed,
    Pct_Removed,
    Removed_With_Video,
    Pct_Video
  )

# Table 1: participants excluded for window exits, with their exit count
table_window_exit <- window_exit_table |>
  rename(Participant_ID = participant_id, N_Window_Exits = n_exits)

# Table 2: participants excluded for RT-trim-%, with their trimmed percentage
table_rt_trim <- rt_trim_table |>
  rename(Participant_ID = participant_id, Pct_Outside_RT_Bounds = pct_outside_bounds)

# Shared theme for all table grobs below
table_theme <- ttheme_default(
  core = list(
    fg_params = list(hjust = 0.5, x = 0.5, fontsize = 10, fontface = "plain"),
    bg_params = list(fill = "white")
  ),
  colhead = list(
    fg_params = list(hjust = 0.5, x = 0.5, fontsize = 11, fontface = "bold"),
    bg_params = list(fill = "#E8E8E8")
  ),
  padding = unit(c(8, 6), "mm")
)

table_window_exit_grob <- tableGrob(table_window_exit, rows = NULL, theme = table_theme)
table_rt_trim_grob     <- tableGrob(table_rt_trim, rows = NULL, theme = table_theme)
table_summary_grob     <- tableGrob(table_for_pdf, rows = NULL, theme = table_theme)

title_window_exit <- textGrob(
  paste0("PARTICIPANTS EXCLUDED: WINDOW EXITS (> ", window_exit_max, ")"),
  gp = gpar(fontsize = 13, fontface = "bold")
)

title_rt_trim <- textGrob(
  paste0("PARTICIPANTS EXCLUDED: RT-TRIM-% (> ", rt_trim_pct_max, "%)"),
  gp = gpar(fontsize = 13, fontface = "bold")
)

title_summary <- textGrob(
  "COMPLETE EXCLUSION SUMMARY (SURVIVORS OF BOTH PARTICIPANT-LEVEL CRITERIA)",
  gp = gpar(fontsize = 13, fontface = "bold")
)

footer <- textGrob(
  paste("Data source:", data_raw_path, "\nGenerated:", Sys.time()),
  gp = gpar(fontsize = 10, fontface = "italic")
)

spacer_small  <- textGrob("", gp = gpar(fontsize = 1))
spacer_medium <- textGrob("", gp = gpar(fontsize = 3))

#### EXCLUSION SUMMARY: EXPORT ####

png_file <- file.path(output_dir, "exclusion_summary.png")

png(
  filename = png_file,
  width = 12,
  height = 10,
  units = "in",
  res = 300,
  bg = "white"
)

grid.arrange(
  title_window_exit,
  table_window_exit_grob,
  spacer_small,
  title_rt_trim,
  table_rt_trim_grob,
  spacer_small,
  title_summary,
  table_summary_grob,
  spacer_medium,
  footer,
  nrow = 10,
  heights = unit(c(0.015, 0.1, 0.005, 0.015, 0.1, 0.005, 0.02, 0.55, 0.01, 0.09), "npc")
)

dev.off()

cat("Exclusion summary report generated: ", png_file, "\n")
