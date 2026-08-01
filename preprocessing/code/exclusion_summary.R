#### EXCLUSION SUMMARY REPORT ####

output_dir <- file.path(project_root, "preprocessing", "output")
dir.create(output_dir, showWarnings = FALSE)

# Load both datasets
df_raw <- readr::read_csv(
  data_raw_path,
  col_types = readr::cols(.default = readr::col_character())
) |>
  dplyr::mutate(
    participant_id = as.character(participant_id),
    rt_ms          = as.numeric(rt_ms),
    video_present  = as.logical(video_present)
  )

df_processed <- readr::read_csv(
  file.path(data_processed_dir, "data_processed.csv"),
  col_types = readr::cols(.default = readr::col_character())
) |>
  dplyr::mutate(
    participant_id = as.character(participant_id),
    rt_ms          = as.numeric(rt_ms),
    video_present  = as.logical(video_present)
  )

# Identify removed rows by row number or unique identifiers
# Create a marker column to identify which rows are in processed data
df_raw <- df_raw |>
  dplyr::mutate(
    row_id = dplyr::row_number()
  )

df_processed <- df_processed |>
  dplyr::mutate(
    row_id = dplyr::row_number()
  )

# Find rows in raw but not in processed by participant and key variables
removed_rows <- df_raw |>
  dplyr::anti_join(
    df_processed,
    by = c("prolific_pid", "participant_id", "block", "trial", "rt_ms")
  )

# Calculate per-participant exclusion statistics
exclusion_summary <- removed_rows |>
  dplyr::group_by(participant_id) |>
  dplyr::summarise(
    n_removed = dplyr::n(),
    n_removed_with_video = sum(video_present == TRUE, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::full_join(
    df_raw |>
      dplyr::group_by(participant_id) |>
      dplyr::summarise(
        n_trials_raw = dplyr::n(),
        .groups = "drop"
      ),
    by = "participant_id"
  ) |>
  dplyr::mutate(
    n_removed = tidyr::replace_na(n_removed, 0),
    n_removed_with_video = tidyr::replace_na(n_removed_with_video, 0)
  ) |>
  dplyr::mutate(
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
  dplyr::select(
    participant_id,
    n_trials_raw,
    n_removed,
    pct_removed,
    n_removed_with_video,
    pct_removed_with_video
  ) |>
  dplyr::arrange(participant_id)

# Calculate overall summary row
total_raw             <- sum(exclusion_summary$n_trials_raw)
total_removed         <- sum(exclusion_summary$n_removed)
total_pct_removed     <- round(100 * total_removed / total_raw, 2)
total_removed_video   <- sum(exclusion_summary$n_removed_with_video)
total_pct_video       <- round(100 * total_removed_video / total_removed, 2)

overall_summary <- tibble::tibble(
  participant_id        = "OVERALL",
  n_trials_raw          = total_raw,
  n_removed             = total_removed,
  pct_removed           = total_pct_removed,
  n_removed_with_video  = total_removed_video,
  pct_removed_with_video = total_pct_video
)

# Combine participant-level and overall summary
exclusion_table <- dplyr::bind_rows(
  exclusion_summary,
  overall_summary
)

# Prepare table for PDF export with better column names
table_for_pdf <- exclusion_table |>
  dplyr::mutate(
    Participant_ID = participant_id,
    Trials_Raw = n_trials_raw,
    Trials_Removed = n_removed,
    Pct_Removed = as.character(pct_removed),
    Removed_With_Video = n_removed_with_video,
    Pct_Video = as.character(pct_removed_with_video)
  ) |>
  dplyr::select(
    Participant_ID,
    Trials_Raw,
    Trials_Removed,
    Pct_Removed,
    Removed_With_Video,
    Pct_Video
  )

# Identify entirely excluded participants
entirely_excluded <- exclusion_summary |>
  dplyr::filter(pct_removed == 100) |>
  dplyr::mutate(
    Participant_ID = as.character(participant_id),
    Trials_Raw = n_trials_raw,
    Trials_Removed = n_removed
  ) |>
  dplyr::select(Participant_ID, Trials_Raw, Trials_Removed)

# Create table grob for entirely excluded participants
table_excluded_grob <- gridExtra::tableGrob(
  entirely_excluded,
  rows = NULL,
  theme = gridExtra::ttheme_default(
    core = list(
      fg_params = list(hjust = 0.5, x = 0.5, fontsize = 10, fontface = "plain"),
      bg_params = list(fill = "white")
    ),
    colhead = list(
      fg_params = list(hjust = 0.5, x = 0.5, fontsize = 11, fontface = "bold"),
      bg_params = list(fill = "#E8E8E8")
    ),
    padding = grid::unit(c(8, 6), "mm")
  )
)

# Create table grob for complete exclusion summary
table_summary_grob <- gridExtra::tableGrob(
  table_for_pdf,
  rows = NULL,
  theme = gridExtra::ttheme_default(
    core = list(
      fg_params = list(hjust = 0.5, x = 0.5, fontsize = 10, fontface = "plain"),
      bg_params = list(fill = "white")
    ),
    colhead = list(
      fg_params = list(hjust = 0.5, x = 0.5, fontsize = 11, fontface = "bold"),
      bg_params = list(fill = "#E8E8E8")
    ),
    padding = grid::unit(c(8, 6), "mm")
  )
)

# Create title text grobs
title_excluded <- grid::textGrob(
  "ENTIRELY EXCLUDED PARTICIPANTS",
  gp = grid::gpar(fontsize = 13, fontface = "bold")
)

title_summary <- grid::textGrob(
  "COMPLETE EXCLUSION SUMMARY",
  gp = grid::gpar(fontsize = 13, fontface = "bold")
)

footer <- grid::textGrob(
  paste("Data source:", data_raw_path, "\nGenerated:", Sys.time()),
  gp = grid::gpar(fontsize = 10, fontface = "italic")
)

# Create spacer grobs
spacer_small <- grid::textGrob("", gp = grid::gpar(fontsize = 1))
spacer_medium <- grid::textGrob("", gp = grid::gpar(fontsize = 3))

# Export to PNG using two-section layout
png_file <- file.path(output_dir, "exclusion_summary.png")

grDevices::png(
  filename = png_file,
  width = 12,
  height = 10,
  units = "in",
  res = 300,
  bg = "white"
)

gridExtra::grid.arrange(
  title_excluded,
  table_excluded_grob,
  spacer_small,
  title_summary,
  table_summary_grob,
  spacer_medium,
  footer,
  nrow = 7,
  heights = grid::unit(c(0.015, 0.17, 0.005, 0.02, 0.68, 0.01, 0.1), "npc")
)

grDevices::dev.off()

cat("Exclusion summary report generated: ", png_file, "\n")
