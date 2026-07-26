# Read and combine all CSV files from data/collected into a single data file

rm(list = ls())

library(readr)
library(tidyverse)

# Define paths
collected_dir <- file.path("data", "collected")
output_dir <- file.path("data", "raw")

# Create output directory if it doesn't exist
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# List all CSV files in the collected directory
csv_files <- list.files(collected_dir, pattern = "\\.csv$", full.names = TRUE)

if (length(csv_files) == 0) {
  stop("No CSV files found in ", collected_dir)
}

# Read all CSV files as character first to avoid type inference issues
# This preserves all data without NA coercion from type mismatches
combined_data <- map_df(
  csv_files,
  ~ read_csv(.x, col_types = cols(.default = col_character())) %>%
    mutate(source_file = basename(.x)),
  .id = NULL
)

# Filter rows to keep only likert, pairwise, or feedback phases
combined_data <- combined_data %>%
  filter(phase %in% c("likert", "pairwise", "feedback"))

# Remove technical columns
combined_data <- combined_data %>%
  select(-c(trial_type, plugin_version, time_elapsed, prolific_study_id,
            prolific_session_id, screen_w, screen_h, timestamp, stimulus,
            success, iti_phase, response_device, stim_onset_ms,
            fixation_onset_ms, source_file))

# Save the combined data
output_file <- file.path(output_dir, "data_raw.csv")
write_csv(combined_data, output_file)

cat("Combined", length(csv_files), "CSV files into", output_file, "\n")
