rm(list = ls())

#### SETUP ####

library(here)
library(tidyverse)
library(gridExtra)
library(grid)

project_root       <- here::here()
code_dir           <- file.path(project_root, "preprocessing", "code")
data_collected_dir <- file.path(project_root, "data", "collected")
data_raw_dir       <- file.path(project_root, "data", "raw")
data_processed_dir <- file.path(project_root, "data", "processed")
output_dir         <- file.path(project_root, "preprocessing", "output")

data_raw_path      <- file.path(data_raw_dir, "data_raw.csv")

dir.create(data_raw_dir, showWarnings = FALSE)
dir.create(data_processed_dir, showWarnings = FALSE)
dir.create(output_dir, showWarnings = FALSE)

#### FILTERING PARAMETERS ####

rt_min          <- 150
rt_max          <- 4000
# A window exit is an episode, not a trial: consecutive flagged trials belong to one exit, and a
# new exit is only counted once the participant has returned to the window. Participants are
# excluded when their episode count is strictly greater than this value, so 1 tolerates a single
# departure of any length and removes anyone who left, came back, and left again.
window_exit_max <- 1
rt_trim_pct_max <- 10

#### EXECUTE PIPELINE ####

source(file.path(code_dir, "converting_to_data_raw.R"))
source(file.path(code_dir, "converting_to_data_processed.R"))

source(file.path(code_dir, "exclusion_summary.R"))
source(file.path(code_dir, "manuscript_paragraph.R"))
source(file.path(code_dir, "examine_rt_in_raw_data.R"))
