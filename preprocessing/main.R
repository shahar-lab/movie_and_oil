rm(list = ls())

#### SETUP ####

library(here)
library(tidyverse)
library(gridExtra)
library(grid)

project_root <- here::here()
code_dir <- file.path(project_root, "preprocessing", "code")
data_collected_dir <- file.path(project_root, "data", "collected")
data_raw_dir <- file.path(project_root, "data", "raw")
data_raw_path <- file.path(data_raw_dir, "data_raw.csv")
data_processed_dir <- file.path(project_root, "data", "processed")
output_dir <- file.path(project_root, "preprocessing", "output")
dir.create(data_raw_dir, showWarnings = FALSE)
dir.create(data_processed_dir, showWarnings = FALSE)
dir.create(output_dir, showWarnings = FALSE)

#### FILTERING PARAMETERS ####

rt_min_ms <- 200
rt_max_ms <- 3000

#### EXECUTE PIPELINE ####

source(file.path(code_dir, "converting_to_data_raw.R"))
source(file.path(code_dir, "build_processed.R"))
source(file.path(code_dir, "exclusion_summary.R"))
