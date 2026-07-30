rm(list = ls())

#### SETUP ####

library(here)
library(tidyverse)
library(ggplot2)
library(patchwork)

project_root <- here::here()
code_dir     <- file.path(project_root, "analysis", "win_stay_lose_shift", "code")
artifacts_dir <- file.path(project_root, "analysis", "win_stay_lose_shift", "artifacts")
output_dir   <- file.path(project_root, "analysis", "win_stay_lose_shift", "output")
data_path    <- file.path(project_root, "data", "raw", "data_raw.csv")

#### EXECUTE PIPELINE ####

source(file.path(code_dir, "prepare_data.R"))
source(file.path(code_dir, "plot.R"))
