rm(list = ls())

#### SETUP ####

library(here)
library(tidyverse)
library(ggplot2)
library(patchwork)

project_root <- here::here()
code_dir     <- file.path(project_root, "analysis", "wsls_card", "code")
artifacts_dir <- file.path(project_root, "analysis", "wsls_card", "artifacts")
output_dir   <- file.path(project_root, "analysis", "wsls_card", "output")
data_path    <- file.path(project_root, "data", "processed", "data_processed.csv")

#### EXECUTE PIPELINE ####

source(file.path(code_dir, "prepare_data.R"))
source(file.path(code_dir, "plot.R"))
