rm(list = ls())

#### SETUP ####

library(here)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(grid)

project_root <- here::here()
code_dir     <- file.path(project_root, "analysis", "accuracy", "code")
artifacts_dir <- file.path(project_root, "analysis", "accuracy", "artifacts")
output_dir   <- file.path(project_root, "analysis", "accuracy", "output")
data_path    <- file.path(project_root, "data", "processed",
                           "data_processed.csv")

#### EXECUTE PIPELINE ####

source(file.path(code_dir, "prepare_data.R"))
source(file.path(code_dir, "plot_accuracy.R"))
source(file.path(code_dir, "plot_learning_curve.R"))
