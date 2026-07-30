rm(list = ls())
library(tidyverse)

data_raw <- read_csv("data/raw/data_raw.csv", show_col_types = FALSE)

acc <- data_raw %>%
  group_by(participant_id) %>%
  summarise(
    ACC = mean(is_correct, na.rm = TRUE),
    .groups = "drop"
  )

print(acc)
