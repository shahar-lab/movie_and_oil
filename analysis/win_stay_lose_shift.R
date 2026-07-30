rm(list = ls())
library(tidyverse)

data_raw <- read_csv("data/raw/data_raw.csv", show_col_types = FALSE)

# Remove first row of each participant (no previous reward)
data_analysis <- data_raw %>%
  filter(!is.na(reward_previous_trial))

# Fit logistic regression for each participant
models <- data_analysis %>%
  nest(data = -participant_id) %>%
  mutate(
    model = map(data, ~glm(stay_card ~ reward_previous_trial,
                           family = binomial(link = "logit"),
                           data = .x)),
    summary = map(model, summary)
  )

# Print results for each participant
for (i in seq_len(nrow(models))) {
  cat("\n=== Participant:", models$participant_id[i], "===\n")
  print(models$summary[[i]])
}
