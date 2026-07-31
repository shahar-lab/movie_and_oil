#### LOAD PREPARED DATA ####

accuracy_per_participant <- readRDS(file.path(artifacts_dir, "accuracy_per_participant.rds"))
overall_accuracy <- readRDS(file.path(artifacts_dir, "overall_accuracy.rds"))
accuracy_summary <- readRDS(file.path(artifacts_dir, "accuracy_summary.rds"))

#### CREATE BAR PLOT ####

overall_mean <- overall_accuracy$accuracy
paul_tol_2 <- c("#1B9E77", "#D95F02")

p_accuracy <- ggplot(accuracy_per_participant, aes(x = reorder(participant_id, accuracy), y = accuracy, fill = participant_id)) +
  geom_col(colour = "black", linewidth = 0.3) +
  geom_hline(yintercept = overall_mean, linetype = "dashed", linewidth = 0.8, colour = "darkgray") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_fill_manual(values = rep(paul_tol_2, length.out = nrow(accuracy_per_participant))) +
  labs(
    title = "Accuracy by Participant",
    x = "Participant",
    y = "Accuracy",
    caption = paste0("Overall mean (dashed line): ", round(overall_mean, 3))
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(face = "bold", size = 14),
    plot.caption = element_text(size = 10),
    legend.position = "none",
    panel.grid.major.y = element_line(colour = "gray90"),
    panel.background = element_rect(fill = "white", colour = NA)
  )

#### EXPORT BAR PLOT ####

plot_name <- "accuracy_by_participant"

ggsave(
  file.path(output_dir, paste0(plot_name, ".pdf")),
  plot = p_accuracy,
  width = 10, height = 8, bg = "white"
)

ggsave(
  file.path(output_dir, paste0(plot_name, ".png")),
  plot = p_accuracy,
  width = 10, height = 8, dpi = 300, bg = "white"
)

#### CREATE SUMMARY TABLE ####

table_df <- accuracy_summary |>
  mutate(
    percentage = paste0(round(accuracy * 100, 1), "%"),
    accuracy_rounded = round(accuracy, 3)
  ) |>
  select(participant_id, n_trials, accuracy_rounded, percentage) |>
  rename(
    Participant = participant_id,
    N_Trials = n_trials,
    Accuracy = accuracy_rounded,
    Percentage = percentage
  )

table_grob <- tableGrob(
  table_df,
  rows = NULL,
  theme = ttheme_default(
    core = list(fg_params = list(hjust = 0, x = 0.1)),
    colhead = list(fg_params = list(hjust = 0, x = 0.1))
  )
)

#### EXPORT TABLE AS PDF ####

pdf(file.path(output_dir, "accuracy_summary.pdf"), width = 8.5, height = 11)
grid.draw(table_grob)
dev.off()
