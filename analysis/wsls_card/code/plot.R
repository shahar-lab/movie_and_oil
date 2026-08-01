#### BOXPLOT VISUALIZATION ####

# Plot for video_present == TRUE
p_with_video <- df_agg |>
  filter(video_present == TRUE) |>
  ggplot(aes(x = reward_label, y = avg_stay, colour = participant_id)) +
  geom_point(size = 4, alpha = 0.7) +
  scale_colour_manual(values = participant_palette) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "With Movie",
    x = "Previous Trial Outcome",
    y = "Average Stay Probability"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

# Plot for video_present == FALSE
p_without_video <- df_agg |>
  filter(video_present == FALSE) |>
  ggplot(aes(x = reward_label, y = avg_stay, colour = participant_id)) +
  geom_point(size = 4, alpha = 0.7) +
  scale_colour_manual(values = participant_palette) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "Without Movie",
    x = "Previous Trial Outcome",
    y = "Average Stay Probability"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

#### GROUP-LEVEL PLOTS ####

# Group-level plot for video_present == TRUE
p_group_with_video <- df_agg |>
  filter(video_present == TRUE) |>
  ggplot(aes(x = reward_label, y = avg_stay)) +
  stat_summary(fun = mean, geom = "point", size = 4, alpha = 0.8, color = "black") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 1.5, linetype = "solid") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "With Movie - Group Level",
    x = "Previous Trial Outcome",
    y = "Average Stay Probability"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

# Group-level plot for video_present == FALSE
p_group_without_video <- df_agg |>
  filter(video_present == FALSE) |>
  ggplot(aes(x = reward_label, y = avg_stay)) +
  stat_summary(fun = mean, geom = "point", size = 4, alpha = 0.8, color = "black") +
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "black", linewidth = 1.5, linetype = "solid") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "Without Movie - Group Level",
    x = "Previous Trial Outcome",
    y = "Average Stay Probability"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

#### ASSEMBLE AND TAG PANELS ####

p_final <- (p_with_video / p_group_with_video) | (p_without_video / p_group_without_video) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 14))

#### EXPORT ####

plot_name <- "win_stay_lose_shift_by_video"

ggsave(
  file.path(output_dir, paste0(plot_name, ".pdf")),
  plot = p_final,
  width = 10, height = 8, bg = "white"
)

ggsave(
  file.path(output_dir, paste0(plot_name, ".png")),
  plot = p_final,
  width = 10, height = 8, dpi = 300, bg = "white"
)
