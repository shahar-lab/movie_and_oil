#### SCATTER PLOT VISUALIZATION - PARTICIPANT LEVEL ####

# Plot for video_present == TRUE
p_with_movie <- df_agg |>
  filter(video_present == TRUE) |>
  ggplot(aes(x = reward_oneback, y = avg_stay_key, colour = participant_id)) +
  geom_point(size = 4, alpha = 0.7) +
  scale_colour_manual(values = participant_palette) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "With Movie",
    x = "Previous Trial Outcome",
    y = "Average Stay Side"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

# Plot for video_present == FALSE
p_without_movie <- df_agg |>
  filter(video_present == FALSE) |>
  ggplot(aes(x = reward_oneback, y = avg_stay_key, colour = participant_id)) +
  geom_point(size = 4, alpha = 0.7) +
  scale_colour_manual(values = participant_palette) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "Without Movie",
    x = "Previous Trial Outcome",
    y = "Average Stay Side"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

#### SCATTER PLOT VISUALIZATION - GROUP LEVEL ####

# Group-level plot for video_present == TRUE
p_group_with_movie <- df_agg |>
  filter(video_present == TRUE) |>
  ggplot(aes(x = reward_oneback, y = avg_stay_key)) +
  stat_summary(fun = mean, geom = "point", size = 5, colour = "black") +
  stat_summary(fun = mean, geom = "line", colour = "black", linewidth = 1, aes(group = 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "With Movie (Group Level)",
    x = "Previous Trial Outcome",
    y = "Average Stay Side"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

# Group-level plot for video_present == FALSE
p_group_without_movie <- df_agg |>
  filter(video_present == FALSE) |>
  ggplot(aes(x = reward_oneback, y = avg_stay_key)) +
  stat_summary(fun = mean, geom = "point", size = 5, colour = "black") +
  stat_summary(fun = mean, geom = "line", colour = "black", linewidth = 1, aes(group = 1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title = "Without Movie (Group Level)",
    x = "Previous Trial Outcome",
    y = "Average Stay Side"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10)
  )

#### ASSEMBLE AND TAG PANELS ####

p_final <- (p_with_movie / p_group_with_movie) | (p_without_movie / p_group_without_movie) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(face = "bold", size = 14))

#### EXPORT ####

plot_name <- "wsls_key_by_video"

ggsave(
  file.path(output_dir, paste0(plot_name, ".pdf")),
  plot = p_final,
  width = 12, height = 10, bg = "white"
)

ggsave(
  file.path(output_dir, paste0(plot_name, ".png")),
  plot = p_final,
  width = 12, height = 10, dpi = 300, bg = "white"
)
