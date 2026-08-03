#### LOAD PREPARED DATA ####

learning_curve_summary <- readRDS(file.path(artifacts_dir, "learning_curve_summary.rds"))
learning_curve_trials  <- readRDS(file.path(artifacts_dir, "learning_curve_trials.rds"))

learning_curve_summary_filtered <- readRDS(file.path(artifacts_dir, "learning_curve_summary_filtered.rds"))
learning_curve_trials_filtered  <- readRDS(file.path(artifacts_dir, "learning_curve_trials_filtered.rds"))

#### FIT LOGISTIC LEARNING CURVES PER CONDITION ####

fit_learning_curves <- function(trials_df) {
  conditions <- unique(trials_df$condition)
  trial_range <- tibble(trial_in_block = seq(min(trials_df$trial_in_block),
                                              max(trials_df$trial_in_block)))
  map_dfr(conditions, function(cond) {
    fit <- glm(is_correct ~ trial_in_block,
               data = filter(trials_df, condition == cond),
               family = binomial)
    trial_range |>
      mutate(
        condition       = cond,
        fitted_accuracy = predict(fit, newdata = trial_range, type = "response")
      )
  })
}

fitted_curves          <- fit_learning_curves(learning_curve_trials)
fitted_curves_filtered <- fit_learning_curves(learning_curve_trials_filtered)

#### CREATE LEARNING CURVE PLOTS ####

condition_colors <- c("With Video" = "#0072B2", "Without Video" = "#D55E00")

make_learning_curve_plot <- function(summary_df, fitted_df, subtitle = NULL) {
  ggplot(summary_df, aes(x = trial_bin, y = mean_accuracy, colour = condition)) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0, alpha = 0.4) +
    geom_point(size = 1.8, alpha = 0.75) +
    geom_line(data = fitted_df, aes(x = trial_in_block, y = fitted_accuracy, colour = condition),
              linewidth = 1) +
    scale_colour_manual(values = condition_colors) +
    scale_x_continuous(breaks = seq(1, 41, 10)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    labs(
      title    = "Learning Curve by Condition",
      subtitle = subtitle,
      x        = "Trial in Block",
      y        = "Accuracy",
      colour   = "Condition"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      panel.grid.minor = element_blank(),
      legend.position = "top"
    )
}

n_high_accuracy <- length(unique(learning_curve_trials_filtered$participant_id))

p_learning_curve <- make_learning_curve_plot(learning_curve_summary, fitted_curves)

p_learning_curve_filtered <- make_learning_curve_plot(
  learning_curve_summary_filtered, fitted_curves_filtered,
  subtitle = paste0("Filtered to participants with overall accuracy >= 0.55 (n = ", n_high_accuracy, ")")
)

#### EXPORT LEARNING CURVE PLOTS ####

plot_name <- "learning_curve"

pdf(file.path(output_dir, paste0(plot_name, ".pdf")), width = 10, height = 8, bg = "white")
print(p_learning_curve)
print(p_learning_curve_filtered)
dev.off()

ggsave(
  file.path(output_dir, paste0(plot_name, ".png")),
  plot = p_learning_curve,
  width = 10, height = 8, dpi = 300, bg = "white"
)

ggsave(
  file.path(output_dir, paste0(plot_name, "_filtered.png")),
  plot = p_learning_curve_filtered,
  width = 10, height = 8, dpi = 300, bg = "white"
)
