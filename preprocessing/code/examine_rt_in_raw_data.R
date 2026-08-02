#### LOAD RAW DATA ####

df_rt_check <- read_csv(
  data_raw_path,
  col_types = cols(.default = col_character())
)

df_rt_check <- df_rt_check |>
  mutate(
    participant_id = as.character(participant_id),
    trial          = as.numeric(trial),
    rt             = as.numeric(rt)
  )

# Same predicate the exclusion criterion counts, so the figure marks exactly those trials
window_exit_trials <- df_rt_check |>
  filter(window_status != "ok")

overflow_band_y <- 525

# ASSUMED[no boundary/band geometry given]: 0-500 main region, boundary line
# at y=500, overflow band drawn as a compressed strip from 500 to 550 with
# every over-threshold trial marked by a fixed "x" shape (not proportional
# to its true RT) so the band signals occurrence only, never magnitude
overflow_trials <- df_rt_check |>
  filter(rt > 500)

#### BUILD PANEL PER PARTICIPANT ####

p_rt_raw <- df_rt_check |>
  ggplot(aes(x = trial, y = rt)) +
  geom_line(colour = "#4477AA", linewidth = 0.3) +
  geom_point(colour = "#4477AA", size = 0.6, alpha = 0.6) +
  geom_vline(
    data = window_exit_trials,
    aes(xintercept = trial),
    colour = "#D55E00", linewidth = 0.4
  ) +
  geom_hline(yintercept = rt_min, linetype = "dashed", colour = "grey50") +
  geom_hline(yintercept = rt_max, linetype = "dashed", colour = "grey50") +
  coord_cartesian(ylim = c(0, 6000)) +
  facet_wrap(~ participant_id) +
  labs(
    title = "Raw RT spread per participant (pre-exclusion)",
    x = "Trial",
    y = "RT (ms)"
  ) +
  theme_bw()

#### BUILD ZOOMED PANEL (0-500ms + >500 overflow band) ####

p_rt_zoom <- df_rt_check |>
  ggplot(aes(x = trial, y = rt)) +
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf, ymin = 500, ymax = 550,
    fill = "grey85", alpha = 0.6
  ) +
  geom_line(colour = "#4477AA", linewidth = 0.3) +
  geom_point(colour = "#4477AA", size = 0.6, alpha = 0.6) +
  geom_point(
    data = overflow_trials,
    aes(x = trial, y = overflow_band_y),
    shape = 4, size = 2, colour = "#D55E00"
  ) +
  geom_vline(
    data = window_exit_trials,
    aes(xintercept = trial),
    colour = "#D55E00", linewidth = 0.4
  ) +
  geom_hline(yintercept = rt_min, linetype = "dashed", colour = "grey50") +
  geom_hline(yintercept = rt_max, linetype = "dashed", colour = "grey50") +
  geom_hline(yintercept = 500, colour = "black", linewidth = 0.4) +
  coord_cartesian(ylim = c(0, 550)) +
  # ASSUMED[no tick spacing given]: 100ms breaks across 0-500 main region
  scale_y_continuous(
    breaks = c(0, 100, 200, 300, 400, 500, overflow_band_y),
    labels = c("0", "100", "200", "300", "400", "500", ">500")
  ) +
  facet_wrap(~ participant_id) +
  labs(
    title = "Raw RT spread per participant, zoomed to fast RTs (pre-exclusion)",
    x = "Trial",
    y = "RT (ms)"
  ) +
  theme_bw()

#### EXPORT ####

plot_name <- "examine_rt_in_raw_data"

# ASSUMED[no multi-page mechanism specified]: opened a single pdf device and
# printed both ggplot objects to it in sequence, giving two consecutive pages
# of one PDF file rather than two separate PDFs
pdf(
  file.path(output_dir, paste0(plot_name, ".pdf")),
  width = 10, height = 8, bg = "white"
)
print(p_rt_raw)
print(p_rt_zoom)
dev.off()

ggsave(
  file.path(output_dir, paste0(plot_name, ".png")),
  plot = p_rt_raw,
  width = 10, height = 8, dpi = 300, bg = "white"
)

# ASSUMED[no naming convention given]: second PNG named with a "_zoomed" suffix
# since PNG has no multi-page concept
ggsave(
  file.path(output_dir, paste0(plot_name, "_zoomed.png")),
  plot = p_rt_zoom,
  width = 10, height = 8, dpi = 300, bg = "white"
)
