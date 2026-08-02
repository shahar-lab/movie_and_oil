#### MANUSCRIPT PARAGRAPH ####

# Reads the objects converting_to_data_processed.R leaves in the global environment
# (window_exit_subjects, rt_trim_subjects, after_rt_trim_pct, df_processed) so the numbers
# in the paragraph are guaranteed consistent with what was actually excluded.
#
# exclusion_summary.R is sourced in between, but keeps all of its own working objects under
# report-local names (df_raw_report, df_processed_report, after_window_exit_report,
# window_exit_table, rt_trim_table), so none of the four objects read here are shadowed.
n_window_exit_excluded <- length(window_exit_subjects)
n_rt_trim_excluded     <- length(rt_trim_subjects)

pct_rt_trimmed <- round(
  100 * (nrow(after_rt_trim_pct) - nrow(df_processed)) / nrow(after_rt_trim_pct),
  2
)

n_trials_final       <- nrow(df_processed)
n_participants_final <- n_distinct(df_processed$participant_id)
mean_trials_final    <- round(n_trials_final / n_participants_final, 2)

paragraph <- paste0(
  "Data treatment. During data preprocessing, we first examined the quality of the ",
  "behavioral data at the participant level. Participants who left the browser window more ",
  "than ", window_exit_max, " time(s) were excluded (n = ", n_window_exit_excluded, "). ",
  "Among the remaining participants, we further excluded those for whom more than ",
  rt_trim_pct_max, "% of trials had a reaction time outside the plausible range of ",
  rt_min, "-", rt_max, " ms (n = ", n_rt_trim_excluded, "). ",
  "From the remaining behavioral observations we omitted trials with a reaction time below ",
  rt_min, " ms or above ", rt_max, " ms (", pct_rt_trimmed, "% of all remaining trials). ",
  "This resulted in ", format(n_trials_final, big.mark = ","), " trials across ",
  n_participants_final, " participants (", mean_trials_final, " mean trials per participant)."
)

writeLines(paragraph, file.path(output_dir, "manuscript_paragraph.md"))

cat("Manuscript paragraph written: ", file.path(output_dir, "manuscript_paragraph.md"), "\n")
