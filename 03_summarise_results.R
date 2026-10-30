################################################################################
# 03_summarise_results.R
################################################################################
# Aim: Combine the validation results from every scenario into one dataset
#      and summarise performance across iterations.
#
# Requires: output from 02_run_validation.R
#
# Output (in output/summary/):
#   AllIter.Rdata    - `combined_df`: one row per scenario x iteration x model
#   LongFormat.Rdata - `summarised_df_long`: one row per scenario x model x
#                      measure, with
#                        AVG     - mean across iterations
#                        LCI/UCI - 2.5th / 97.5th percentiles across iterations
#                        MCSE    - Monte Carlo standard error of AVG
#                        NACount - iterations where the measure was not estimable
#   summary_long.csv - `summarised_df_long` as a CSV (e.g. for supplementary tables)
#
# Run from the project root. Settings are in R/settings.R.
################################################################################

source(file.path("R", "settings.R"))
source(file.path("R", "functions.R"))

dir.create(summary_dir, showWarnings = FALSE, recursive = TRUE)

performance_measures <- c("AUC", "Brier_scaled", "Brier", "bias", "mse",
                          "Cal_Int", "Cal_Slope", "rmse")

################################################################################
# 1. Load every scenario's results
################################################################################
val_design <- build_val_design(val_parameters, val_seed, mechanisms_to_run)

available <- file.exists(file.path(val_results_dir, val_design$val_file))
if (any(!available)) {
  warning(sum(!available), " scenario result file(s) not found and will be skipped:\n  ",
          paste(val_design$val_file[!available], collapse = "\n  "))
}
val_design <- val_design[available, ]

## Extract the per-iteration performance measures from one results file
extract_target_measures <- function(file) {
  load(file)  # loads simulation_results

  bind_rows(lapply(seq_along(simulation_results$iterations), function(i) {
    simulation_results$iterations[[i]]$target_measures %>%
      mutate(across(any_of(performance_measures), as.numeric),  # drops pROC 'auc' class
             iteration = i, .before = 1)
  }))
}

combined_df <- bind_rows(lapply(seq_len(nrow(val_design)), function(k) {
  s <- val_design[k, ]
  message("Loading ", s$val_file)
  extract_target_measures(file.path(val_results_dir, s$val_file)) %>%
    mutate(mechanism = s$mechanism, N_val = s$N_val,
           Y_prev = s$Y_prev, R_prev = s$R_prev, .before = 1)
}))

################################################################################
# 2. Labels for tables and figures
################################################################################
ordered_factor <- function(label, value) {
  factor(label, levels = unique(label[order(value)]))
}

add_labels <- function(df) {
  df %>%
    mutate(Method      = factor(method_labels[Model], levels = method_labels),
           Mechanism   = factor(mechanism_labels[mechanism], levels = mechanism_labels),
           Missingness = ordered_factor(paste0(R_prev * 100, "%"), R_prev),
           samplesize  = ordered_factor(paste0("N=", format(N_val, big.mark = ",",
                                                            scientific = FALSE, trim = TRUE)),
                                        N_val),
           Prevalence  = ordered_factor(paste0(Y_prev * 100, "%"), Y_prev))
}

combined_df <- add_labels(combined_df)

################################################################################
# 3. Summarise across iterations
################################################################################
summarised_df_long <- combined_df %>%
  tidyr::pivot_longer(all_of(performance_measures),
                      names_to = "Measure", values_to = "value") %>%
  group_by(mechanism, N_val, Y_prev, R_prev, Model,
           Method, Mechanism, Missingness, samplesize, Prevalence, Measure) %>%
  summarise(AVG     = mean(value, na.rm = TRUE),
            LCI     = quantile(value, 0.025, na.rm = TRUE),
            UCI     = quantile(value, 0.975, na.rm = TRUE),
            MCSE    = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
            NACount = sum(is.na(value)),
            n_iter  = n(),
            .groups = "drop") %>%
  mutate(Measure = factor(Measure, levels = performance_measures))

################################################################################
# 4. Save
################################################################################
save(combined_df,        file = file.path(summary_dir, "AllIter.Rdata"))
save(summarised_df_long, file = file.path(summary_dir, "LongFormat.Rdata"))
write.csv(summarised_df_long, file.path(summary_dir, "summary_long.csv"), row.names = FALSE)

message("Summarised ", nrow(val_design), " scenario(s)")
