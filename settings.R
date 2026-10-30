################################################################################
# R/settings.R
################################################################################
# All user-editable settings for the simulation study live here.
# Sourced by 01_generate_development_data.R and 02_run_validation.R.
#
# All paths are relative to the project root: run scripts with the working
# directory set to the project root (e.g. open the .Rproj file in RStudio).
################################################################################

library(tidyr)  # crossing()

################################################################################
# Output locations
################################################################################
dev_data_dir    <- file.path("output", "development_datasets")
figure_dir      <- file.path("output", "figures")
val_results_dir <- file.path("output", "validation_results")
summary_dir     <- file.path("output", "summary")
results_fig_dir <- file.path("output", "figures", "results")
log_dir         <- file.path("output", "logs")

################################################################################
# Missingness mechanisms to run
################################################################################
## Any subset of: "MCAR", "MAR", "MNAR", "MNAR_NoU"
## (definitions in R/functions.R, section 1)
mechanisms_to_run <- c("MCAR", "MAR", "MNAR", "MNAR_NoU")

################################################################################
# True data-generating parameters (shared by development and validation)
################################################################################
## Effect of each covariate (log-odds ratio) on the outcome Y
gamma_true <- c(gamma_x1 = 0.5,
                gamma_x2 = 0.5,
                gamma_x3 = 0.5,
                gamma_x4 = 0.5,
                gamma_x5 = 0.5,
                gamma_U  = 0.5)

################################################################################
# Development data
################################################################################
dev_parameters <- crossing(
  N_dev  = 100000,
  Y_prev = c(0.1, 0.01)   # outcome prevalence: 10% and 1%
)

dev_prop_incomplete <- 0.25  # proportion of development rows with missing data

## One seed per mechanism (as used in the original development scripts)
dev_seeds <- c(MCAR     = 13245,
               MAR      = 1234,
               MNAR     = 2875,
               MNAR_NoU = 132)

################################################################################
# Validation simulation
################################################################################
val_parameters <- crossing(
  n_iter = 200,                       # repetitions per scenario
  N_val  = c(500, 10000, 100000),     # validation sample size
  Y_prev = c(0.1, 0.01),              # outcome prevalence
  R_prev = c(0.25, 0.5, 0.75)         # proportion of rows with missing data
)

## Each scenario (mechanism x N_val x Y_prev x R_prev) gets its own seed:
## val_seed + scenario_id, where scenario_id is the row of the full design
## across all four mechanisms (see 02_run_validation.R).
val_seed <- 541756

## Parallel processing: number of worker processes
n_cores <- max(1, parallel::detectCores() - 1)

## What to keep for every iteration, in addition to the performance measures.
## Not needed by 03/04 (bias, mse and rmse are now computed during
## validation). Predictions for N_val = 100000 are large (approx. 1 GB in
## memory per scenario across 200 iterations).
save_predictions <- FALSE  # individual predicted risks from each model
save_val_data    <- FALSE  # full simulated validation dataset

## Skip scenarios whose results file already exists (allows resuming a run)
overwrite_existing <- FALSE

################################################################################
# Labels for tables and figures
################################################################################
## Order here sets the order in figures and legends
method_labels <- c(Prediction_RI       = "Regression Imputation",
                   Prediction_Ref      = "Reference Values",
                   Prediction_SubModel = "Pattern sub-model",
                   Prediction_XG       = "XGBoost",
                   Predictions_CRI     = "Fully Observed")

mechanism_labels <- c(MCAR     = "MCAR",
                      MAR      = "MAR",
                      MNAR     = "MNAR-WithY",
                      MNAR_NoU = "MNAR-NoY")

################################################################################
# Figures
################################################################################
## Mechanisms shown together in each combined figure (one row per mechanism)
figure_groups <- list(MCAR_MAR = c("MCAR", "MAR"),
                      MNAR     = c("MNAR", "MNAR_NoU"))

## Performance measures plotted (columns of each combined figure, in order).
## `xlim` / `by` fix the axis range and tick spacing for the main scenario
## only (`main_scenario`); other scenarios use axes fitted to their data.
## `ref` draws a dashed reference line (NULL for none).
plot_measures <- list(
  AUC       = list(column = "AUC",          title = "Discrimination",           xlim = c(0.78, 0.84),  by = 0.02, ref = NULL),
  Brier     = list(column = "Brier_scaled", title = "Brier Score Scaled",       xlim = c(0.10, 0.20),  by = 0.02, ref = NULL),
  Cal_Slope = list(column = "Cal_Slope",    title = "Calibration Slope",        xlim = c(0.90, 1.10),  by = 0.05, ref = 1),
  Cal_Int   = list(column = "Cal_Int",      title = "Calibration in the Large", xlim = c(-0.35, 0.15), by = 0.10, ref = 0)
)

main_scenario <- list(N_val = 100000, Y_prev = 0.1)

method_colours <- c("Regression Imputation" = "#984ea3",
                    "Reference Values"      = "#ff7f00",
                    "Pattern sub-model"     = "#4daf4a",
                    "XGBoost"               = "#377eb8",
                    "Fully Observed"        = "black")
