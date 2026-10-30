################################################################################
# 02_run_validation.R
################################################################################
# Aim: For every scenario (mechanism x N_val x Y_prev x R_prev), repeatedly
#      simulate validation data with missing x_1 / x_3, apply the models from
#      01_generate_development_data.R, and record predictive performance.
#
# Requires: output from 01_generate_development_data.R
#
# Output (in output/validation_results/): one .Rdata per scenario containing
# a list `simulation_results` with elements:
#   parameters - scenario settings, seed and true gamma values
#   iterations - list of length n_iter; each element contains
#                  target_measures - performance of each model (all that
#                                    03/04 need)
#                  predictions     - predicted risks (only if save_predictions)
#                  val_data        - simulated data   (only if save_val_data)
#
# Scenarios run in parallel; progress is written to output/logs/.
# Scenarios whose results file already exists are skipped unless
# overwrite_existing = TRUE (so an interrupted run can be resumed).
#
# Run from the project root. Settings are in R/settings.R.
################################################################################

source(file.path("R", "settings.R"))
source(file.path("R", "functions.R"))
library(parallel)
library(foreach)
library(doParallel)

dir.create(val_results_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(log_dir,         showWarnings = FALSE, recursive = TRUE)

################################################################################
# 1. Scenario design
################################################################################
## Seeds are assigned over the full design (all four mechanisms) so that a
## scenario keeps the same seed whichever subset of mechanisms is run.
val_design <- build_val_design(val_parameters, val_seed, mechanisms_to_run)

## Check development datasets exist
dev_files_needed <- unique(file.path(dev_data_dir,
                                     mapply(dev_filename, val_design$mechanism, val_design$Y_prev)))
missing_dev <- dev_files_needed[!file.exists(dev_files_needed)]
if (length(missing_dev) > 0) {
  stop("Development datasets not found (run 01_generate_development_data.R first):\n  ",
       paste(missing_dev, collapse = "\n  "))
}

## Skip scenarios already completed
if (!overwrite_existing) {
  done <- file.exists(file.path(val_results_dir, val_design$val_file))
  if (any(done)) message("Skipping ", sum(done), " scenario(s) with existing results")
  val_design <- val_design[!done, ]
}

## Largest scenarios first, for better load balancing across workers
val_design <- val_design %>% arrange(desc(N_val), scenario_id)

message("Scenarios to run: ", nrow(val_design))

################################################################################
# 2. Run scenarios in parallel
################################################################################
if (nrow(val_design) > 0) {

  ## Absolute paths so workers write to the right place regardless of their
  ## working directory (computed at run time; nothing machine-specific is
  ## stored in the code)
  dev_data_dir_abs    <- normalizePath(dev_data_dir)
  val_results_dir_abs <- normalizePath(val_results_dir)
  functions_file      <- normalizePath(file.path("R", "functions.R"))

  n_workers <- min(n_cores, nrow(val_design))
  log_file  <- file.path(log_dir, paste0("validation_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))

  if (n_workers > 1) {
    cl <- makeCluster(n_workers, outfile = log_file)
    registerDoParallel(cl)
    clusterCall(cl, function(f) { source(f); NULL }, functions_file)
  } else {
    registerDoSEQ()
  }
  message("Running on ", n_workers, " worker(s); progress log: ", log_file)

  status <- foreach(i = seq_len(nrow(val_design)),
                    .combine = rbind,
                    .errorhandling = "pass") %dopar% {
    run_validation_scenario(scenario         = as.list(val_design[i, ]),
                            gamma            = gamma_true,
                            dev_data_dir     = dev_data_dir_abs,
                            val_results_dir  = val_results_dir_abs,
                            save_predictions = save_predictions,
                            save_val_data    = save_val_data)
  }

  if (n_workers > 1) stopCluster(cl)

  ##############################################################################
  # 3. Summary
  ##############################################################################
  print(table(status$status))
  if (any(status$status == "failed")) {
    message("Failed scenarios:")
    print(status[status$status == "failed", ])
  }
  write.csv(status,
            file.path(log_dir, paste0("validation_status_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")),
            row.names = FALSE)
}
