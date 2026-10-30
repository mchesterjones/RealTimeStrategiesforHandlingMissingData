################################################################################
# 01_generate_development_data.R
################################################################################
# Aim: Simulate one development dataset per missingness mechanism and outcome
#      prevalence, and fit the candidate prediction models to each.
#
# Output (in output/development_datasets/): one .Rdata per mechanism x Y_prev,
# each containing a list `development_dataset` with elements:
#   dev_data  - simulated development data (true and amputed x_1, x_3)
#   model     - logistic model on fully observed (true) covariates
#   refmodel  - reference-value imputation model + reference values
#   submodel  - pattern sub-models (Mercaldo & Blume, 2020)
#   rimodel   - regression imputation models + outcome model
#   xgmodel   - XGBoost model using native missing-value handling
#
# Missing-data pattern plots are saved to output/figures/.
#
# Run from the project root. Settings are in R/settings.R.
################################################################################

source(file.path("R", "settings.R"))
source(file.path("R", "functions.R"))

dir.create(dev_data_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(figure_dir,   showWarnings = FALSE, recursive = TRUE)

for (mechanism in mechanisms_to_run) {

  mdef <- mechanism_definitions[[mechanism]]
  message("==== Development data: ", mechanism, " (seed ", dev_seeds[[mechanism]], ") ====")

  # Seed set once per mechanism, before looping over prevalences
  set.seed(dev_seeds[[mechanism]])

  for (i in seq_len(nrow(dev_parameters))) {

    pars <- dev_parameters[i, ]
    message("  Y_prev = ", pars$Y_prev)

    # 1. Simulate development data
    dev_data <- simulate_data(N       = pars$N_dev,
                              Y_prev  = pars$Y_prev,
                              R_prev  = dev_prop_incomplete,
                              gamma   = gamma_true,
                              mech    = mdef$mech,
                              weights = mdef$weights)
    print_data_summary(dev_data)

    # 2. Missing-data pattern plot
    pdf(file.path(figure_dir,
                  paste0(mechanism, "_pattern_plot_Yprev_", pars$Y_prev, ".pdf")),
        width = 7, height = 5)
    invisible(md.pattern(dev_data[, c("x_1", "x_2", "x_3", "x_4", "x_5", "U")]))
    dev.off()

    # 3. Fit models and save
    development_dataset <- c(list(dev_data = dev_data), fit_all_models(dev_data))

    save(development_dataset,
         file = file.path(dev_data_dir, dev_filename(mechanism, pars$Y_prev)))
  }
}
