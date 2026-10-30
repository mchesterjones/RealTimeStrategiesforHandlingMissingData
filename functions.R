################################################################################
# R/functions.R
################################################################################
# Data-generating mechanism and all functions for the simulation study.
# Sourced by 01_generate_development_data.R, 02_run_validation.R and by each
# parallel worker.
#
# Sections
#   1. Data-generating mechanism (covariates, missingness)
#   2. Data simulation
#   3. Model development
#   4. Predictions in validation data
#   5. Predictive performance
#   6. Running the validation simulation
################################################################################

library(dplyr)
library(mice)     # ampute(), md.pattern()
library(xgboost)
library(pROC)     # roc(), var() for DeLong AUC variance

################################################################################
# 1. Data-generating mechanism
################################################################################

## Correlation structure of (x_1, x_2, x_3, x_4, x_5, U)
## x_1 is strongly correlated with x_4 and x_5; U is independent of all.
sigma_covariates <- matrix(c(1.0, 0.2, 0.2, 0.7, 0.7, 0,
                             0.2, 1.0, 0.2, 0.2, 0.2, 0,
                             0.2, 0.2, 1.0, 0.2, 0.2, 0,
                             0.7, 0.2, 0.2, 1.0, 0.5, 0,
                             0.7, 0.2, 0.2, 0.5, 1.0, 0,
                             0.0, 0.0, 0.0, 0.0, 0.0, 1),
                           nrow = 6, byrow = TRUE)

## Missing-data patterns (columns: x_1, x_2, x_3, x_4, x_5, U; 0 = missing)
##   row 1: x_1 missing
##   row 2: x_1 and x_3 missing
##   row 3: x_3 missing
missing_patterns <- matrix(c(0, 1, 1, 1, 1, 1,
                             0, 1, 0, 1, 1, 1,
                             1, 1, 0, 1, 1, 1),
                           nrow = 3, byrow = TRUE)

## ampute() weights: rows = patterns (as above), columns = x_1, x_2, x_3, x_4,
## x_5, U. A non-zero weight means that variable drives the probability of
## the pattern.

## MAR: missingness depends on observed x_2 only
mar_weights <- matrix(0, nrow = 3, ncol = 6)
mar_weights[1, 2] <- 1  # x_1 missing       <- x_2
mar_weights[2, 2] <- 1  # x_1 & x_3 missing <- x_2
mar_weights[3, 2] <- 1  # x_3 missing       <- x_2

## MNAR: missingness depends on the missing value itself, x_2 and unmeasured U
mnar_weights <- matrix(0, nrow = 3, ncol = 6)
mnar_weights[1, c(1, 2, 6)]    <- 0.5  # x_1 missing       <- x_1, x_2, U
mnar_weights[2, c(1, 2, 3, 6)] <- 0.5  # x_1 & x_3 missing <- x_1, x_2, x_3, U
mnar_weights[3, c(2, 3, 6)]    <- 0.5  # x_3 missing       <- x_2, x_3, U

## MNAR without U: as MNAR but U does not drive missingness
mnar_nou_weights <- matrix(0, nrow = 3, ncol = 6)
mnar_nou_weights[1, c(1, 2)]    <- 0.5  # x_1 missing       <- x_1, x_2
mnar_nou_weights[2, c(1, 2, 3)] <- 0.5  # x_1 & x_3 missing <- x_1, x_2, x_3
mnar_nou_weights[3, c(2, 3)]    <- 0.5  # x_3 missing       <- x_2, x_3

## `file_label` keeps the original output filenames so existing results and
## downstream scripts continue to work.
mechanism_definitions <- list(
  MCAR     = list(mech = "MCAR", weights = NULL,             file_label = "MCAR"),
  MAR      = list(mech = "MAR",  weights = mar_weights,      file_label = "MAR"),
  MNAR     = list(mech = "MNAR", weights = mnar_weights,     file_label = "MNAR"),
  MNAR_NoU = list(mech = "MNAR", weights = mnar_nou_weights, file_label = "MNAR_NoY")
)

## Validation scenario design --------------------------------------------------
## Full design across all mechanisms. Seeds are assigned before filtering so a
## scenario keeps the same seed whichever subset of mechanisms is run.
build_val_design <- function(val_parameters, val_seed, mechanisms_to_run) {
  tidyr::crossing(mechanism = names(mechanism_definitions), val_parameters) %>%
    mutate(scenario_id = row_number(),
           seed        = val_seed + scenario_id) %>%
    filter(mechanism %in% mechanisms_to_run) %>%
    mutate(val_file = mapply(val_filename, mechanism, N_val, Y_prev, R_prev))
}

## Filenames ------------------------------------------------------------------
dev_filename <- function(mechanism, Y_prev) {
  paste0("DevData_", mechanism_definitions[[mechanism]]$file_label,
         "_Yprev_", Y_prev, ".Rdata")
}

val_filename <- function(mechanism, N_val, Y_prev, R_prev) {
  paste0("ValData_", mechanism_definitions[[mechanism]]$file_label,
         "_Nval_", N_val, "_Yprev_", Y_prev, "_Rprev_", R_prev, ".Rdata")
}

################################################################################
# 2. Data simulation (used for both development and validation data)
################################################################################

## Inverse logit: P(Y = 1) = exp(x) / (1 + exp(x))
expit_function <- function(x) {1 / (1 + exp(-x))}

simulate_data <- function(N,          # number of individuals
                          Y_prev,     # target outcome prevalence
                          R_prev,     # proportion of rows given a missing-data pattern
                          gamma,      # named vector of true effects (see settings)
                          mech,       # "MCAR", "MAR" or "MNAR"
                          weights = NULL) {

  gamma_x1 <- gamma[["gamma_x1"]]
  gamma_x2 <- gamma[["gamma_x2"]]
  gamma_x3 <- gamma[["gamma_x3"]]
  gamma_x4 <- gamma[["gamma_x4"]]
  gamma_x5 <- gamma[["gamma_x5"]]
  gamma_U  <- gamma[["gamma_U"]]

  # 1. Simulate correlated covariates; dichotomise x_5
  IPD <- as.data.frame(MASS::mvrnorm(n = N, mu = rep(0, 6), Sigma = sigma_covariates))
  colnames(IPD) <- c("x_1", "x_2", "x_3", "x_4", "x_5", "U")
  IPD <- IPD %>%
    mutate(x_5 = rbinom(n = N, size = 1, prob = plogis(x_5)))

  # 2. Ampute x_1 and x_3 under the chosen mechanism
  data_miss <- ampute(IPD,
                      prop     = R_prev,
                      patterns = missing_patterns,
                      mech     = mech,
                      weights  = weights)

  # 3. Calibrate the intercept gamma_0 to give the target outcome prevalence
  gamma_0 <- as.numeric(coef(glm(rbinom(N, 1, prob = Y_prev) ~
                                   offset(gamma_x1 * x_1 +
                                            gamma_x2 * x_2 +
                                            gamma_x3 * x_3 +
                                            gamma_x4 * x_4 +
                                            gamma_x5 * x_5 +
                                            gamma_U  * U),
                                 family = binomial(link = "logit"),
                                 data = IPD))[1])

  # 4. Simulate the outcome from the true (complete) covariates
  IPD$Y <- rbinom(N, size = 1,
                  prob = expit_function(gamma_0 +
                                          gamma_x1 * IPD$x_1 +
                                          gamma_x2 * IPD$x_2 +
                                          gamma_x3 * IPD$x_3 +
                                          gamma_x4 * IPD$x_4 +
                                          gamma_x5 * IPD$x_5 +
                                          gamma_U  * IPD$U))

  # 5. Keep true x_1, x_3 alongside their amputed versions
  colnames(IPD) <- c("x_1true", "x_2", "x_3true", "x_4", "x_5", "U", "Y")
  sim_data <- cbind(IPD, data_miss[["amp"]][, c("x_1", "x_3")])

  # 6. Grouping of x_4 used for reference-value imputation
  sim_data <- sim_data %>%
    mutate(x_4group = case_when(x_4 <= 0 ~ "Group 1",
                                x_4 >  0 ~ "Group 2"))

  # 7. Label missing-data patterns
  sim_data <- sim_data %>%
    mutate(pattern = case_when(!is.na(x_1) & !is.na(x_3) ~ 0,  # complete
                                is.na(x_1) & !is.na(x_3) ~ 1,  # x_1 missing
                                is.na(x_1) &  is.na(x_3) ~ 2,  # x_1 & x_3 missing
                               !is.na(x_1) &  is.na(x_3) ~ 3)) # x_3 missing

  return(sim_data)
}

## Summary of outcome prevalence and missingness (for checking)
print_data_summary <- function(sim_data) {
  print("Y Prev")
  print(table(sim_data$Y))
  print(prop.table(table(sim_data$Y)) * 100)
  print(prop.table(table(sim_data$pattern)) * 100)
  print(prop.table(table(is.na(sim_data$x_1))) * 100)
  print(prop.table(table(is.na(sim_data$x_3))) * 100)
}

################################################################################
# 3. Model development
################################################################################

####---------------------------------------
## 3.1 Complete (true) covariates
####---------------------------------------
complete_mod_function <- function(dev_data) {
  glm(Y ~ x_1true + x_2 + x_3true + x_4 + x_5,
      data = dev_data, family = binomial(link = "logit"))
}

####---------------------------------------
## 3.2 Reference-value imputation
##     Missing x_1 / x_3 replaced by the observed mean within strata of
##     x_5 and x_4group.
####---------------------------------------
ref_mod_function <- function(dev_data) {

  reference_values <- dev_data %>%
    group_by(x_5, x_4group) %>%
    summarise(mean_x1 = mean(x_1, na.rm = TRUE),
              mean_x3 = mean(x_3, na.rm = TRUE),
              .groups = "drop")

  dev_data <- dev_data %>%
    left_join(reference_values, by = c("x_5", "x_4group")) %>%
    mutate(x1_refimp = coalesce(x_1, mean_x1),
           x3_refimp = coalesce(x_3, mean_x3))

  model_ref <- glm(Y ~ x1_refimp + x_2 + x3_refimp + x_4 + x_5,
                   data = dev_data, family = binomial(link = "logit"))

  return(list(model_ref        = model_ref,
              reference_values = reference_values))
}

####---------------------------------------
## 3.3 Pattern sub-models
##     Mercaldo & Blume (2020): https://academic.oup.com/biostatistics/article/21/2/236/5092384
##     One model per missing-data pattern, using only the observed covariates.
####---------------------------------------
sub_mod_function <- function(dev_data) {

  fit_pattern <- function(formula, p) {
    glm(formula, data = dev_data %>% filter(pattern == p),
        family = binomial(link = "logit"))
  }

  return(list(
    model_allobserved = fit_pattern(Y ~ x_1 + x_2 + x_3 + x_4 + x_5, 0),
    model_x1missing   = fit_pattern(Y ~       x_2 + x_3 + x_4 + x_5, 1),
    model_x1x3missing = fit_pattern(Y ~       x_2 +       x_4 + x_5, 2),
    model_x3missing   = fit_pattern(Y ~ x_1 + x_2 +       x_4 + x_5, 3)
  ))
}

####---------------------------------------
## 3.4 XGBoost (native handling of missing values)
####---------------------------------------
xg_function <- function(dev_data) {

  xgtrain <- xgb.DMatrix(data  = as.matrix(dev_data[, c("x_1", "x_2", "x_3", "x_4", "x_5")]),
                         label = dev_data[["Y"]],
                         missing = NA)

  params <- list(
    objective   = "binary:logistic",
    eval_metric = "auc",
    max_depth   = 4,    # limits complexity
    eta         = 0.1   # learning rate
  )

  xgb.train(params = params, data = xgtrain, nrounds = 100, verbose = 1)
}

####---------------------------------------
## 3.5 Regression imputation
##     Deterministic imputation of x_1 / x_3 from the covariates observed in
##     each pattern, then a logistic model on the imputed data.
####---------------------------------------
ri_impute <- function(data, rimodel) {
  data %>%
    mutate(x_1_ri = case_when(
             is.na(x_1) & !is.na(x_3) ~ predict(rimodel[["x1_ri_model"]],   newdata = data, type = "response"),
             is.na(x_1) &  is.na(x_3) ~ predict(rimodel[["x1x3_ri_model"]], newdata = data, type = "response"),
            !is.na(x_1)               ~ x_1),
           x_3_ri = case_when(
             is.na(x_3) & !is.na(x_1) ~ predict(rimodel[["x3_ri_model"]],   newdata = data, type = "response"),
             is.na(x_3) &  is.na(x_1) ~ predict(rimodel[["x3x1_ri_model"]], newdata = data, type = "response"),
            !is.na(x_3)               ~ x_3))
}

ri_function <- function(dev_data) {

  rimodel <- list(
    x1_ri_model   = lm(x_1 ~       x_2 + x_3 + x_4 + x_5, data = dev_data),  # x_1 missing
    x1x3_ri_model = lm(x_1 ~       x_2 +       x_4 + x_5, data = dev_data),  # x_1 & x_3 missing
    x3_ri_model   = lm(x_3 ~ x_1 + x_2 +       x_4 + x_5, data = dev_data),  # x_3 missing
    x3x1_ri_model = lm(x_3 ~       x_2 +       x_4 + x_5, data = dev_data)   # x_1 & x_3 missing
  )

  rimodel$model_ri <- glm(Y ~ x_1_ri + x_2 + x_3_ri + x_4 + x_5,
                          data = ri_impute(dev_data, rimodel),
                          family = binomial(link = "logit"))

  return(rimodel)
}

####---------------------------------------
## 3.6 Fit all models
####---------------------------------------
fit_all_models <- function(dev_data) {
  # Order kept as in the original scripts
  submodel <- sub_mod_function(dev_data)
  refmodel <- ref_mod_function(dev_data)
  xgmodel  <- xg_function(dev_data)
  model    <- complete_mod_function(dev_data)
  rimodel  <- ri_function(dev_data)

  list(submodel = submodel,
       refmodel = refmodel,
       xgmodel  = xgmodel,
       rimodel  = rimodel,
       model    = model)
}

################################################################################
# 4. Predictions in validation data
################################################################################
prediction_function <- function(val_data, submodel, refmodel, xgboost_model,
                                model, rimodel) {

  ## Complete (true) covariates
  predictions_cri <- predict(model, newdata = val_data, type = "response")

  ## Reference values: development-data means within strata
  val_data <- val_data %>%
    left_join(refmodel[["reference_values"]], by = c("x_5", "x_4group")) %>%
    mutate(x1_refimp = coalesce(x_1, mean_x1),
           x3_refimp = coalesce(x_3, mean_x3))

  predictions_ref <- predict(refmodel[["model_ref"]], newdata = val_data, type = "response")

  ## Pattern sub-models: apply the sub-model matching each row's pattern
  predictions_sm <- rep(NA_real_, nrow(val_data))
  pattern_models <- c("0" = "model_allobserved",
                      "1" = "model_x1missing",
                      "2" = "model_x1x3missing",
                      "3" = "model_x3missing")

  for (p in names(pattern_models)) {
    rows <- val_data$pattern == as.numeric(p)
    if (sum(rows) > 0) {
      predictions_sm[rows] <- predict(submodel[[pattern_models[[p]]]],
                                      newdata = val_data[rows, ],
                                      type = "response")
    }
  }

  ## XGBoost
  dval <- xgb.DMatrix(data  = as.matrix(val_data[, c("x_1", "x_2", "x_3", "x_4", "x_5")]),
                      label = val_data$Y,
                      missing = NA)
  predictions_xg <- predict(xgboost_model, dval)

  ## Regression imputation using development-data imputation models
  predictions_ri <- predict(rimodel[["model_ri"]],
                            newdata = ri_impute(val_data, rimodel),
                            type = "response")

  data.frame(Y                   = val_data$Y,
             Predictions_CRI     = as.numeric(predictions_cri),
             Prediction_Ref      = as.numeric(predictions_ref),
             Prediction_SubModel = predictions_sm,
             Prediction_XG       = predictions_xg,
             Prediction_RI       = as.numeric(predictions_ri))
}

################################################################################
# 5. Predictive performance
################################################################################
## For each model: calibration intercept and slope, AUC (DeLong variance),
## Brier score and scaled Brier score.
predictive.performance.function <- function(predictions) {

  Y <- predictions$Y
  results_list <- list()

  for (col_name in colnames(predictions)[-1]) {
    Predicted_Risks <- predictions[[col_name]]

    ## Mean difference between observed outcome and predicted risk, and its
    ## mean square (note: mse is identical to the Brier score)
    bias <- mean(Y - Predicted_Risks)
    mse  <- mean((Y - Predicted_Risks)^2)
    rmse <- sqrt(mse)

    ## Brier score, and scaled relative to predicting the observed prevalence
    Brier          <- mean((Predicted_Risks - Y)^2)
    Brier_baseline <- mean((mean(Y) - Y)^2)
    Brier_scaled   <- ifelse(mean(Y) %in% c(0, 1), NA, 1 - (Brier / Brier_baseline))

    ## Calibration
    LP          <- log(Predicted_Risks / (1 - Predicted_Risks))  # linear predictor
    variance_LP <- var(LP)
    variance_PR <- var(Predicted_Risks)

    if (sum(Y) == 0 || variance_PR == 0 || variance_PR < 1e-10 || variance_LP > 8) {
      Cal_Int <- Cal_Int_var <- Cal_Slope <- Cal_Slope_var <- NA
      message("Calibration intercept and slope cannot be calculated for ", col_name)
    } else {
      Cal_Int_model <- glm(Y ~ offset(LP), family = binomial(link = "logit"))
      Cal_Int       <- coef(Cal_Int_model)[1]
      Cal_Int_var   <- vcov(Cal_Int_model)[1, 1]

      Cal_Slope_model <- glm(Y ~ LP, family = binomial(link = "logit"))
      Cal_Slope       <- coef(Cal_Slope_model)[2]
      Cal_Slope_var   <- vcov(Cal_Slope_model)[2, 2]
    }

    ## Discrimination
    if (sum(Y) == 0 || variance_PR == 0 || variance_LP > 9) {
      AUC <- AUC_var <- NA
    } else {
      AUC <- tryCatch({
        roc_obj <- roc(response = Y, predictor = as.vector(Predicted_Risks),
                       direction = "<", levels = c(0, 1))
        roc_obj$auc
      }, error = function(e) {
        message("Error in ROC calculation for ", col_name, ": ", e$message)
        NA
      })

      AUC_var <- tryCatch({
        if (!is.na(AUC)) var(roc_obj, method = "delong") else NA
      }, error = function(e) {
        message("Error in variance calculation for ", col_name, ": ", e$message)
        NA
      })
    }

    results_list[[col_name]] <- data.frame(
      Model         = col_name,
      Cal_Int       = Cal_Int,
      Cal_Int_var   = Cal_Int_var,
      Cal_Slope     = Cal_Slope,
      Cal_Slope_var = Cal_Slope_var,
      AUC           = as.numeric(AUC),
      AUC_var       = AUC_var,
      Brier         = Brier,
      Brier_scaled  = Brier_scaled,
      bias          = bias,
      mse           = mse,
      rmse          = rmse
    )
  }

  do.call(rbind, results_list)
}

################################################################################
# 6. Running the validation simulation
################################################################################

####---------------------------------------
## 6.1 One iteration: simulate validation data, predict, assess performance
####---------------------------------------
simulation_singlerun_fnc <- function(N_val, Y_prev, R_prev, gamma, mechanism,
                                     dev_models,
                                     save_predictions = TRUE,
                                     save_val_data = FALSE) {

  mdef <- mechanism_definitions[[mechanism]]

  val_data <- simulate_data(N       = N_val,
                            Y_prev  = Y_prev,
                            R_prev  = R_prev,
                            gamma   = gamma,
                            mech    = mdef$mech,
                            weights = mdef$weights)

  predictions <- prediction_function(val_data      = val_data,
                                     submodel      = dev_models$submodel,
                                     refmodel      = dev_models$refmodel,
                                     xgboost_model = dev_models$xgmodel,
                                     model         = dev_models$model,
                                     rimodel       = dev_models$rimodel)

  out <- list(target_measures = predictive.performance.function(predictions))
  if (save_predictions) out$predictions <- predictions
  if (save_val_data)    out$val_data    <- val_data
  out
}

####---------------------------------------
## 6.2 One scenario: load development models, run n_iter iterations, save
##     Returns a one-row status data frame (results are written to disk so
##     that large objects are not held in memory across scenarios).
####---------------------------------------
run_validation_scenario <- function(scenario, gamma, dev_data_dir, val_results_dir,
                                    save_predictions = TRUE,
                                    save_val_data = FALSE) {

  label <- paste0(scenario$mechanism, " | N_val = ", scenario$N_val,
                  " | Y_prev = ", scenario$Y_prev, " | R_prev = ", scenario$R_prev)

  tryCatch({
    # Development models for this mechanism and prevalence
    dev_env <- new.env()
    load(file.path(dev_data_dir, dev_filename(scenario$mechanism, scenario$Y_prev)),
         envir = dev_env)
    dev_models <- dev_env$development_dataset

    set.seed(scenario$seed)

    iterations <- vector("list", scenario$n_iter)
    for (iter in seq_len(scenario$n_iter)) {
      iterations[[iter]] <- simulation_singlerun_fnc(
        N_val            = scenario$N_val,
        Y_prev           = scenario$Y_prev,
        R_prev           = scenario$R_prev,
        gamma            = gamma,
        mechanism        = scenario$mechanism,
        dev_models       = dev_models,
        save_predictions = save_predictions,
        save_val_data    = save_val_data
      )
      if (iter %% 10 == 0) message(label, ": iteration ", iter, " complete")
    }

    simulation_results <- list(parameters = c(as.list(scenario), as.list(gamma)),
                               iterations = iterations)

    save(simulation_results, file = file.path(val_results_dir, scenario$val_file))
    message(label, ": saved ", scenario$val_file)

    data.frame(scenario_id = scenario$scenario_id, file = scenario$val_file,
               status = "success", error = NA_character_)

  }, error = function(e) {
    message(label, ": FAILED - ", conditionMessage(e))
    data.frame(scenario_id = scenario$scenario_id, file = scenario$val_file,
               status = "failed", error = conditionMessage(e))
  })
}
