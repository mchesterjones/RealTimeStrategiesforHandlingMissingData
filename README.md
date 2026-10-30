# RealTimeStrategiesforHandlingMissingData
This describes the simulation study followed by applied example 
# Simulation study: multiple missing covariates at implementation

Code for a simulation study comparing methods for handling missing predictor
values (x_1, x_3) when applying a clinical prediction model, under MCAR, MAR
and MNAR missingness.

Methods compared: regression imputation, reference values, pattern
sub-models, XGBoost (native missing-value handling), and a model using the
fully observed covariates as a benchmark.

## Running

Open the project in RStudio via the `.Rproj` file (or otherwise set the
working directory to the project root), then:

```r
source("run_all.R")
```

All settings (scenarios, iterations, seeds, number of cores, figure options)
are in `R/settings.R`. All output is written to `output/`.

| Script | Purpose | Output |
|---|---|---|
| `01_generate_development_data.R` | Simulate development data and fit models | `output/development_datasets/`, missing-data pattern plots |
| `02_run_validation.R` | Validation simulation, run in parallel; resumes if interrupted | `output/validation_results/`, logs |
| `03_summarise_results.R` | Combine and summarise performance across iterations | `output/summary/` |
| `04_figures.R` | Figures | `output/figures/results/` |

Shared code: `R/settings.R` (settings) and `R/functions.R` (data-generating
mechanism, models, performance measures).

## Requirements

R packages: dplyr, tidyr, mice, xgboost, pROC, MASS, foreach, doParallel,
ggplot2, patchwork, svglite. Package versions used are recorded in
`output/logs/sessionInfo.txt` after a run.

## Design

| Factor | Levels |
|---|---|
| Missingness mechanism | MCAR, MAR, MNAR with U, MNAR without U |
| Validation sample size | 500, 10,000, 100,000 |
| Outcome prevalence | 10%, 1% |
| Proportion of rows with missing data | 25%, 50%, 75% |

200 iterations per scenario; development sample size 100,000.
