################################################################################
# run_all.R
################################################################################
# Runs the full simulation study from start to finish.
#
#   1. 01_generate_development_data.R - development data and models
#   2. 02_run_validation.R            - validation simulation
#   3. 03_summarise_results.R         - combine and summarise results
#   4. 04_figures.R                   - figures
#
# Run from the project root (e.g. open the .Rproj file in RStudio, then
# source("run_all.R")). All settings are in R/settings.R.
################################################################################

source("01_generate_development_data.R") ## Creates development models 
source("02_run_validation.R") ## Runs validation of models 200 times per scenario
source("03_summarise_results.R") ## Summarises results for paper
source("04_figures.R") ## Creates figures for paper
source("05_DAGS.R") # Note: can comment out if you don't wnat to keep creating DAG figures

## Record package versions for reproducibility
dir.create(file.path("output", "logs"), showWarnings = FALSE, recursive = TRUE)
writeLines(capture.output(sessionInfo()),
           file.path("output", "logs", "sessionInfo.txt"))
