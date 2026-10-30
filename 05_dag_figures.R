################################################################################
# 05_dag_figures.R
#
# DAGs of the missing-data mechanisms (MCAR, MAR, MNAR without U, MNAR with U)
# for each missing-data pattern, plus a landscape grid of all of them.
# Reads the patterns, ampute() weights and correlation matrix from the
# settings, so no simulation results are needed: can be run on its own.
#
# Output: output/figures/dags/
################################################################################

library(ggplot2)
library(patchwork)

source(file.path("R", "settings.R"))
## missing_patterns / mechanism_definitions / sigma_covariates may live in
## functions.R rather than settings.R; source it too if they are not found.
if (!all(c("missing_patterns", "mechanism_definitions", "sigma_covariates") %in% ls()))
  source(file.path("R", "functions.R"))
source(file.path("R", "dag_figures.R"))

dag_dir <- file.path("output", "figures", "dags")

## One figure per mechanism x pattern
save_all_dags(dag_dir, device = c("pdf", "png"))

## Landscape grid (A4): rows MCAR, MAR, MNAR without U, MNAR with U;
## columns = patterns
dag_grid <- plot_dag_grid()
for (dev in c("pdf", "png")) {
  ggsave(file.path(dag_dir, paste0("DAG_grid_landscape.", dev)), dag_grid,
         width = 11.69, height = 8.27, dpi = 300, bg = "white")
}

message("DAG figures saved to: ", normalizePath(dag_dir))
