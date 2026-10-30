################################################################################
# 04_figures.R
################################################################################
# Aim: Plot predictive performance of each method for every scenario.
#
# Requires: output from 03_summarise_results.R
#
# Output (in output/figures/results/):
#   <group>_N<N_val>_Yprev<Y_prev>_patchwork.pdf/.svg
#       one combined figure per group of mechanisms (figure_groups) x sample
#       size x outcome prevalence; one row per mechanism, one column per
#       measure (plot_measures)
#   <mechanism>_<N_val>_<measure>.svg
#       individual panels for the main scenario (main_scenario)
#
# Each panel shows the mean across iterations (point), the 2.5th-97.5th
# percentiles across iterations (bar) and each iteration's estimate (crosses).
# The Fully Observed model does not depend on missingness, so it is shown in
# the lowest-missingness panel only.
#
# Run from the project root. Settings (labels, colours, axis limits) are in
# R/settings.R.
################################################################################

source(file.path("R", "settings.R"))
library(dplyr)
library(ggplot2)
library(patchwork)
library(svglite)  # for .svg output

dir.create(results_fig_dir, showWarnings = FALSE, recursive = TRUE)

load(file.path(summary_dir, "AllIter.Rdata"))     # combined_df
load(file.path(summary_dir, "LongFormat.Rdata"))  # summarised_df_long

################################################################################
# 1. Show Fully Observed model in the lowest-missingness panel only
################################################################################
hide_fully_observed <- function(df) {
  df %>% filter(!(Model == "Predictions_CRI" & R_prev != min(R_prev)))
}
combined_df        <- hide_fully_observed(combined_df)
summarised_df_long <- hide_fully_observed(summarised_df_long)

################################################################################
# 2. Plotting functions
################################################################################

## One panel: one mechanism x one measure, faceted by missingness
make_measure_plot <- function(summary_sub, iter_sub, m, fixed_axis) {

  p <- ggplot(summary_sub %>% filter(Measure == m$column),
              aes(x = AVG, y = Method, colour = Method)) +
    geom_point(size = 3) +
    geom_errorbar(aes(xmin = LCI, xmax = UCI), width = 0.8) +
    geom_point(data = iter_sub,
               aes(x = .data[[m$column]], y = Method),
               shape = 4, alpha = 0.5,
               position = position_jitter(width = 0.0001, seed = 1)) +
    labs(x = m$title,
         y = "Proportion of rows missing",
         colour = "Method (mean, 2.5th-97.5th percentile)") +
    facet_grid(Missingness ~ ., scales = "fixed", switch = "both") +
    scale_colour_manual(values = method_colours, drop = FALSE) +
    theme_minimal() +
    theme(legend.position  = "right",
          strip.text       = element_text(size = 14),
          strip.placement  = "outside",
          strip.background = element_blank(),
          axis.title.x     = element_text(size = 16, face = "bold", margin = margin(t = 10)),
          axis.title.y     = element_text(size = 16, face = "bold", margin = margin(r = 10)),
          plot.title       = element_text(size = 16, face = "bold"),
          axis.text.x      = element_text(size = 12),
          axis.text.y      = element_blank(),
          axis.ticks.y     = element_blank())

  if (!is.null(m$ref)) {
    p <- p + geom_vline(xintercept = m$ref, linetype = "dashed", colour = "grey30")
  }

  ## Fixed axis for the main scenario. coord_cartesian() zooms without
  ## dropping data, so bars extending past the limits are clipped rather than
  ## removed.
  if (fixed_axis) {
    p <- p +
      scale_x_continuous(breaks = seq(floor(m$xlim[1] / m$by) * m$by,
                                      ceiling(m$xlim[2] / m$by) * m$by,
                                      by = m$by)) +
      coord_cartesian(xlim = m$xlim)
  }

  p
}

## Combined figure: rows = mechanisms in the group, columns = measures
make_patchwork <- function(plots, mechanisms, title) {

  n_measures <- length(plot_measures)

  ## Row label on the third panel of each row (as in the original figures)
  tags <- unlist(lapply(mechanisms, function(mech) {
    t <- rep("", n_measures)
    t[min(3, n_measures)] <- mechanism_labels[[mech]]
    t
  }))

  wrap_plots(plots, ncol = n_measures) +
    plot_layout(guides = "collect", axes = "collect_y") +
    plot_annotation(
      title      = title,
      tag_levels = list(tags),
      theme = theme(plot.title        = element_text(size = 18, face = "bold", hjust = 0.5),
                    plot.tag          = element_text(size = 20, face = "bold", angle = 90, vjust = 0.5),
                    plot.tag.position = "left")
    ) &
    theme(legend.position = "bottom",
          axis.title.x    = element_text(size = 12),
          axis.title.y    = element_text(size = 12),
          plot.title      = element_text(size = 12))
}

################################################################################
# 3. Create figures for every sample size and prevalence
################################################################################
scenarios <- summarised_df_long %>% distinct(N_val, Y_prev, samplesize, Prevalence)

for (k in seq_len(nrow(scenarios))) {

  sc <- scenarios[k, ]
  is_main <- sc$N_val == main_scenario$N_val && sc$Y_prev == main_scenario$Y_prev
  n_label <- format(sc$N_val, scientific = FALSE)

  summary_sc <- summarised_df_long %>% filter(N_val == sc$N_val, Y_prev == sc$Y_prev)
  iter_sc    <- combined_df        %>% filter(N_val == sc$N_val, Y_prev == sc$Y_prev)

  for (group_name in names(figure_groups)) {

    mechanisms <- intersect(figure_groups[[group_name]], unique(summary_sc$mechanism))
    if (length(mechanisms) == 0) next

    plots <- list()
    for (mech in mechanisms) {
      for (measure_name in names(plot_measures)) {

        p <- make_measure_plot(summary_sub = summary_sc %>% filter(mechanism == mech),
                               iter_sub    = iter_sc    %>% filter(mechanism == mech),
                               m           = plot_measures[[measure_name]],
                               fixed_axis  = is_main)
        plots[[paste(mech, measure_name, sep = "_")]] <- p

        if (is_main) {
          ggsave(file.path(results_fig_dir,
                           paste0(mech, "_", n_label, "_", measure_name, ".svg")),
                 plot = p, width = 8, height = 6, dpi = 300)
        }
      }
    }

    fig <- make_patchwork(plots, mechanisms,
                          title = paste0("Model Performance Across Missingness Mechanisms (",
                                         sc$samplesize, ", outcome prevalence ", sc$Prevalence, ")"))

    file_stub <- file.path(results_fig_dir,
                           paste0(group_name, "_N", n_label, "_Yprev", sc$Y_prev, "_patchwork"))
    ggsave(paste0(file_stub, ".pdf"), plot = fig, width = 14, height = 5 * length(mechanisms))
    ggsave(paste0(file_stub, ".svg"), plot = fig, width = 14, height = 5 * length(mechanisms))
    message("Saved ", basename(file_stub))
  }
}
