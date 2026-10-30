################################################################################
# DAGs of the missing-data mechanisms, built from the simulation settings
#
# Draws the DAGs straight from `missing_patterns`, `mechanism_definitions` (the
# ampute() weights) and `sigma_covariates`, so the figures always match the
# simulation code. Uses ggplot2 only (no extra packages).
#
#   - Black arrows: data-generating model for Y, and X -> X* (observed version)
#   - Red arrows:   variables with a non-zero ampute() weight -> R
#   - Grey dashed lines: covariate correlations >= `cor_threshold`
#     (correlations below the threshold are listed in the caption)
################################################################################

library(ggplot2)

## Node layout (matches the hand-drawn DAGs)
dag_nodes <- data.frame(
  name  = c("x_2", "x_1", "x_1star", "R", "x_3star", "x_3", "U", "Y", "x_4", "x_5"),
  label = c("X2", "X1", "X1*", "R", "X3*", "X3", "U", "Y", "X4", "X5"),
  x     = c(2, 2, 2, 0, 2, 2, 2, 4.5, 6.5, 6.5),
  y     = c(8, 6, 4, 3, 2, 0, -2, 3, 4.5, 1.5)
)

covariate_names <- c("x_1", "x_2", "x_3", "x_4", "x_5", "U")  # column order of
                                                              # weights / patterns / sigma

## Edge list for one mechanism and one missing-data pattern ---------------------
dag_edges <- function(mechanism, pattern, cor_threshold = 0.5) {

  weights   <- mechanism_definitions[[mechanism]]$weights
  is_missing <- missing_patterns[pattern, ] == 0
  names(is_missing) <- covariate_names

  # Outcome model: every covariate (incl. U) -> Y
  edges <- data.frame(from = covariate_names, to = "Y", type = "outcome")

  # True value -> observed version, for the two incomplete covariates
  edges <- rbind(edges,
                 data.frame(from = c("x_1", "x_3"), to = c("x_1star", "x_3star"),
                            type = "measurement"))

  # R -> observed version, only for variables made missing in this pattern
  amputed <- intersect(c("x_1", "x_3"), covariate_names[is_missing])
  if (length(amputed) > 0)
    edges <- rbind(edges, data.frame(from = "R", to = paste0(amputed, "star"),
                                     type = "measurement"))

  # Missingness model: non-zero ampute() weights -> R (none under MCAR)
  if (!is.null(weights)) {
    drivers <- covariate_names[weights[pattern, ] != 0]
    if (length(drivers) > 0)
      edges <- rbind(edges, data.frame(from = drivers, to = "R", type = "missingness"))
  }

  # Correlations between covariates (undirected)
  idx <- which(upper.tri(sigma_covariates) & sigma_covariates >= cor_threshold,
               arr.ind = TRUE)
  if (nrow(idx) > 0)
    edges <- rbind(edges, data.frame(from = covariate_names[idx[, 1]],
                                     to   = covariate_names[idx[, 2]],
                                     type = "correlation"))
  edges
}

## Shorten an edge so it starts/ends at the node circle, not its centre --------
trim_edges <- function(edges, radius = 0.45) {
  from <- dag_nodes[match(edges$from, dag_nodes$name), c("x", "y")]
  to   <- dag_nodes[match(edges$to,   dag_nodes$name), c("x", "y")]
  len  <- sqrt((to$x - from$x)^2 + (to$y - from$y)^2)
  ux <- (to$x - from$x) / len; uy <- (to$y - from$y) / len
  cbind(edges,
        x = from$x + radius * ux, y = from$y + radius * uy,
        xend = to$x - radius * ux, yend = to$y - radius * uy)
}

## Caption text describing correlations below the threshold -------------------
correlation_caption <- function(cor_threshold) {
  s <- sigma_covariates
  dimnames(s) <- list(covariate_names, covariate_names)
  low <- which(upper.tri(s) & s > 0 & s < cor_threshold, arr.ind = TRUE)
  shown <- which(upper.tri(s) & s >= cor_threshold, arr.ind = TRUE)
  fmt <- function(i) paste0(toupper(sub("_", "", rownames(s)[i[, 1]])), "-",
                            toupper(sub("_", "", colnames(s)[i[, 2]])),
                            " (", s[i], ")")
  txt <- character(0)
  if (nrow(shown) > 0) txt <- c(txt, paste("Dashed:", paste(fmt(shown), collapse = ", ")))
  if (nrow(low) > 0) {
    vals <- unique(s[low])
    txt <- c(txt, if (length(vals) == 1)
      paste0("All other pairs of X1-X5 correlated ", vals, " (not drawn); U independent")
      else paste("Not drawn:", paste(fmt(low), collapse = ", ")))
  }
  paste(txt, collapse = "\n")
}

## Draw one DAG ---------------------------------------------------------------
plot_missingness_dag <- function(mechanism, pattern,
                                 cor_threshold = 0.5,
                                 show_correlations = TRUE,
                                 title = NULL) {

  edges <- dag_edges(mechanism, pattern, cor_threshold)
  if (!show_correlations) edges <- edges[edges$type != "correlation", ]
  edges <- trim_edges(edges)

  # R label shows which pattern it indicates
  nodes <- dag_nodes
  nodes$label[nodes$name == "R"] <- paste0("R", pattern)

  directed <- edges[edges$type != "correlation", ]
  corr     <- edges[edges$type == "correlation", ]
  arrow_head <- arrow(length = unit(0.12, "inches"), type = "closed")

  if (is.null(title)) {
    missing_vars <- c("X1", "X3")[missing_patterns[pattern, c(1, 3)] == 0]
    title <- paste0(mechanism, ": pattern ", pattern, " (",
                    paste(missing_vars, collapse = " & "), " missing)")
  }

  show_corr <- show_correlations && nrow(corr) > 0

  p <- ggplot() +
    geom_segment(data = directed,
                 aes(x = x, y = y, xend = xend, yend = yend, colour = type),
                 arrow = arrow_head, linewidth = 0.6) +
    (if (show_corr)
       geom_curve(data = corr, aes(x = x, y = y, xend = xend, yend = yend),
                  curvature = -0.25, linetype = "dashed", colour = "grey50",
                  linewidth = 0.5)) +
    geom_point(data = nodes, aes(x, y), shape = 21, size = 16,
               fill = "white", colour = "black") +
    geom_text(data = nodes, aes(x, y, label = label), size = 5) +
    scale_colour_manual(values = c(outcome = "black", measurement = "black",
                                   missingness = "red"), guide = "none") +
    coord_equal(xlim = c(-0.6, 7.1), ylim = c(-2.6, 8.6)) +
    labs(title = title,
         caption = if (show_corr) correlation_caption(cor_threshold)) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 12),
          plot.caption = element_text(hjust = 0, size = 8, colour = "grey30"))
  p
}

## All mechanisms x patterns ---------------------------------------------------
## Saves one figure per mechanism/pattern, e.g. output/figures/dags/DAG_MAR_pattern2.pdf
save_all_dags <- function(out_dir = file.path("output", "figures", "dags"),
                          device = c("pdf", "svg", "png"), ...) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  for (mechanism in names(mechanism_definitions)) {
    for (pattern in seq_len(nrow(missing_patterns))) {
      p <- plot_missingness_dag(mechanism, pattern, ...)
      for (dev in device) {
        ggsave(file.path(out_dir, paste0("DAG_", mechanism, "_pattern", pattern, ".", dev)),
               p, width = 5.5, height = 7.5, dpi = 300)
      }
    }
  }
  invisible(out_dir)
}
