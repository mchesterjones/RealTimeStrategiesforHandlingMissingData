################################################################################
# app/app.R
################################################################################
# Interactive dashboard of the simulation study results.
#
# Data: AllIter.Rdata and LongFormat.Rdata from 03_summarise_results.R.
#   - Looks in app/data/ first (use this when deploying, e.g. to shinyapps.io)
#   - otherwise reads output/summary/ in the project
#
# Run from the project root with:  shiny::runApp("app")
################################################################################

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)

################################################################################
# Data
################################################################################
data_dir <- if (file.exists(file.path("data", "LongFormat.Rdata"))) {
  "data"
} else {
  file.path("..", "output", "summary")
}
load(file.path(data_dir, "AllIter.Rdata"))     # combined_df
load(file.path(data_dir, "LongFormat.Rdata"))  # summarised_df_long

## The Fully Observed model does not depend on missingness: show it in the
## lowest-missingness panel only (as in 04_figures.R)
hide_fully_observed <- function(df) {
  df %>% filter(!(Model == "Predictions_CRI" & R_prev != min(R_prev)))
}
combined_df        <- hide_fully_observed(combined_df)
summarised_df_long <- hide_fully_observed(summarised_df_long)

## Choices taken from the data, so the app follows any change to the design
## or labels in R/settings.R
present_levels <- function(x) levels(droplevels(x))
mechanism_choices  <- present_levels(summarised_df_long$Mechanism)
samplesize_choices <- present_levels(summarised_df_long$samplesize)
prevalence_choices <- present_levels(summarised_df_long$Prevalence)
missing_choices    <- present_levels(summarised_df_long$Missingness)
n_iter             <- max(summarised_df_long$n_iter)

method_levels  <- levels(summarised_df_long$Method)
method_colours <- setNames(c("#984ea3", "#ff7f00", "#4daf4a", "#377eb8", "black")[seq_along(method_levels)],
                           method_levels)

## Columns the user can compare across (the other two are fixed by selection)
compare_choices <- c("Sample size" = "samplesize",
                     "Missingness mechanism" = "Mechanism",
                     "Outcome prevalence" = "Prevalence")

## Summary statistic: median/IQR offered if 03_summarise_results.R provides it
has_median   <- all(c("MEDIAN", "Q1", "Q3") %in% names(summarised_df_long))
stat_choices <- c("Mean (2.5th-97.5th percentile)" = "mean")
if (has_median) stat_choices <- c(stat_choices, "Median (interquartile range)" = "median")

################################################################################
# Performance measures
################################################################################
measure_info <- list(
  AUC = list(
    label = "Discrimination (AUC)", ref = NULL,
    text  = "The area under the ROC curve (C-statistic) is the probability that a randomly
             chosen individual with the outcome has a higher predicted risk than one without.
             0.5 indicates no discrimination; 1 indicates perfect discrimination."),
  Brier_scaled = list(
    label = "Scaled Brier score", ref = 0,
    text  = "The Brier score relative to a model that predicts the observed prevalence for
             everyone: 1 - Brier / Brier<sub>null</sub>. Higher is better; values below 0 are
             worse than predicting the prevalence."),
  Brier = list(
    label = "Brier score", ref = NULL,
    text  = "The mean squared difference between predicted risk and the observed outcome.
             Lower is better. Its scale depends on the outcome prevalence."),
  Cal_Int = list(
    label = "Calibration in the large", ref = 0,
    text  = "The intercept of a logistic regression of the outcome with the model's linear
             predictor as an offset. 0 indicates no systematic miscalibration; positive values
             indicate risks are underestimated on average, negative values that they are
             overestimated."),
  Cal_Slope = list(
    label = "Calibration slope", ref = 1,
    text  = "The slope of a logistic regression of the outcome on the model's linear predictor.
             1 is ideal; values below 1 indicate predictions that are too extreme, values above 1
             predictions that are not extreme enough. With very few events the slope is highly
             variable, and it is treated as not estimable when the recalibration model is
             unstable."),
  bias = list(
    label = "Mean difference (observed - predicted)", ref = 0,
    text  = "The average of the observed outcome minus the predicted risk. 0 indicates predicted
             risks are correct on average; positive values indicate underestimation."),
  rmse = list(
    label = "Root mean squared error", ref = NULL,
    text  = "The square root of the mean squared difference between observed outcome and
             predicted risk (the square root of the Brier score). Lower is better.")
)
measure_choices <- setNames(names(measure_info),
                            vapply(measure_info, `[[`, "", "label"))

################################################################################
# UI
################################################################################
ui <- dashboardPage(
  dashboardHeader(title = "Real-time imputation", titleWidth = 320),

  dashboardSidebar(
    width = 320,
    sidebarMenu(
      menuItem("Aims",           tabName = "aims",      icon = icon("bullseye")),
      menuItem("Methods",        tabName = "methods",   icon = icon("flask")),
      menuItem("Study flow",     tabName = "flowchart", icon = icon("diagram-project")),
      menuItem("Results: plots", tabName = "results",   icon = icon("chart-line")),
      menuItem("Results: table", tabName = "table",     icon = icon("table"))
    )
  ),

  dashboardBody(
    withMathJax(),
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side { background-color: #f8f9fa; }
      .main-header .navbar { background-color: #2c3e50 !important; }
      .main-header .logo   { background-color: #34495e !important; }
      .skin-blue .main-sidebar { background-color: #34495e; }
      .box { border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
      .content-text { font-size: 15px; line-height: 1.6; color: #2c3e50; }
      .objective-box { background-color: #e8f4f8; border-left: 4px solid #3498db;
                       padding: 15px; margin: 15px 0; border-radius: 4px; }
      .flowchart-container { text-align: center; padding: 20px; }
      .flowchart-box { background-color: #3498db; color: white; padding: 12px; margin: 6px;
                       border-radius: 8px; display: inline-block; min-width: 160px; }
      .arrow { font-size: 24px; color: #3498db; margin: 6px; }
      table.patterns td, table.patterns th { padding: 4px 14px; text-align: center;
                                             border: 1px solid #ddd; }
    "))),

    tabItems(

      ## Aims -----------------------------------------------------------------
      tabItem(tabName = "aims",
        fluidRow(box(title = "Aim", status = "primary", solidHeader = TRUE, width = 12,
          div(class = "content-text",
            div(class = "objective-box",
                p("To compare the performance of real-time approaches for handling missing
                   predictor values when a clinical prediction model is used in practice,
                   across missingness mechanisms, proportions of missing data, validation
                   sample sizes and outcome prevalences.")),
            h4("Approaches compared"),
            tags$ul(
              tags$li(strong("Regression imputation:"), " missing predictors predicted from the observed predictors"),
              tags$li(strong("Reference values:"), " missing predictors replaced by subgroup means"),
              tags$li(strong("Pattern sub-models:"), " a separate model for each missing-data pattern"),
              tags$li(strong("XGBoost:"), " gradient-boosted trees with built-in handling of missing values"),
              tags$li(strong("Fully observed:"), " benchmark model using the true (complete) predictor values")
            )
          )
        ))
      ),

      ## Methods --------------------------------------------------------------
      tabItem(tabName = "methods",
        fluidRow(box(title = "Methods", status = "primary", solidHeader = TRUE, width = 12,
          div(class = "content-text",

            h3("Data-generating mechanism"),
            p("Five predictors \\(X_1, \\dots, X_5\\) and an unmeasured variable \\(U\\) were
               drawn from a multivariate normal distribution with mean 0 and covariance"),
            p("$$\\Sigma = \\begin{bmatrix}
                 1 & 0.2 & 0.2 & 0.7 & 0.7 & 0 \\\\
                 0.2 & 1 & 0.2 & 0.2 & 0.2 & 0 \\\\
                 0.2 & 0.2 & 1 & 0.2 & 0.2 & 0 \\\\
                 0.7 & 0.2 & 0.2 & 1 & 0.5 & 0 \\\\
                 0.7 & 0.2 & 0.2 & 0.5 & 1 & 0 \\\\
                 0 & 0 & 0 & 0 & 0 & 1
               \\end{bmatrix}$$"),
            p("\\(X_5\\) was dichotomised: \\(X_5 \\sim \\text{Bernoulli}(\\text{expit}(Z_5))\\)."),
            p("The binary outcome was generated as"),
            p("$$P(Y = 1) = \\text{expit}(\\beta_0 + 0.5X_1 + 0.5X_2 + 0.5X_3 + 0.5X_4 + 0.5X_5 + 0.5U)$$"),
            p(paste0("with \\(\\beta_0\\) chosen to give an outcome prevalence of ",
                     paste(prevalence_choices, collapse = " or "), ".")),

            h3("Missing data"),
            p("Values of \\(X_1\\) and \\(X_3\\) were removed using ", code("mice::ampute()"),
              ". Rows selected to have missing data were assigned one of three patterns with
               equal probability:"),
            tags$table(class = "patterns",
              tags$tr(tags$th("Pattern"), tags$th("\\(X_1\\)"), tags$th("\\(X_3\\)")),
              tags$tr(tags$td("1"), tags$td("missing"),  tags$td("observed")),
              tags$tr(tags$td("2"), tags$td("missing"),  tags$td("missing")),
              tags$tr(tags$td("3"), tags$td("observed"), tags$td("missing"))
            ),
            br(),
            p("The probability that a row had missing data depended on a weighted sum of the
               variables, which differed by mechanism:"),
            tags$ul(
              tags$li(strong("MCAR:"), " unrelated to any variable"),
              tags$li(strong("MAR:"), " depends on the observed \\(X_2\\)"),
              tags$li(strong("MNAR:"), " depends on the missing value(s) themselves, \\(X_2\\) and the unmeasured \\(U\\)"),
              tags$li(strong("MNAR without U:"), " depends on the missing value(s) themselves and \\(X_2\\)")
            ),
            p(paste0("25% of development rows had missing data; in validation datasets ",
                     paste(missing_choices, collapse = ", "), " of rows had missing data.")),

            h3("Development"),
            p("For each mechanism and outcome prevalence, one development dataset of
               \\(N = 100{,}000\\) was generated and the following models fitted:"),
            tags$ul(
              tags$li(strong("Regression imputation:"), " linear models predicting \\(X_1\\) and/or \\(X_3\\)
                      from the predictors observed in each pattern; a logistic model was then fitted
                      to the imputed data."),
              tags$li(strong("Reference values:"), " missing \\(X_1\\) and \\(X_3\\) replaced by their
                      development-data means within strata of \\(X_5\\) and \\(X_4\\) (\\(\\le 0\\) vs \\(> 0\\));
                      a logistic model was fitted to the imputed data."),
              tags$li(strong("Pattern sub-models:"), " a separate logistic model for each missing-data
                      pattern (including complete cases), using only the predictors observed in that
                      pattern (Mercaldo & Blume, 2020)."),
              tags$li(strong("XGBoost:"), " binary logistic objective, maximum depth 4, learning rate 0.1,
                      100 boosting rounds, with missing values handled natively."),
              tags$li(strong("Fully observed:"), " logistic model using the true values of \\(X_1\\) and \\(X_3\\).")
            ),

            h3("Validation"),
            p(paste0("For every combination of mechanism, validation sample size (",
                     paste(samplesize_choices, collapse = ", "), "), outcome prevalence and
                     proportion of rows with missing data, ", n_iter, " validation datasets were
                     generated with the same mechanism as development. Each model was applied and
                     its performance assessed by calibration in the large, calibration slope, AUC
                     and the (scaled) Brier score. Results are summarised across iterations."))
          )
        ))
      ),

      ## Flow chart -----------------------------------------------------------
      tabItem(tabName = "flowchart",
        fluidRow(box(title = "Study flow", status = "primary", solidHeader = TRUE, width = 12,
          div(class = "flowchart-container",
            div(class = "flowchart-box", "Generate development data (N = 100,000)"),
            div(class = "arrow", "↓"),
            div(class = "flowchart-box", "Remove values of X1 and X3 (25% of rows)"),
            div(
              div(class = "flowchart-box", style = "background-color: #e74c3c;", "MCAR"),
              div(class = "flowchart-box", style = "background-color: #f39c12;", "MAR"),
              div(class = "flowchart-box", style = "background-color: #9b59b6;", "MNAR"),
              div(class = "flowchart-box", style = "background-color: #8e44ad;", "MNAR without U")
            ),
            div(class = "arrow", "↓"),
            div(class = "flowchart-box", "Fit models"),
            div(
              div(class = "flowchart-box", style = "background-color: #984ea3;", "Regression imputation"),
              div(class = "flowchart-box", style = "background-color: #ff7f00;", "Reference values"),
              div(class = "flowchart-box", style = "background-color: #4daf4a;", "Pattern sub-models"),
              div(class = "flowchart-box", style = "background-color: #377eb8;", "XGBoost"),
              div(class = "flowchart-box", style = "background-color: black;",   "Fully observed")
            ),
            div(class = "arrow", "↓"),
            div(class = "flowchart-box", paste0("Generate validation data (", n_iter, " iterations per scenario)")),
            fluidRow(
              column(4, h5("Sample size"),        tags$ul(lapply(samplesize_choices, tags$li))),
              column(4, h5("Rows with missing data"), tags$ul(lapply(missing_choices, tags$li))),
              column(4, h5("Outcome prevalence"), tags$ul(lapply(prevalence_choices, tags$li)))
            ),
            div(class = "arrow", "↓"),
            div(class = "flowchart-box", "Assess predictive performance"),
            div(class = "arrow", "↓"),
            div(class = "flowchart-box", style = "background-color: #2c3e50;", "Compare methods")
          )
        ))
      ),

      ## Results: plots -------------------------------------------------------
      tabItem(tabName = "results",
        fluidRow(
          box(title = "Controls", status = "primary", solidHeader = TRUE, width = 3,
            selectInput("measure", "Performance measure", choices = measure_choices),
            radioButtons("compare", "Compare across (columns)", choices = compare_choices),
            conditionalPanel("input.compare != 'Mechanism'",
              selectInput("mechanism", "Missingness mechanism", choices = mechanism_choices)),
            conditionalPanel("input.compare != 'samplesize'",
              selectInput("samplesize", "Validation sample size", choices = samplesize_choices,
                          selected = tail(samplesize_choices, 1))),
            conditionalPanel("input.compare != 'Prevalence'",
              selectInput("prevalence", "Outcome prevalence", choices = prevalence_choices,
                          selected = tail(prevalence_choices, 1))),
            hr(),
            radioButtons("stat", "Summary", choices = stat_choices),
            checkboxInput("show_iter", "Show individual iterations", value = TRUE)
          ),
          box(title = "Performance", status = "primary", solidHeader = TRUE, width = 9,
            plotOutput("results_plot", height = "600px"),
            p(em("Each panel row is a proportion of rows with missing data. Crosses are individual
                  iterations. The fully observed model is shown in the top row only, as it does
                  not depend on missingness."))
          )
        ),
        fluidRow(
          box(title = "Interpretation", status = "info", solidHeader = TRUE, width = 12,
              uiOutput("measure_text"))
        )
      ),

      ## Results: table -------------------------------------------------------
      tabItem(tabName = "table",
        fluidRow(
          box(title = "Summary across iterations", status = "primary", solidHeader = TRUE, width = 12,
            p("Uses the selections on the ", strong("Results: plots"), " tab. MCSE: Monte Carlo
               standard error of the mean. Not estimable: iterations in which the measure could not
               be calculated."),
            DTOutput("results_table")
          )
        )
      )
    )
  )
)

################################################################################
# Server
################################################################################
server <- function(input, output, session) {

  ## Filter to the selected scenario(s): the compared variable varies, the
  ## other two are fixed by the selections
  apply_selection <- function(df) {
    if (input$compare != "Mechanism")  df <- df %>% filter(Mechanism  == input$mechanism)
    if (input$compare != "samplesize") df <- df %>% filter(samplesize == input$samplesize)
    if (input$compare != "Prevalence") df <- df %>% filter(Prevalence == input$prevalence)
    df
  }

  summary_selected <- reactive({
    summarised_df_long %>%
      filter(Measure == input$measure) %>%
      apply_selection()
  })

  iterations_selected <- reactive({
    combined_df %>% apply_selection()
  })

  output$results_plot <- renderPlot({
    m <- measure_info[[input$measure]]
    s <- summary_selected()
    validate(need(nrow(s) > 0, "No results for this selection."))

    if (input$stat == "median") {
      s <- s %>% mutate(point = MEDIAN, lower = Q1, upper = Q3)
    } else {
      s <- s %>% mutate(point = AVG, lower = LCI, upper = UCI)
    }

    p <- ggplot(s, aes(x = point, y = Method, colour = Method))

    if (!is.null(m$ref)) {
      p <- p + geom_vline(xintercept = m$ref, linetype = "dashed", colour = "grey40")
    }
    if (input$show_iter) {
      p <- p + geom_point(data = iterations_selected(),
                          aes(x = .data[[input$measure]], y = Method),
                          shape = 4, alpha = 0.35, na.rm = TRUE,
                          position = position_jitter(width = 0, height = 0.25, seed = 1))
    }

    p +
      geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.6, linewidth = 0.8, na.rm = TRUE) +
      geom_point(size = 3, na.rm = TRUE) +
      facet_grid(rows = vars(Missingness), cols = vars(.data[[input$compare]]),
                 switch = "y", scales = "fixed") +
      scale_colour_manual(values = method_colours, drop = FALSE) +
      labs(x = m$label, y = "Proportion of rows with missing data",
           colour = names(stat_choices)[stat_choices == input$stat]) +
      theme_minimal(base_size = 14) +
      theme(legend.position  = "bottom",
            legend.direction = "vertical",
            strip.placement  = "outside",
            strip.text       = element_text(size = 13, face = "bold"),
            axis.text.y      = element_blank(),
            axis.ticks.y     = element_blank(),
            panel.spacing.x  = unit(1.2, "lines"))
  })

  output$measure_text <- renderUI({
    m <- measure_info[[input$measure]]
    HTML(paste0("<b>", m$label, "</b><br>", m$text))
  })

  output$results_table <- renderDT({
    s <- summary_selected() %>%
      arrange(.data[[input$compare]], Missingness, Method)

    tab <- s %>%
      transmute(!!names(compare_choices)[compare_choices == input$compare] := .data[[input$compare]],
                `Rows missing` = Missingness,
                Method,
                Mean = AVG, `2.5th` = LCI, `97.5th` = UCI)
    if (has_median) {
      tab <- tab %>% mutate(Median = s$MEDIAN, Q1 = s$Q1, Q3 = s$Q3)
    }
    if ("MCSE" %in% names(s)) tab <- tab %>% mutate(MCSE = s$MCSE)
    tab <- tab %>% mutate(`Not estimable` = s$NACount, Iterations = s$n_iter)

    num_cols <- intersect(c("Mean", "2.5th", "97.5th", "Median", "Q1", "Q3", "MCSE"), names(tab))
    datatable(tab, rownames = FALSE, extensions = "Buttons",
              caption = measure_info[[input$measure]]$label,
              options = list(pageLength = 25, dom = "Bfrtip", buttons = c("copy", "csv"))) %>%
      formatRound(num_cols, digits = 4)
  })
}

shinyApp(ui, server)
