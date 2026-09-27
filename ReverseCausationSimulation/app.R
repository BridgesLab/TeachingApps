# Reverse causation simulator: non-nutritive sweeteners (NNS) and BMI
# PUBHLTH430. Run with shiny::runApp() from this folder.
#
# A simulated cohort where heavier people are more likely to choose NNS. Students
# compare how four designs estimate the effect of NNS on BMI over 5 years.

library(shiny)
library(bslib)
library(ggplot2)

# University of Michigan brand colors. Maize is never used for text on white.
UM <- c(blue = "#00274C", maize = "#FFCB05", grey = "#7A8CA3",
        tappan_red = "#9A3324", arboretum_blue = "#2F65A7")

METHODS <- c("Naive cohort", "Exclude people with obesity",
             "Adjust for baseline BMI", "Simulated randomized trial")

# ---- Simulation (plain R, tested in tests/testthat) --------------------------

# Probability of NNS use at a given true baseline BMI. Centered at BMI 27 so
# overall use stays near 38-40% whatever k is.
p_nns <- function(bmi, k) plogis(qlogis(0.38) + k * (bmi - 27))

# Expected NNS use among people with and without obesity (true BMI >= 30 vs < 30)
# in the population, from p_nns() and the N(27, 5) BMI distribution. Exact, so the
# readout doesn't jitter with the sample.
nns_use_by_obesity <- function(k) {
  use_between <- function(lo, hi) {
    integrate(function(b) p_nns(b, k) * dnorm(b, 27, 5), lo, hi)$value /
      (pnorm(hi, 27, 5) - pnorm(lo, 27, 5))
  }
  with_ob <- use_between(30, Inf)
  without_ob <- use_between(-Inf, 30)
  c(with = with_ob, without = without_ob, ratio = with_ob / without_ob)
}

# Outcome model shared by the cohort and the trial
bmi_at_followup <- function(bmi, nns, true_effect, noise) {
  bmi + 0.5 + 0.05 * (bmi - 27) + true_effect * nns + noise
}

# Draw every random number once for a given n and seed. The sliders for k,
# true_effect and me_sd only transform these draws (common random numbers), so
# moving a slider shifts the estimates smoothly instead of reshuffling everyone.
draw_random <- function(n, seed) {
  set.seed(seed)
  cohort <- list(bmi = rnorm(n, 27, 5), u_nns = runif(n),
                 noise = rnorm(n, 0, 1.5), z_me = rnorm(n))
  # the RCT gets its own stream so the cohort sliders never change it
  set.seed(seed + 1)
  rct <- list(bmi = rnorm(n, 27, 5), nns = rbinom(n, 1, 0.5),
              noise = rnorm(n, 0, 1.5))
  list(cohort = cohort, rct = rct)
}

simulate_study <- function(draws, true_effect = 0, k = 0.13, me_sd = 0) {
  d <- draws$cohort
  nns <- as.integer(d$u_nns < p_nns(d$bmi, k))
  cohort <- data.frame(
    bmi_true = d$bmi,
    nns = nns,
    bmi_followup = bmi_at_followup(d$bmi, nns, true_effect, d$noise),
    bmi_measured = d$bmi + me_sd * d$z_me   # baseline BMI as the researcher records it
  )
  r <- draws$rct
  rct <- data.frame(
    bmi_true = r$bmi,
    nns = r$nns,
    bmi_followup = bmi_at_followup(r$bmi, r$nns, true_effect, r$noise)
  )
  list(cohort = cohort, rct = rct)
}

# Coefficient on nns with its 95% CI
nns_coef <- function(fit) {
  est <- coef(fit)[["nns"]]
  ci <- confint(fit, "nns", level = 0.95)
  c(estimate = est, lower = ci[1, 1], upper = ci[1, 2])
}

estimate_effects <- function(sim, true_effect) {
  cohort <- sim$cohort
  fits <- list(
    lm(bmi_followup ~ nns, data = cohort),
    lm(bmi_followup ~ nns, data = cohort[cohort$bmi_measured < 30, ]),
    lm(bmi_followup ~ nns + bmi_measured, data = cohort),
    lm(bmi_followup ~ nns, data = sim$rct)
  )
  out <- as.data.frame(do.call(rbind, lapply(fits, nns_coef)))
  out$method <- factor(METHODS, levels = METHODS)
  out$bias <- out$estimate - true_effect
  out$covers <- out$lower <= true_effect & out$upper >= true_effect
  out[, c("method", "estimate", "lower", "upper", "bias", "covers")]
}

run_simulation <- function(n = 5000, seed = 430, true_effect = 0, k = 0.13, me_sd = 0) {
  sim <- simulate_study(draw_random(n, seed), true_effect, k, me_sd)
  list(sim = sim, estimates = estimate_effects(sim, true_effect))
}

# ---- Plots --------------------------------------------------------------------

plot_estimates <- function(est, true_effect) {
  est$status <- factor(ifelse(est$covers, "CI includes true effect", "CI misses true effect"),
                       levels = c("CI includes true effect", "CI misses true effect"))
  # stable axis so students can watch estimates move; widen only if needed
  xlim <- range(c(-3, 7, est$lower, est$upper))
  ggplot(est, aes(x = estimate, y = method, colour = status, shape = status)) +
    geom_vline(xintercept = 0, colour = "grey85", linewidth = 0.6) +
    geom_vline(xintercept = true_effect, colour = UM[["blue"]], linetype = "dashed", linewidth = 0.8) +
    annotate("text", x = true_effect, y = 4.5, label = "True effect", size = 4.2,
             colour = UM[["blue"]], fontface = "bold") +
    geom_errorbar(aes(xmin = lower, xmax = upper), width = 0.18, linewidth = 1.1, orientation = "y") +
    geom_point(size = 4) +
    # label to the right of each CI so it never sits on the reference lines
    geom_text(aes(x = upper, label = sprintf("%+.2f", estimate)), hjust = -0.25, size = 4.5,
              fontface = "bold", show.legend = FALSE) +
    scale_y_discrete(limits = rev(METHODS)) +
    scale_x_continuous(limits = xlim, breaks = seq(floor(xlim[1]), ceiling(xlim[2]), 1)) +
    scale_colour_manual(values = c("CI includes true effect" = UM[["blue"]],
                                   "CI misses true effect" = UM[["tappan_red"]]), drop = FALSE) +
    scale_shape_manual(values = c("CI includes true effect" = 16,
                                  "CI misses true effect" = 17), drop = FALSE) +
    coord_cartesian(clip = "off") +
    labs(x = "Difference in 5-year BMI, NNS users − non-users (kg/m²)",
         y = NULL, colour = NULL, shape = NULL) +
    theme_classic(base_size = 14) +
    theme(legend.position = "top", legend.justification = "left",
          axis.text.y = element_text(size = 14, colour = "black"))
}

plot_baseline <- function(cohort) {
  cohort$group <- factor(ifelse(cohort$nns == 1, "NNS users", "Non-users"),
                         levels = c("NNS users", "Non-users"))
  ggplot(cohort, aes(x = bmi_true, fill = group, colour = group)) +
    geom_density(alpha = 0.35, linewidth = 0.9) +
    geom_vline(xintercept = 30, linetype = "dashed", colour = "grey30") +
    annotate("text", x = 30.4, y = Inf, label = "BMI 30\n(obesity)", hjust = 0, vjust = 1.3,
             size = 4, colour = "grey30") +
    scale_fill_manual(values = c("NNS users" = UM[["blue"]], "Non-users" = UM[["grey"]])) +
    scale_colour_manual(values = c("NNS users" = UM[["blue"]], "Non-users" = UM[["grey"]])) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +   # headroom for the BMI 30 label
    labs(x = "True baseline BMI (kg/m²)", y = "Density", fill = NULL, colour = NULL) +
    theme_classic(base_size = 14) +
    theme(legend.position = "top")
}

# ---- UI -----------------------------------------------------------------------

try_this <- tags$ol(
  tags$li("Leave the true effect at 0. Move the reverse causation slider from 0 to 0.3. What happens to the naive estimate?"),
  tags$li("Set the true effect to −1 (NNS helps). Can the naive cohort still make NNS look harmful?"),
  tags$li("Why doesn't excluding people with obesity fix the problem completely? (Look at the bottom plot.)"),
  tags$li("Raise the measurement error. What happens to the adjusted estimate? What does this mean for cohorts that rely on self-reported weight?"),
  tags$li("Which estimate stays on the true effect no matter what you do, and why?"),
  tags$li("Click ", tags$em("New sample"), " several times. The trial estimate wobbles around the true effect ",
          "and occasionally misses it. Randomization removes bias, not chance. How does the wobble change with cohort size?")
)

ui <- page_navbar(
  title = "Reverse causation: NNS & BMI",
  theme = bs_theme(version = 5, primary = UM[["blue"]], secondary = UM[["arboretum_blue"]],
                   warning = UM[["maize"]], danger = UM[["tappan_red"]]),
  fillable = FALSE,
  navbar_options = navbar_options(collapsible = TRUE, bg = UM[["blue"]], theme = "dark"),
  header = div(
    class = "alert small mb-0 rounded-0 border-0 text-center py-2",
    style = paste0("background-color:", UM[["maize"]], "; color:", UM[["blue"]], ";"),
    role = "note",
    strong("For teaching only. "),
    "All data are simulated to illustrate study design. The numbers are not estimates of the real effect of NNS."
  ),
  nav_panel(
    "Simulator",
    layout_sidebar(
      sidebar = sidebar(
        width = 340,
        p(class = "small",
          "In this simulated cohort you control whether NNS truly affects BMI and how strongly ",
          "heavier people choose NNS. Compare how four study designs estimate the effect."),
        sliderInput("true_effect", "True effect of NNS on BMI (kg/m² over 5 years)",
                    min = -2, max = 2, value = 0, step = 0.1),
        helpText(class = "mt-n2", "Negative = NNS helps; positive = NNS harms."),
        sliderInput("k", "Reverse causation: how strongly does higher BMI push people toward NNS?",
                    min = 0, max = 0.3, value = 0.13, step = 0.01),
        div(class = "mt-n2 mb-3 p-2 rounded", style = paste0("background-color:#EEF2F7; color:", UM[["blue"]], ";"),
            uiOutput("k_readout")),
        sliderInput("me_sd", "Error in measured baseline BMI (SD, kg/m²)",
                    min = 0, max = 3, value = 0, step = 0.25),
        helpText(class = "mt-n2", "For example, from self-reported weight."),
        sliderInput("n", "Cohort size", min = 500, max = 20000, value = 5000, step = 500, sep = ","),
        actionButton("resample", "New sample", icon = icon("shuffle"), class = "btn-primary w-100"),
        div(class = "small text-muted mt-2", textOutput("seed_readout"))
      ),
      card(
        card_header("How four study designs estimate the effect of NNS"),
        plotOutput("estimates_plot", height = "380px")
      ),
      accordion(
        open = FALSE,
        accordion_panel("Try this", icon = icon("lightbulb"), try_this)
      ),
      layout_columns(
        col_widths = breakpoints(sm = 12, lg = c(5, 7)),
        card(
          card_header("Baseline BMI by NNS use"),
          plotOutput("baseline_plot", height = "300px"),
          card_footer(class = "small", textOutput("baseline_caption"))
        ),
        card(
          card_header("Estimates"),
          div(class = "small", tableOutput("estimates_table")),
          card_footer(class = "small",
                      "Mean difference in follow-up BMI (kg/m²), NNS users minus non-users. ",
                      "Bias = estimate − true effect.")
        )
      )
    )
  ),
  nav_spacer(),
  nav_item(tags$span(class = "navbar-text small", "PUBHLTH430 · simulated data"))
)

# ---- Server -------------------------------------------------------------------

server <- function(input, output, session) {
  seed <- reactiveVal(430)
  observeEvent(input$resample, seed(sample.int(1e6, 1)))

  # debounce so dragging a slider doesn't queue up dozens of refits
  n <- debounce(reactive(input$n), 300)
  settings <- debounce(reactive(list(true_effect = input$true_effect, k = input$k,
                                     me_sd = input$me_sd)), 150)

  draws <- reactive(draw_random(n(), seed()))
  sim <- reactive({
    s <- settings()
    simulate_study(draws(), s$true_effect, s$k, s$me_sd)
  })
  estimates <- reactive(estimate_effects(sim(), settings()$true_effect))

  # not debounced, so it updates while the slider is dragged
  output$k_readout <- renderUI({
    u <- nns_use_by_obesity(input$k)
    tagList(
      div(strong(sprintf("People with obesity are %.1f× as likely to use NNS", u[["ratio"]]))),
      div(class = "small", sprintf("%.0f%% of people with obesity vs %.0f%% of people without",
                                   100 * u[["with"]], 100 * u[["without"]])),
      div(class = "small text-muted", sprintf("At BMI 22: %.0f%% use NNS · At BMI 32: %.0f%%",
                                              100 * p_nns(22, input$k), 100 * p_nns(32, input$k)))
    )
  })
  output$seed_readout <- renderText(sprintf("Sample seed: %d", seed()))

  output$estimates_plot <- renderPlot(plot_estimates(estimates(), settings()$true_effect), res = 96)
  output$baseline_plot <- renderPlot(plot_baseline(sim()$cohort), res = 96)

  output$baseline_caption <- renderText({
    cohort <- sim()$cohort
    sprintf("Mean baseline BMI: NNS users %.1f, non-users %.1f. Overall NNS use: %.0f%%.",
            mean(cohort$bmi_true[cohort$nns == 1]), mean(cohort$bmi_true[cohort$nns == 0]),
            100 * mean(cohort$nns))
  })

  output$estimates_table <- renderTable({
    e <- estimates()
    data.frame(
      Method = as.character(e$method),
      Estimate = sprintf("%+.2f", e$estimate),
      `95% CI` = sprintf("%+.2f, %+.2f", e$lower, e$upper),
      Bias = sprintf("%+.2f", e$bias),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, width = "100%", align = "lrrr")
}

shinyApp(ui, server)
