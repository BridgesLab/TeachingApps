# LDL-C diet response: why people differ (NUTR630 lipid unit)
# Enter a diet change, set the person's knobs, and see the predicted LDL-C
# change for the average person and for this person, with the spread of
# outcomes across people and from coefficient uncertainty.
# Run with shiny::runApp() from this folder. Files in R/ are sourced automatically.

library(shiny)
library(bslib)
library(ggplot2)

params <- load_params("data/parameters.csv")
P <- param_vector(params)
diets <- load_diets("data/diets.csv")
foods <- load_foods("data/foods.csv")
TYPICAL <- diet_from_row(diets, "typical_us")
START_AFTER <- "keto_eggs_butter"

ui <- page_navbar(
  title = "LDL-C diet response",
  window_title = "LDL-C and diet: why people differ",
  theme = bs_theme(version = 5, primary = UM[["blue"]], secondary = UM[["arboretum_blue"]],
                   warning = UM[["maize"]], danger = UM[["tappan_red"]]),
  fillable = FALSE,
  navbar_options = navbar_options(collapsible = TRUE, bg = UM[["blue"]], theme = "dark"),
  header = tagList(
    tags$style(HTML(APP_CSS)),
    div(class = "alert small mb-0 rounded-0 border-0 text-center py-2 avg-panel", role = "note",
        strong("Not for clinical use. "),
        "An illustrative teaching model built from linear regression coefficients. ",
        "It should not be used to predict any real person's lipid response.")
  ),
  sidebar = sidebar(
    width = 360, open = list(desktop = "open", mobile = "closed"),
    numericInput("ldl0", "Baseline LDL-C (mg/dL)", 95, min = 30, max = 400),
    radioButtons("mode", "Average-person math", selected = "full",
                 choices = c("Full model (all fats, cholesterol, fiber, plant sterols)" = "full",
                             "Simple (SFA + cholesterol only, as on the class cards)" = "card")),
    checkboxInput("predict_first", "Predict first (hide answers until I guess)", FALSE),
    tags$h6("This person's knobs (one per decoder site)", class = "fw-bold mt-2 mb-0"),
    knob_panels(P),
    actionButton("reset_knobs", "Reset knobs to the average person", class = "btn-outline-secondary btn-sm")
  ),

  nav_panel(
    "Predict", value = "predict",
    conditionalPanel(
      "!output.show_answers",
      card(
        class = "border-warning",
        card_header("Predict first"),
        p("Use the average-person math. What will LDL-C be after the diet change?"),
        layout_columns(
          col_widths = breakpoints(sm = 12, md = c(4, 4, 4)),
          numericInput("guess_avg", "Average person (mg/dL)", NA),
          numericInput("guess_person", "This person (mg/dL)", NA),
          div(class = "pt-md-4", actionButton("reveal", "Reveal", class = "btn-primary"))
        )
      )
    ),
    # Answers are rendered (not just hidden) so they are never stale after Reveal
    uiOutput("headline"),
    uiOutput("warnings"),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(7, 5)),
      uiOutput("answer_cards"),
      diet_card(TYPICAL, diet_from_row(diets, START_AFTER), foods, diets, START_AFTER)
    )
  ),

  nav_panel(
    "Fix it", value = "fix",
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(4, 8)),
      fix_controls(),
      div(
        card(card_header("LDL-C before, after the diet change, and with the fixes"),
             plotOutput("fix_plot", height = "340px")),
        card(card_header("Each fix on its own (change in LDL-C vs the diet change alone)"),
             uiOutput("fix_table"))
      )
    )
  ),

  nav_panel("Sources and assumptions", value = "sources", sources_panel(params)),
  nav_spacer(),
  nav_item(tags$span(class = "navbar-text small", "NUTR630 · illustrative model"))
)

server <- function(input, output, session) {

  num <- function(x, default = 0) if (is.null(x) || is.na(x)) default else x
  fmt_range <- function(r) sprintf("%+.0f to %+.0f", r[1], r[2])

  read_diet <- function(prefix) {
    d <- lapply(DIET_FIELDS, function(f) num(input[[paste0(prefix, f)]]))
    names(d) <- DIET_FIELDS
    d
  }
  set_diet <- function(prefix, d) {
    for (f in DIET_FIELDS) updateNumericInput(session, paste0(prefix, f), value = round(d[[f]], 2))
  }

  before <- reactive(if (isTRUE(input$before_fixed)) TYPICAL else read_diet("b_"))
  after <- reactive(read_diet("a_"))
  ldl0 <- reactive(max(num(input$ldl0, 95), 20))
  mode <- reactive(input$mode %||% "full")

  knobs <- reactive(list(
    abs = num(input$abs, 50) / 100, synth = num(input$synth, 1),
    apoe = input$apoe %||% "e3e3", ldlr = num(input$ldlr, 100) / 100,
    de_on = isTRUE(input$de_on), de_flux = num(input$de_flux, 1)
  ))
  pred <- reactive(predict_ldl(before(), after(), ldl0(), knobs(), P, mode(), isTRUE(input$split)))

  spread <- reactive({
    n <- min(max(num(input$spread_n, 2000), 200), 5000)
    simulate_spread(before(), after(), ldl0(), knobs(), params, mode(), isTRUE(input$split),
                    n = n, seed = 630 + num(input$spread_redraw))
  })

  # Diet -------------------------------------------------------------------------
  observeEvent(input$example, {
    req(input$example)
    set_diet("a_", diet_from_row(diets, input$example))
  }, ignoreInit = TRUE)
  output$example_text <- renderUI({
    req(input$example)
    p(class = "small-help", diets$description[diets$diet_id == input$example])
  })

  for (pre in c("a_", "b_")) local({
    prefix <- pre
    observeEvent(input[[paste0(prefix, "egg_plus")]], updateNumericInput(
      session, paste0(prefix, "chol"), value = num(input[[paste0(prefix, "chol")]]) + P[["egg_chol_mg"]]))
    observeEvent(input[[paste0(prefix, "egg_minus")]], updateNumericInput(
      session, paste0(prefix, "chol"), value = max(num(input[[paste0(prefix, "chol")]]) - P[["egg_chol_mg"]], 0)))
    output[[paste0(prefix, "macros")]] <- renderUI({
      d <- if (prefix == "a_") after() else before()
      p(class = "small-help", sprintf("Fat %.1f%% · carbohydrate %.1f%% of energy", diet_fat(d), diet_carb(d)))
    })
  })
  output$before_summary <- renderUI({
    d <- TYPICAL
    tagList(tags$h6("Before", class = "fw-bold"),
            p(class = "small-help", diets$description[diets$diet_id == "typical_us"]),
            tags$ul(class = "small-help",
                    tags$li(sprintf("SFA %g%%, MUFA %g%%, PUFA %g%%, trans %g%%", d$sfa, d$mufa, d$pufa, d$trans)),
                    tags$li(sprintf("Cholesterol %g mg/day", d$chol)),
                    tags$li(sprintf("Soluble fiber %g g/day; plant sterols %g g/day", d$fiber, d$sterol))))
  })
  observeEvent(input$apply_foods, {
    servings <- vapply(foods$food_id, function(id) num(input[[paste0("food_", id)]]), numeric(1))
    names(servings) <- foods$food_id
    set_diet("a_", diet_from_foods(before(), foods, servings))
  })

  # Knobs ------------------------------------------------------------------------
  output$site_a_text <- renderUI({
    comp <- pred()$components
    d <- after()$sfa - before()$sfa
    p(sprintf("SFA %+g%% of energy → %+.0f mg/dL. Cholesterol %+g mg/day → %+.0f mg/dL.",
              round(d, 1), comp[["sfa"]], after()$chol - before()$chol, comp[["chol"]]))
  })
  output$comp_text <- renderUI({
    k <- knobs()
    p(class = "small-help mb-1", sprintf(
      "With %.0f%% absorbed, this person offsets %.0f%% of the extra absorbed cholesterol by making less (average person: %.0f%%).",
      100 * k$abs, 100 * comp_from_lathosterol(k$synth, k$abs, P), 100 * P[["comp_avg"]]))
  })
  observeEvent(input$fh_demo, updateSliderInput(session, "ldlr", value = 100 * P[["ldlr_fh"]]))
  observeEvent(input$reset_knobs, {
    updateSliderInput(session, "abs", value = 100 * P[["abs_avg"]])
    updateSliderInput(session, "synth", value = 1)
    updateSelectInput(session, "apoe", selected = "e3e3")
    updateSliderInput(session, "ldlr", value = 100)
    updateCheckboxInput(session, "de_on", value = FALSE)
  })

  # Predict first ----------------------------------------------------------------
  revealed <- reactiveVal(FALSE)
  observeEvent(input$reveal, revealed(TRUE))
  observeEvent(input$predict_first, revealed(FALSE))
  show_answers <- reactive(!isTRUE(input$predict_first) || revealed())
  output$show_answers <- show_answers
  outputOptions(output, "show_answers", suspendWhenHidden = FALSE)

  output$answer_cards <- renderUI({
    if (!show_answers()) {
      return(card(card_header("Spread of outcomes and where the change comes from"),
                  p(class = "text-muted", "These appear after you reveal.")))
    }
    tagList(
      card(
        full_screen = TRUE,
        card_header("Spread of outcomes: the same diet change in many people"),
        layout_columns(
          col_widths = breakpoints(sm = 12, md = c(6, 6)),
          numericInput("spread_n", "People simulated", 2000, min = 200, max = 5000, step = 100),
          div(class = "pt-md-4", actionButton("spread_redraw", "Draw new people", class = "btn-outline-primary btn-sm"))
        ),
        plotOutput("spread_plot", height = "340px"),
        uiOutput("variance_table")
      ),
      card(
        full_screen = TRUE,
        card_header("Where this person's change comes from (mg/dL)"),
        plotOutput("waterfall", height = "420px"),
        card_footer(textOutput("waterfall_caption"))
      ),
      card(card_header("Benchmarks and ApoB (particle count)"), uiOutput("benchmarks"))
    )
  })

  # Predict tab ------------------------------------------------------------------
  output$headline <- renderUI({
    req(show_answers())
    r <- pred()
    sp <- spread()
    across <- summarise_spread(sp$both, sp$ldl0)
    gap <- round(r$person_delta - r$avg_delta)
    boxes <- layout_columns(
      col_widths = breakpoints(sm = 12, md = c(4, 4, 4)),
      value_box(title = "Average person", value = sprintf("%.0f → %.0f", r$ldl0, r$avg_after),
                p(sprintf("%+.0f mg/dL", r$avg_delta)),
                p(class = "small", sprintf("95%% range %s (coefficient uncertainty)",
                                           fmt_range(change_interval(sp$avg_coef, sp$ldl0)))),
                theme = value_box_theme(bg = UM[["maize"]], fg = UM[["blue"]])),
      value_box(title = "This person", value = sprintf("%.0f → %.0f", r$ldl0, r$person_after),
                p(sprintf("%+.0f mg/dL%s", r$person_delta,
                          if (gap == 0) " (knobs at the average)" else sprintf(" (%+.0f vs average)", gap))),
                p(class = "small", sprintf("95%% range %s (coefficient uncertainty)",
                                           fmt_range(change_interval(sp$person_coef, sp$ldl0)))),
                theme = value_box_theme(bg = UM[["blue"]], fg = "white")),
      value_box(title = "Across people", value = sprintf("%s mg/dL", fmt_range(c(across$lo, across$hi))),
                p(sprintf("Middle 90%% of people · SD %.0f mg/dL", across$sd)),
                p(class = "small", sprintf("LDL-C rises in %.0f%%; %.0f%% reach 160+, %.0f%% reach 190+",
                                           across$pct_rise, across$pct_160, across$pct_190)),
                theme = value_box_theme(bg = UM[["light"]], fg = UM[["blue"]]))
    )
    lines <- list()
    if (gap != 0) {
      biggest <- names(which.max(abs(r$knob_steps)))
      lines <- c(lines, list(sprintf("This person's biggest knob: site %s (%+.0f mg/dL).",
                                     biggest, r$knob_steps[[biggest]])))
    }
    if (isTRUE(input$predict_first) && revealed()) {
      g <- c(num(input$guess_avg, NA), num(input$guess_person, NA))
      if (any(!is.na(g))) lines <- c(lines, list(sprintf(
        "Your guesses: average %s, this person %s.",
        if (is.na(g[1])) "–" else sprintf("%.0f (off by %+.0f)", g[1], g[1] - r$avg_after),
        if (is.na(g[2])) "–" else sprintf("%.0f (off by %+.0f)", g[2], g[2] - r$person_after))))
    }
    if (r$capped) lines <- c(lines, list("LDL receptor activity hit the model's floor, so this prediction is capped."))
    tagList(boxes, if (length(lines)) div(class = "mb-3", lapply(lines, p, class = "mb-1")))
  })

  output$warnings <- renderUI({
    w <- range_warnings(after(), P, "After")
    if (!isTRUE(input$before_fixed)) w <- c(w, range_warnings(before(), P, "Before"))
    if (isTRUE(input$split) && diet_sfa_other(after()) < -0.05) {
      w <- c(w, "After: the SFA subtypes add up to more than total SFA.")
    }
    if (!length(w)) return(NULL)
    div(class = "alert alert-warning small",
        strong("Outside the range of the trials behind the coefficients"),
        tags$ul(class = "mb-0", lapply(w, tags$li)))
  })

  output$spread_plot <- renderPlot({
    plot_spread(spread(), pred(), P, show_keto = diet_carb(after()) <= 20)
  }, res = 96)

  output$variance_table <- renderUI({
    vt <- variance_table(spread())
    knob_rows <- vt[grepl("^knob_", vt$key), ]
    top <- if (max(knob_rows$sd) > 0.5) sub(" only$", "", trimws(knob_rows$source[which.max(knob_rows$sd)])) else NULL
    tagList(
      tags$table(class = "table table-sm lab-table mb-1",
        tags$thead(tags$tr(tags$th("Source of spread"), tags$th("SD"),
                           tags$th("Middle 90%"))),
        tags$tbody(lapply(seq_len(nrow(vt)), function(i) {
          strong_row <- vt$key[i] %in% c("avg_coef", "people", "both")
          cell <- function(x) if (strong_row) tags$strong(x) else x
          tags$tr(tags$td(class = if (!strong_row) "ps-4", cell(trimws(vt$source[i]))),
                  tags$td(cell(sprintf("%.1f", vt$sd[i]))),
                  tags$td(class = "text-nowrap", cell(fmt_range(c(vt$lo[i], vt$hi[i])))))
        }))),
      p(class = "small-help",
        "Change in LDL-C, mg/dL. ",
        if (!is.null(top)) sprintf("For this diet change, the knob that spreads people out most is %s. ", top),
        "Knobs interact, so the one-at-a-time rows don't add up to the total. ",
        "D/E (a hypothesis) is not drawn.",
        if (diet_carb(after()) <= 20) sprintf(
          " Real keto trials: Retterstøl 2018 found individual LDL-C rises of %g%% to %g%% in 3 weeks; in Burén 2021 LDL-C rose in every participant.",
          P[["keto_trial_min"]], P[["keto_trial_max"]]))
    )
  })

  output$waterfall <- renderPlot(plot_waterfall(pred(), mode()), res = 96)
  output$waterfall_caption <- renderText(paste(
    "Yellow: the average-person terms (each fixed by the diet).",
    "Blue: this person's knobs, switched on in decoder order (B, C, D/E, F); each bar is what that site adds once the earlier ones are set."))

  output$benchmarks <- renderUI({
    r <- pred()
    across <- summarise_spread(spread()$both, ldl0())
    row <- function(who, ldl) {
      apob <- apob_estimate(ldl, P)
      tags$tr(tags$td(who), tags$td(sprintf("%.0f", ldl)), tags$td(as.character(ldl_category(ldl))),
              tags$td(sprintf("%.0f", apob)),
              tags$td(if (apob >= P[["apob_high"]]) tags$strong(class = "text-danger", "≥130: adds risk") else "–"))
    }
    tagList(
      tags$table(class = "table table-sm lab-table",
        tags$thead(tags$tr(tags$th(""), tags$th("LDL-C"), tags$th("Category"),
                           tags$th("ApoB est."), tags$th("ApoB"))),
        tags$tbody(
          row("Before", r$ldl0),
          row("Average person after", r$avg_after),
          row("This person after", r$person_after),
          row("Across people: 5th percentile", r$ldl0 + across$lo),
          row("Across people: 95th percentile", r$ldl0 + across$hi)
        )),
      p(class = "small-help",
        "LDL-C categories: under 100 optimal · 100–129 near optimal · 130–159 borderline high · 160–189 high · 190+ very high. ",
        "Each LDL (and VLDL) particle carries one ApoB, so ApoB counts particles. ",
        "The estimate is a straight line from LDL-C; when TG-rich particles are unloaded fast (D/E) or TG is high, LDL is smaller and denser, so there can be more particles than LDL-C suggests.")
    )
  })

  # Fix it tab -------------------------------------------------------------------
  fixes <- reactive(list(
    swap_pct = num(input$fx_swap), swap_to = input$fx_swap_to %||% "nut", eggs = num(input$fx_eggs),
    psyllium = num(input$fx_psyllium), sterols = isTRUE(input$fx_sterols),
    ezetimibe = isTRUE(input$fx_ezetimibe), statin = isTRUE(input$fx_statin), carbs = isTRUE(input$fx_carbs)
  ))
  fix_result <- reactive(evaluate_fixes(before(), after(), ldl0(), knobs(), fixes(), P,
                                        mode(), isTRUE(input$split)))
  output$fix_plot <- renderPlot({
    f <- fix_result()
    plot_fixes(ldl0(), f$base, f$all)
  }, res = 96)
  output$fix_table <- renderUI({
    t <- fix_result()$table
    if (is.null(t)) return(p(class = "text-muted", "Choose one or more fixes from the menu."))
    t <- t[order(t$person), ]
    tagList(
      tags$table(class = "table table-sm lab-table",
        tags$thead(tags$tr(tags$th("Fix"), tags$th("Site"), tags$th("How"),
                           tags$th("Average person"), tags$th("This person"))),
        tags$tbody(lapply(seq_len(nrow(t)), function(i) tags$tr(
          tags$td(t$fix[i]), tags$td(site_badge(t$site[i])), tags$td(class = "small-help", t$how[i]),
          tags$td(sprintf("%+.0f", t$average[i])), tags$td(tags$strong(sprintf("%+.0f", t$person[i]))))))),
      p(class = "small-help",
        "Which fix is aimed at this person's site, and which would do little for them? Fixes together do not simply add up.")
    )
  })

  # Sources tab ------------------------------------------------------------------
  output$param_table <- renderTable({
    df <- params
    if (isTruthy(input$param_cat)) df <- df[df$category == input$param_cat, ]
    fmt <- function(x) ifelse(is.na(x), "", format(x, drop0trailing = TRUE, trim = TRUE))
    df$value <- fmt(df$value)
    df$`95% interval` <- ifelse(is.na(df$lower), "", paste(fmt(df$lower), "to", fmt(df$upper)))
    df[, c("param_id", "category", "value", "95% interval", "unit", "description", "source", "verified", "notes")]
  }, striped = TRUE, spacing = "xs", na = "")
}

shinyApp(ui, server)
