# Tab 2: Testing -------------------------------------------------------------

N_PEOPLE <- 10000
N_DRAWS_HEADLINE <- 2000
N_DRAWS_CURVES <- 400

mod_testing_ui <- function(id, scenarios) {
  ns <- NS(id)
  scenario_choices <- c("Choose a scenario card…" = "",
                        stats::setNames(scenarios$scenario_id, scenarios$label))

  layout_sidebar(
    sidebar = sidebar(
      width = 330,
      open = list(desktop = "open", mobile = "always-above"),
      selectInput(ns("scenario"), "Scenario card", choices = scenario_choices),
      uiOutput(ns("scenario_card")),
      accordion(
        open = c("person", "test"), multiple = TRUE,
        accordion_panel(
          "Who is being tested?", value = "person",
          sliderInput(ns("age"), "Age", min = 20, max = 80, value = 25, step = 1),
          selectInput(ns("group"), "Risk group", choices = GROUP_CHOICES),
          conditionalPanel(
            "input.group == 'fdr'", ns = ns,
            numericInput(ns("fdr_age"), "Relative's age at diagnosis", value = 50, min = 20, max = 100)
          ),
          checkboxGroupInput(ns("lifestyle"), "Lifestyle factors", choices = LIFESTYLE_CHOICES),
          radioButtons(ns("symptoms"), "Symptoms", choices = SYMPTOM_CHOICES),
          checkboxInput(ns("override"), "Type in my own pre-test probability"),
          conditionalPanel(
            "input.override", ns = ns,
            numericInput(ns("prev_pct"), "Pre-test probability of cancer (%)",
                         value = 0.1, min = 0.0001, max = 99, step = 0.01)
          )
        ),
        accordion_panel(
          "Which test?", value = "test",
          radioButtons(ns("test"), NULL, choices = TEST_CHOICES),
          radioButtons(ns("target"), "What counts as a 'true' positive?", choices = TARGET_CHOICES),
          checkboxInput(ns("unlock"), "Unlock test accuracy"),
          conditionalPanel(
            "input.unlock", ns = ns,
            sliderInput(ns("sens_crc"), "Sensitivity for cancer", min = 0.3, max = 1, value = 0.92, step = 0.005),
            sliderInput(ns("sens_aa"), "Sensitivity for advanced adenoma", min = 0, max = 1, value = 0.42, step = 0.005),
            sliderInput(ns("spec"), "Specificity", min = 0.5, max = 1, value = 0.87, step = 0.0005),
            actionLink(ns("reset_acc"), "Reset to published values")
          )
        ),
        accordion_panel(
          "Options", value = "options",
          checkboxInput(ns("sequential"), "Follow a positive test with colonoscopy + biopsy", value = FALSE),
          checkboxInput(ns("uncertainty"), "Show uncertainty (simulation)", value = FALSE),
          checkboxInput(ns("log_scale"), "Log scale for probabilities", value = TRUE)
        )
      )
    ),

    uiOutput(ns("headline")),
    card(
      card_header("What this means"),
      uiOutput(ns("sentence"))
    ),
    card(
      full_screen = TRUE,
      card_header(textOutput(ns("icon_title"), inline = TRUE)),
      plotOutput(ns("icon"), height = "auto"),
      uiOutput(ns("icon_legend")),
      card_footer(textOutput(ns("icon_caption")))
    ),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(6, 6)),
      card(
        full_screen = TRUE,
        card_header("Before and after the test"),
        plotOutput(ns("ladder"), height = "auto"),
        card_footer(textOutput(ns("ladder_caption")))
      ),
      card(
        card_header(paste0("What happens next, per ", scales::comma(N_PEOPLE), " people tested")),
        tableOutput(ns("downstream")),
        card_footer(textOutput(ns("downstream_caption")))
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Same test, different people: PPV by age and risk group"),
      plotOutput(ns("curves"), height = "auto"),
      card_footer(textOutput(ns("curves_caption")))
    ),
    conditionalPanel(
      "input.uncertainty", ns = ns,
      card(
        card_header("How sure are we? Simulated PPV"),
        plotOutput(ns("posterior"), height = "280px"),
        card_footer(textOutput(ns("posterior_caption")))
      )
    )
  )
}

mod_testing_server <- function(id, params, scenarios) {
  moduleServer(id, function(input, output, session) {

    # Scenario cards ---------------------------------------------------------
    current_card <- reactive({
      req(input$scenario)
      dplyr::filter(scenarios, scenario_id == input$scenario)
    })

    observeEvent(input$scenario, {
      req(input$scenario != "")
      s <- current_card()
      lifestyle <- if (is.na(s$lifestyle)) character(0) else strsplit(s$lifestyle, ";")[[1]]
      updateSliderInput(session, "age", value = s$age)
      updateSelectInput(session, "group", selected = s$group)
      if (!is.na(s$fdr_age)) updateNumericInput(session, "fdr_age", value = s$fdr_age)
      updateCheckboxGroupInput(session, "lifestyle", selected = lifestyle)
      updateRadioButtons(session, "symptoms", selected = s$symptoms)
      updateRadioButtons(session, "test", selected = s$test)
      updateRadioButtons(session, "target", selected = s$target)
      updateCheckboxInput(session, "sequential", value = isTRUE(s$sequential))
      updateCheckboxInput(session, "override", value = FALSE)
      updateCheckboxInput(session, "unlock", value = FALSE)
    })

    output$scenario_card <- renderUI({
      req(input$scenario != "")
      s <- current_card()
      div(class = "alert alert-info small mb-2",
          p(class = "mb-1", s$card_text),
          p(class = "mb-0", strong("Report back: "), s$discussion))
    })

    # Accuracy sliders follow the chosen test unless the user edits them ----
    reset_sliders <- function() {
      d <- accuracy_defaults(params, input$test, input$target)
      updateSliderInput(session, "sens_crc", value = d$sens_crc)
      updateSliderInput(session, "sens_aa", value = d$sens_aa)
      updateSliderInput(session, "spec", value = d$spec)
    }
    observeEvent(list(input$test, input$target), reset_sliders())
    observeEvent(input$reset_acc, reset_sliders())

    # Model ------------------------------------------------------------------
    settings <- reactive({
      req(input$age, input$group, input$test, input$target, input$symptoms)
      list(
        age = input$age,
        group = input$group,
        fdr_age = if (is.na(input$fdr_age %||% NA)) 50 else input$fdr_age,
        lifestyle = input$lifestyle %||% character(0),
        symptoms = input$symptoms,
        test = input$test,
        target = input$target,
        prev_override = if (isTRUE(input$override) && isTruthy(input$prev_pct)) {
          min(max(input$prev_pct, 1e-4), 99) / 100
        },
        accuracy_override = if (isTRUE(input$unlock)) {
          list(sens_crc = input$sens_crc, sens_aa = input$sens_aa, spec = input$spec)
        }
      )
    }) |> debounce(250)

    scenario <- reactive({
      s <- settings()
      build_scenario(params, s$age, s$group, s$fdr_age, s$lifestyle, s$symptoms,
                     s$test, s$target, s$prev_override, s$accuracy_override)
    })

    point <- reactive(run_scenario(scenario(), n = N_PEOPLE))

    draws <- reactive({
      req(isTRUE(input$uncertainty))
      run_scenario(scenario(), n_draws = N_DRAWS_HEADLINE, n = N_PEOPLE)
    })

    interval <- function(metric) {
      if (!isTRUE(input$uncertainty)) return(NULL)
      summarise_draws(draws()[[metric]])
    }

    equiv_age <- reactive({
      s <- settings()
      risk_equivalent_age(s$group, params, s$fdr_age, s$lifestyle,
                          ref_age = param_value(params, "guide_avg_start"))
    })

    test_label <- reactive(sub(" \\(.*", "", label_of(input$test, TEST_CHOICES)))
    disease_word <- reactive(if (input$target == "crc") "cancer" else "cancer or an advanced adenoma")
    sequential_on <- reactive(isTRUE(input$sequential) && input$test != "colonoscopy")

    # Headline numbers --------------------------------------------------------
    output$headline <- renderUI({
      r <- point()
      ivl <- function(metric, f) {
        s <- interval(metric)
        if (is.null(s)) NULL else p(class = "small mb-0", "95% range: ", fmt_interval(s$lower, s$upper, f))
      }
      ea <- equiv_age()
      ref <- param_value(params, "guide_avg_start")
      ea_text <- if (is.na(ea)) paste0("Not reached by 85") else if (ea <= 18) "18 or younger" else paste0(round(ea), " years")

      boxes <- list(
        value_box(
          title = "Chance of disease if positive (PPV)",
          value = fmt_pct(r$ppv),
          p(fmt_one_in(r$ppv)), ivl("ppv", fmt_pct),
          theme = "primary"
        ),
        value_box(
          title = "Chance of no disease if negative (NPV)",
          value = fmt_pct(r$npv),
          p(paste0("Missed: ", fmt_one_in(r$post_neg), " negatives")), ivl("npv", fmt_pct),
          theme = "secondary"
        ),
        value_box(
          title = "False positives per true positive",
          value = fmt_count(r$fp_per_tp),
          p(paste0("per ", if (input$target == "crc") "cancer" else "lesion", " found by the test")),
          ivl("fp_per_tp", fmt_count),
          theme = "warning"
        ),
        value_box(
          title = paste0("Risk-equivalent age (vs. average-risk ", ref, ")"),
          value = ea_text,
          p(paste0("Age this profile reaches the yearly risk of an average-risk ", ref, "-year-old")),
          theme = "light"
        )
      )
      if (sequential_on()) {
        boxes <- append(boxes, list(value_box(
          title = "PPV after colonoscopy + biopsy",
          value = fmt_pct(r$ppv_seq),
          p("Positive on both tests"), ivl("ppv_seq", fmt_pct),
          theme = "success"
        )), after = 1)
      }
      do.call(layout_column_wrap, c(list(width = "210px", fill = FALSE, heights_equal = "row"), boxes))
    })

    output$sentence <- renderUI({
      r <- point()
      s <- settings()
      who <- paste0("a ", s$age, "-year-old (", label_of(s$group, GROUP_CHOICES), ")")
      screening <- s$symptoms == "none"
      ppv_s <- interval("ppv")
      unc <- if (is.null(ppv_s)) "" else
        paste0(" Allowing for uncertainty in the inputs, it could plausibly be anywhere from ",
               fmt_one_in(ppv_s$upper), " to ", fmt_one_in(ppv_s$lower), ".")
      aa_note <- if (input$target == "crc" && input$test != "colonoscopy" && r$fp_with_aa >= 0.5) {
        paste0(" Of the ", fmt_count(r$fp), " false positives, about ", fmt_count(r$fp_with_aa),
               " have an advanced adenoma: not cancer, but worth removing.")
      } else ""
      tagList(
        p(class = "lead mb-2", HTML(paste0(
          "If ", who, " tests positive on ", test_label(), ", their chance of actually having ",
          disease_word(), " is about <strong>", fmt_one_in(r$ppv), "</strong> (", fmt_pct(r$ppv), ").", unc))),
        p(HTML(paste0(
          "For every ", if (input$target == "crc") "cancer" else "lesion", " the test finds, about <strong>",
          fmt_count(r$fp_per_tp), "</strong> people without it also test positive and are sent for a colonoscopy.",
          aa_note))),
        if (!screening) p(class = "text-body-secondary",
          "This person has symptoms, so this is diagnostic testing, not screening. Symptoms raise the ",
          "pre-test probability; guidelines generally recommend colonoscopy for red-flag symptoms at any age ",
          "rather than relying on a negative stool test.")
      )
    })

    # Icon array ------------------------------------------------------------
    counts <- reactive({
      r <- point()
      c(tp = r$tp, fn = r$fn, fp = r$fp, tn = r$tn)
    })

    output$icon_title <- renderText({
      paste0(scales::comma(N_PEOPLE), " people like this, all tested with ", test_label())
    })

    output_width <- function(name) {
      session$clientData[[paste0("output_", session$ns(name), "_width")]] %||% 600
    }
    is_narrow <- function(name) output_width(name) < 480

    output$icon <- renderPlot(
      plot_icon_array(counts(), input$target, N_PEOPLE, narrow = is_narrow("icon")),
      height = function() round(min(output_width("icon"), 700) * 1.08), res = 96,
      alt = reactive(icon_alt_text(counts(), input$target, N_PEOPLE))
    )

    output$icon_legend <- renderUI(icon_legend_ui(counts(), input$target, N_PEOPLE))

    output$icon_caption <- renderText({
      r <- point()
      paste0("Each dot is one person. Large blue circles are people with ", disease_word(),
             " the test caught; large red triangles are those it missed; orange squares are false positives; ",
             "grey dots tested negative and are disease-free. Expected numbers are rounded to whole people ",
             "(expected people with the target condition: ", fmt_count(r$tp + r$fn), ").")
    })

    # Probability ladder ----------------------------------------------------
    ladder_df <- reactive(prob_ladder_data(point(), test_label(), sequential_on()))

    output$ladder <- renderPlot(
      plot_prob_ladder(ladder_df(), isTRUE(input$log_scale), narrow = is_narrow("ladder")),
      height = function() 70 + nrow(ladder_df()) * (if (is_narrow("ladder")) 80 else 70), res = 96,
      alt = reactive(paste0("Bar chart of the probability of ", disease_word(), ": ",
                            paste(gsub("\n", " ", ladder_df()$stage), collapse = "; "), "."))
    )

    output$ladder_caption <- renderText({
      paste0("Bayes' rule in action: the test result updates the pre-test probability. ",
             if (isTRUE(input$log_scale)) "Log scale: each gridline is 100 times the one before. " else "",
             if (sequential_on()) "The last bar assumes colonoscopy is independent of the first test's error." else
               "Tick 'Follow a positive test with colonoscopy' to add the follow-up step.")
    })

    # Downstream table -------------------------------------------------------
    output$downstream <- renderTable({
      r <- point()
      rows <- tibble::tribble(
        ~metric, ~label,
        "positives",     "Positive tests",
        "colonoscopies", "Colonoscopies",
        "crc_found",     "Cancers found",
        "crc_missed",    "Cancers missed",
        "aa_found",      "Advanced adenomas found",
        "perforations",  "Perforations",
        "bleeds",        "Serious bleeds",
        "colos_per_crc_found", "Colonoscopies per cancer found"
      )
      out <- tibble::tibble(Outcome = rows$label,
                            Expected = fmt_count(unlist(r[rows$metric])))
      if (isTRUE(input$uncertainty)) {
        d <- draws()
        out$`95% range` <- vapply(rows$metric, function(m) {
          s <- summarise_draws(d[[m]]); fmt_interval(s$lower, s$upper)
        }, character(1))
      }
      out
    }, striped = TRUE, width = "100%", align = "l")

    output$downstream_caption <- renderText({
      paste0("Assumes everyone with a positive ", test_label(),
             " has a follow-up colonoscopy (in practice many do not). Harm rates from the parameter file; ",
             "cancers 'found' require both the test and the colonoscopy to detect them.")
    })

    # PPV by age -------------------------------------------------------------
    curves <- reactive({
      s <- settings()
      ppv_by_age(params, standard_profiles(s$fdr_age, s$group, s$lifestyle),
                 ages = 20:80, symptoms = s$symptoms, test = s$test, target = s$target,
                 accuracy_override = s$accuracy_override,
                 n_draws = if (isTRUE(input$uncertainty)) N_DRAWS_CURVES else 0)
    })

    output$curves <- renderPlot({
      s <- settings()
      current <- tibble::tibble(
        age = s$age, ppv = max(point()$ppv, 1e-5),
        colour_key = if (length(s$lifestyle) > 0) "custom" else s$group
      )
      plot_ppv_curves(curves(), current, isTRUE(input$log_scale), narrow = is_narrow("curves"))
    }, height = function() if (is_narrow("curves")) 520 else 460, res = 96, alt = reactive(curves_alt_text(curves(), test_label())))

    output$curves_caption <- renderText({
      paste0("Each line shows the PPV of ", test_label(), " for one risk group across ages; the open circle ",
             "marks the current settings. ",
             if (isTRUE(input$uncertainty)) "Shaded bands are 95% simulation intervals. " else "",
             "The test itself is identical along every line; only the pre-test probability changes.")
    })

    # Posterior --------------------------------------------------------------
    output$posterior <- renderPlot({
      plot_ppv_posterior(draws(), point()$ppv)
    }, res = 96, alt = reactive({
      s <- summarise_draws(draws()$ppv)
      paste0("Histogram of ", N_DRAWS_HEADLINE, " simulated PPVs; median ", fmt_pct(s$median),
             ", 95% range ", fmt_pct(s$lower), " to ", fmt_pct(s$upper), ".")
    }))

    output$posterior_caption <- renderText({
      paste0("Each of ", scales::comma(N_DRAWS_HEADLINE), " simulations draws sensitivity, specificity and ",
             "prevalence from Beta distributions (see Sources tab) and recomputes the PPV. Red line: point ",
             "estimate; dashed lines: 95% range.")
    })

    # expose state for other tabs (e.g. the prior-elicitation tab)
    list(point = point, settings = settings)
  })
}

# Alt-text helpers -------------------------------------------------------------

icon_alt_text <- function(counts, target, n) {
  k <- round_counts(counts[c("tp", "fn", "fp", "tn")], n)
  paste0("Icon array of ", scales::comma(n), " people: ",
         k[["tp"]], " true positives, ", k[["fn"]], " false negatives, ",
         scales::comma(k[["fp"]]), " false positives and ", scales::comma(k[["tn"]]), " true negatives.")
}

curves_alt_text <- function(curves, test_label) {
  at <- curves |> dplyr::filter(age %in% c(25, 45, 65))
  parts <- at |>
    dplyr::group_by(profile) |>
    dplyr::summarise(txt = paste0(profile[1], ": ", paste0(fmt_pct(ppv), " at ", age, collapse = ", ")),
                     .groups = "drop")
  paste0("Line chart of ", test_label, " PPV by age for each risk group. ",
         paste(parts$txt, collapse = "; "), ".")
}
