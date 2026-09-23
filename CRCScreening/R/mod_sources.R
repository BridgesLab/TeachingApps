# Tab 4: Sources and assumptions ---------------------------------------------

ASSUMPTIONS <- c(
  "Prevalence of undiagnosed, screen-detectable cancer = annual incidence x mean sojourn time. This ignores prior screening history.",
  "Risk groups multiply the average-risk incidence by a constant relative risk at every age; Lynch syndrome instead uses a Weibull curve fitted to cumulative risk at 50 and 75.",
  "Lifestyle relative risks multiply together (no interactions, no confounding between them) and apply to adenomas as well as cancers.",
  "Symptoms update the pre-test odds with a likelihood ratio. The values are case-control odds ratios used as approximate likelihood ratios; 'both' multiplies them, assuming independence.",
  "Test sensitivity and specificity come from trials in people aged 40-50+; accuracy in 20-year-olds is not well studied, and specificity varies with age.",
  "'Specificity' is measured in people without advanced neoplasia. When the target is cancer only, stool and blood tests still react to advanced adenomas at the adenoma sensitivity, so those positives count as false positives for cancer.",
  "Sequential testing assumes the colonoscopy's errors are independent of the first test's errors, given the true state.",
  "Colonoscopy with biopsy is treated as near-definitive for cancer; everyone with a positive test is assumed to have the follow-up colonoscopy.",
  "One-time testing only: no repeat rounds, programme adherence, overdiagnosis or mortality benefit.",
  "Uncertainty: sensitivity and specificity use Beta distributions built from trial counts (Jeffreys prior); prevalence uses a Beta with the coefficient of variation given in the file; harms use a lognormal distribution from the 95% interval.",
  "Cumulative risks ignore death from other causes."
)

mod_sources_ui <- function(id, params) {
  ns <- NS(id)
  cats <- sort(unique(params$category))
  layout_columns(
    col_widths = 12,
    card(
      card_header("Every number in this app, and where it comes from"),
      div(class = "alert alert-warning mb-2",
          strong("Draft values. "),
          "Every value is marked VERIFY until it has been checked against the primary source. ",
          "Edit data/parameters.csv to change any number; the app reloads it on restart."),
      selectInput(ns("category"), "Show category", choices = c("All" = "", cats), width = "280px"),
      div(style = "overflow-x: auto;", tableOutput(ns("table")))
    ),
    card(
      card_header("Simplifying assumptions"),
      tags$ol(lapply(ASSUMPTIONS, tags$li))
    )
  )
}

mod_sources_server <- function(id, params) {
  moduleServer(id, function(input, output, session) {
    output$table <- renderTable({
      df <- params
      if (isTruthy(input$category)) df <- dplyr::filter(df, category == input$category)
      df |>
        dplyr::mutate(
          uncertainty = dplyr::case_when(
            dist == "beta" ~ sprintf("Beta(%.1f, %.1f)", alpha, beta),
            dist == "lognormal" ~ sprintf("95%% CI %g to %g", lower, upper),
            TRUE ~ "fixed"
          ),
          value = format(value, drop0trailing = TRUE, scientific = FALSE, trim = TRUE)
        ) |>
        dplyr::select(id = param_id, category, value, units, uncertainty, source, status, notes)
    }, striped = TRUE, spacing = "xs", na = "")
  })
}
