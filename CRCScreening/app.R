# Colorectal cancer risk and screening: an in-class activity
# Run with shiny::runApp() from this folder. Files in R/ are sourced automatically.

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)

params <- load_params("data/parameters.csv")
scenarios <- load_scenarios("data/scenarios.csv")

ui <- page_navbar(
  title = "CRC risk & screening",
  theme = bs_theme(version = 5, primary = "#0072B2", secondary = "#56B4E9",
                   warning = "#E69F00", success = "#009E73"),
  fillable = FALSE,
  navbar_options = navbar_options(collapsible = TRUE),
  selected = "testing",
  # shown above every tab, including on phones where the navbar collapses
  header = div(
    class = "alert alert-warning small mb-0 rounded-0 border-0 border-bottom text-center py-2",
    role = "note",
    strong("For teaching only. "),
    "This is an illustrative model with simplified, unverified numbers. ",
    "It is not a diagnostic tool and must not be used to make decisions about anyone's health. ",
    "Talk to a clinician about real screening or symptoms."
  ),

  nav_panel(
    "0 · Before you start", value = "prior",
    mod_placeholder_ui("prior", "Before you start: make a guess", c(
      "A 25-year-old with no symptoms gets a positive Cologuard. What is the chance they have cancer?",
      "Enter a guess, then reveal the model answer and compare.",
      "Optional class histogram of guesses."
    ))
  ),
  nav_panel(
    "1 · Risk in context", value = "risk",
    mod_placeholder_ui("risk", "Risk in context", c(
      "Cumulative CRC risk curves (ages 20-75) by risk group and lifestyle factors.",
      "Risk-equivalent age marked on the plot, compared with guideline start ages.",
      "Same relative risk as absolute risk at 25 vs 55.",
      "Optional: early-onset birth-cohort trend, relative vs absolute change."
    ))
  ),
  nav_panel("2 · Testing", value = "testing", mod_testing_ui("testing", scenarios)),
  nav_panel(
    "3 · Genetic testing", value = "genetic",
    mod_placeholder_ui("genetic", "Genetic testing as a test", c(
      "Lynch prevalence, cascade testing in relatives, variants of uncertain significance.",
      "Should every 20-year-old get a Lynch panel? Same PPV machinery."
    ))
  ),
  nav_panel("4 · Sources", value = "sources", mod_sources_ui("sources", params)),
  nav_spacer(),
  nav_item(tags$span(class = "navbar-text small", "Illustrative teaching model; not for diagnosis"))
)

server <- function(input, output, session) {
  mod_testing_server("testing", params, scenarios)
  mod_sources_server("sources", params)
}

shinyApp(ui, server)
