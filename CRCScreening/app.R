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
  # University of Michigan brand colors (see UM_COLORS in R/plots.R)
  theme = bs_theme(version = 5,
                   primary = UM_COLORS[["blue"]], secondary = UM_COLORS[["arboretum_blue"]],
                   success = UM_COLORS[["taubman_teal"]], info = UM_COLORS[["arboretum_blue"]],
                   warning = UM_COLORS[["maize"]], danger = UM_COLORS[["tappan_red"]]),
  fillable = FALSE,
  navbar_options = navbar_options(collapsible = TRUE, bg = UM_COLORS[["blue"]], theme = "dark"),
  selected = "analysis",
  # shown above every tab, including on phones where the navbar collapses
  header = div(
    class = "alert small mb-0 rounded-0 border-0 text-center py-2",
    style = paste0("background-color:", UM_COLORS[["maize"]], "; color:", UM_COLORS[["blue"]], ";"),
    role = "note",
    strong("For teaching only. "),
    "This is an illustrative model with simplified, unverified numbers. ",
    "It is not a diagnostic tool and must not be used to make decisions about anyone's health. ",
    "Talk to a clinician about real screening or symptoms."
  ),

  nav_panel("Analysis", value = "analysis", mod_analysis_ui("analysis", scenarios)),
  nav_panel("Sources", value = "sources", mod_sources_ui("sources", params)),
  nav_spacer(),
  nav_item(tags$span(class = "navbar-text small", "Illustrative teaching model; not for diagnosis"))
)

server <- function(input, output, session) {
  mod_analysis_server("analysis", params, scenarios)
  mod_sources_server("sources", params)
}

shinyApp(ui, server)
