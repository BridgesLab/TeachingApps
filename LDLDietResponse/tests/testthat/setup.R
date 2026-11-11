# Source the app's plain-R functions for testing
app_dir <- normalizePath(file.path(testthat::test_path(), "..", ".."))
suppressPackageStartupMessages(library(shiny))
for (f in list.files(file.path(app_dir, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = FALSE)
}
params <- load_params(file.path(app_dir, "data", "parameters.csv"))
P <- param_vector(params)
diets <- load_diets(file.path(app_dir, "data", "diets.csv"))
foods <- load_foods(file.path(app_dir, "data", "foods.csv"))
TYPICAL <- diet_from_row(diets, "typical_us")
EGGS <- diet_from_row(diets, "keto_eggs_butter")

#' Knobs with some fields changed from the average person
knobs_with <- function(...) utils::modifyList(default_knobs(P), list(...))
