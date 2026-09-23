# Source the app's plain-R functions (not the Shiny modules' UI) for testing
app_dir <- normalizePath(file.path(testthat::test_path(), "..", ".."))
for (f in list.files(file.path(app_dir, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = FALSE)
}
params <- load_params(file.path(app_dir, "data", "parameters.csv"))
scenarios <- load_scenarios(file.path(app_dir, "data", "scenarios.csv"))
