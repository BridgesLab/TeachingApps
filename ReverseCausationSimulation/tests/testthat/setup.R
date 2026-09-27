# Source app.R to get its plain-R functions. The final shinyApp() call only
# builds an app object; it doesn't start a server.
app_dir <- normalizePath(file.path(testthat::test_path(), "..", ".."))
source(file.path(app_dir, "app.R"), local = FALSE, chdir = TRUE)
