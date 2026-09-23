# Parameter file loading, validation and lookup ------------------------------
#
# All numbers used by the app live in data/parameters.csv. Nothing numeric
# should be hard-coded in the R/ functions except labels and plotting choices.

PARAM_COLUMNS <- c(
  "param_id", "category", "quantity", "group", "test", "target",
  "age_min", "age_max", "value", "dist", "alpha", "beta", "lower", "upper",
  "units", "source", "status", "notes"
)

PARAM_DISTS <- c("fixed", "beta", "lognormal")

#' Read a CSV with base R (keeps the app light for Shinylive/webR), with
#' every column as character except the named numeric/logical ones
read_csv_typed <- function(path, numeric = character(), logical = character()) {
  df <- utils::read.csv(path, colClasses = "character", na.strings = c("", "NA"),
                        check.names = FALSE, encoding = "UTF-8")
  for (col in numeric) df[[col]] <- as.numeric(df[[col]])
  for (col in logical) df[[col]] <- as.logical(df[[col]])
  tibble::as_tibble(df)
}

#' Read and validate the parameter CSV
load_params <- function(path = "data/parameters.csv") {
  params <- read_csv_typed(
    path, numeric = c("age_min", "age_max", "value", "alpha", "beta", "lower", "upper")
  )
  validate_params(params)
  params
}

#' Stop with an informative message if the parameter table is malformed
validate_params <- function(params) {
  missing_cols <- setdiff(PARAM_COLUMNS, names(params))
  if (length(missing_cols) > 0) {
    stop("Parameter file is missing columns: ", paste(missing_cols, collapse = ", "))
  }
  dups <- params$param_id[duplicated(params$param_id)]
  if (length(dups) > 0) stop("Duplicate param_id: ", paste(dups, collapse = ", "))
  if (any(is.na(params$value))) {
    stop("Missing value for: ", paste(params$param_id[is.na(params$value)], collapse = ", "))
  }
  bad_dist <- setdiff(unique(params$dist), PARAM_DISTS)
  if (length(bad_dist) > 0) stop("Unknown dist: ", paste(bad_dist, collapse = ", "))

  beta_rows <- params |> dplyr::filter(dist == "beta")
  bad_beta <- beta_rows |> dplyr::filter(is.na(alpha) | is.na(beta) | alpha <= 0 | beta <= 0)
  if (nrow(bad_beta) > 0) stop("Invalid Beta parameters for: ", paste(bad_beta$param_id, collapse = ", "))

  ln_rows <- params |> dplyr::filter(dist == "lognormal")
  bad_ln <- ln_rows |> dplyr::filter(is.na(lower) | is.na(upper) | lower <= 0 | lower >= upper)
  if (nrow(bad_ln) > 0) stop("Invalid lognormal CI for: ", paste(bad_ln$param_id, collapse = ", "))

  invisible(TRUE)
}

#' Look up a single row / value by id
param_row <- function(params, id) {
  row <- params[params$param_id == id, , drop = FALSE]
  if (nrow(row) != 1) stop("Expected exactly one parameter with id '", id, "', found ", nrow(row))
  row
}

param_value <- function(params, id) param_row(params, id)$value

#' Lognormal sdlog implied by a 95% interval
lognormal_sdlog <- function(lower, upper) (log(upper) - log(lower)) / (2 * stats::qnorm(0.975))

#' Load scenario presets and check they reference valid inputs
load_scenarios <- function(path = "data/scenarios.csv") {
  sc <- read_csv_typed(path, numeric = c("age", "fdr_age"), logical = "sequential")
  stopifnot(
    all(sc$group %in% GROUP_CHOICES),
    all(sc$test %in% TEST_CHOICES),
    all(sc$target %in% TARGET_CHOICES),
    all(sc$symptoms %in% SYMPTOM_CHOICES)
  )
  sc
}

# Labels ---------------------------------------------------------------------

TEST_CHOICES <- c(
  "Cologuard (original)"       = "cologuard",
  "Cologuard Plus"             = "cologuard_plus",
  "FIT (stool immunochemical)" = "fit",
  "Shield (blood test)"        = "shield",
  "Colonoscopy"                = "colonoscopy"
)

GROUP_CHOICES <- c(
  "Average risk"                        = "average",
  "Parent/sibling/child had CRC"        = "fdr",
  "Top 10% polygenic risk score"        = "prs_top10",
  "Lynch syndrome: MLH1"                = "lynch_MLH1",
  "Lynch syndrome: MSH2"                = "lynch_MSH2",
  "Lynch syndrome: MSH6"                = "lynch_MSH6",
  "Lynch syndrome: PMS2"                = "lynch_PMS2"
)

LIFESTYLE_CHOICES <- c(
  "Obesity (BMI 30 or more)"          = "obesity",
  "Heavy alcohol (50+ g/day)"         = "alcohol",
  "Processed meat (50+ g/day)"        = "processed_meat",
  "Low-fiber diet"                    = "low_fiber"
)

SYMPTOM_CHOICES <- c(
  "None (screening)"                  = "none",
  "Rectal bleeding"                   = "rectal_bleeding",
  "Iron-deficiency anemia"            = "ida",
  "Both"                              = "both"
)

TARGET_CHOICES <- c(
  "Cancer only"                       = "crc",
  "Cancer or advanced adenoma"        = "crc_aa"
)

label_of <- function(value, choices) names(choices)[match(value, choices)]
