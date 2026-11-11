# Data files: parameters, example diets, foods ---------------------------------
#
# Every number the model uses lives in data/parameters.csv. The R code holds
# labels and plotting choices only.

PARAM_COLUMNS <- c("param_id", "category", "value", "lower", "upper", "unit", "description",
                   "source", "verified", "notes")

DIET_FIELDS <- c("energy", "protein", "sfa", "sfa_lm", "sfa_p", "sfa_s",
                 "mufa", "pufa", "trans", "chol", "fiber", "sterol")

#' Read a CSV with base R (light for Shinylive), numeric/logical columns typed
read_csv_typed <- function(path, numeric = character(), logical = character()) {
  df <- utils::read.csv(path, colClasses = "character", na.strings = c("", "NA"),
                        check.names = FALSE, encoding = "UTF-8")
  for (col in numeric) df[[col]] <- as.numeric(df[[col]])
  for (col in logical) df[[col]] <- as.logical(df[[col]])
  df
}

load_params <- function(path = "data/parameters.csv") {
  params <- read_csv_typed(path, numeric = c("value", "lower", "upper"))
  validate_params(params)
  params
}

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
  if (!all(params$verified %in% c("y", "n"))) stop("verified must be y or n")
  has_ci <- !is.na(params$lower) | !is.na(params$upper)
  bad_ci <- has_ci & (is.na(params$lower) | is.na(params$upper) |
                        params$lower > params$value | params$upper < params$value)
  if (any(bad_ci)) stop("Invalid 95% interval for: ", paste(params$param_id[bad_ci], collapse = ", "))
  invisible(TRUE)
}

#' Named numeric vector of parameter values, for fast lookup in the model
param_vector <- function(params) stats::setNames(params$value, params$param_id)

load_diets <- function(path = "data/diets.csv") {
  read_csv_typed(path, numeric = DIET_FIELDS)
}

load_foods <- function(path = "data/foods.csv") {
  read_csv_typed(path, numeric = c("sfa_g", "sfa_lm_g", "sfa_p_g", "sfa_s_g", "mufa_g",
                                   "pufa_g", "trans_g", "chol_mg", "fiber_g", "sterol_g"))
}

#' One diet (a named list) from a row of diets.csv
diet_from_row <- function(diets, id) {
  row <- diets[diets$diet_id == id, , drop = FALSE]
  if (nrow(row) != 1) stop("Expected one diet with id '", id, "'")
  as.list(row[1, DIET_FIELDS])
}

# Labels -------------------------------------------------------------------------

APOE_CHOICES <- c("E2/E3" = "e2e3", "E3/E3 (most common)" = "e3e3",
                  "E3/E4" = "e3e4", "E4/E4" = "e4e4")

apoe_label <- function(code) sub(" \\(.*", "", names(APOE_CHOICES)[match(code, APOE_CHOICES)])

#' One entry per decoder site (from the Lipid Transport cards)
SITES <- data.frame(
  letter = c("A", "B", "C", "D", "E", "F", "G"),
  name = c("Fat on the plate", "Gut absorption", "Liver cholesterol sensing and synthesis",
           "Fuel use and VLDL output", "LPL unloading",
           "Liver clearance (LDL receptor and ApoE)", "HDL and CETP"),
  lecture = c("Lipid Intro", "Digestion & Absorption", "Lipid Synthesis", "Lipid Catabolism",
              "Lipid Transport", "Lipid Transport", "Lipid Transport"),
  stringsAsFactors = FALSE
)
