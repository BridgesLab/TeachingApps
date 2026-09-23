# Absolute risk by age and risk group ----------------------------------------
#
# Everything is expressed as an annual CRC hazard (incidence per person-year):
#   * average risk: SEER 5-year-band incidence, interpolated log-linearly
#   * first-degree relative, polygenic score, lifestyle: average x relative risk
#   * Lynch syndrome: a Weibull curve fitted to cumulative risk at 50 and 75
# Lifestyle relative risks multiply (assumes independence and no interaction).
# Competing mortality is ignored, so cumulative risks are "risk if you live
# that long".

incidence_table <- function(params) {
  params |>
    dplyr::filter(category == "incidence", group == "average") |>
    dplyr::transmute(age_min, age_max,
                     age_mid = (age_min + age_max) / 2,
                     rate = value / 1e5) |>
    dplyr::arrange(age_min)
}

#' Average-risk annual CRC incidence at (possibly non-integer) ages
baseline_hazard <- function(age, params) {
  tab <- incidence_table(params)
  exp(stats::approx(tab$age_mid, log(tab$rate), xout = age, rule = 2)$y)
}

#' Relative risk for one affected first-degree relative, by the relative's age
#' at diagnosis
rr_fdr <- function(fdr_age, params) {
  rows <- params |> dplyr::filter(category == "relative_risk", quantity == "rr_crc", group == "fdr")
  hit <- rows$value[fdr_age >= rows$age_min & fdr_age < rows$age_max + 1]
  if (length(hit) != 1) stop("No FDR relative-risk band covers age ", fdr_age)
  hit
}

#' Combined relative risk for a set of lifestyle factors (multiplicative)
lifestyle_rr <- function(lifestyle, params) {
  if (length(lifestyle) == 0) return(1)
  rows <- params |> dplyr::filter(category == "relative_risk", quantity == "rr_crc",
                                  group %in% lifestyle)
  if (nrow(rows) != length(unique(lifestyle))) stop("Unknown lifestyle factor(s)")
  prod(rows$value)
}

is_lynch <- function(group) startsWith(group, "lynch_")
lynch_gene <- function(group) sub("^lynch_", "", group)

#' Weibull penetrance fitted through cumulative risk at 50 and 75
#' F(a) = 1 - exp(-((a - a0) / lambda)^k) for a > a0
lynch_weibull <- function(gene, params) {
  a0  <- param_value(params, "lynch_onset_age")
  r50 <- param_value(params, paste0("pen_", gene, "_50"))
  r75 <- param_value(params, paste0("pen_", gene, "_75"))
  y1 <- log(-log(1 - r50)); y2 <- log(-log(1 - r75))
  t1 <- 50 - a0;            t2 <- 75 - a0
  k <- (y2 - y1) / (log(t2) - log(t1))
  lambda <- exp(log(t1) - y1 / k)
  list(k = k, lambda = lambda, a0 = a0)
}

weibull_hazard <- function(age, w) {
  t <- pmax(age - w$a0, 0)
  (w$k / w$lambda) * (t / w$lambda)^(w$k - 1)
}

#' Annual CRC hazard for a risk profile
profile_hazard <- function(age, group = "average", params, fdr_age = 50,
                           lifestyle = character()) {
  h <- if (is_lynch(group)) {
    weibull_hazard(age, lynch_weibull(lynch_gene(group), params))
  } else {
    base <- baseline_hazard(age, params)
    switch(group,
      average   = base,
      fdr       = base * rr_fdr(fdr_age, params),
      prs_top10 = base * param_value(params, "rr_prs_top10"),
      stop("Unknown risk group: ", group)
    )
  }
  h * lifestyle_rr(lifestyle, params)
}

#' Cumulative risk of CRC from age `from` to each age in `age`
cumulative_risk <- function(age, group = "average", params, fdr_age = 50,
                            lifestyle = character(), from = 20, step = 0.1) {
  grid <- seq(from, max(age, from), by = step)
  h <- profile_hazard(grid, group, params, fdr_age, lifestyle)
  H <- c(0, cumsum((h[-1] + h[-length(h)]) / 2 * step))
  1 - exp(-stats::approx(grid, H, xout = pmax(age, from), rule = 2)$y)
}

#' Age at which a profile reaches the risk of an average-risk person of
#' `ref_age` (default 45, the USPSTF/ACS start age).
#'
#' metric = "annual" compares yearly incidence; "ten_year" compares the risk
#' of CRC over the next 10 years. Returns NA if never reached by `max_age`,
#' and `min_age` if already reached at the youngest age considered.
risk_equivalent_age <- function(group = "average", params, fdr_age = 50,
                                lifestyle = character(), ref_age = 45,
                                metric = c("annual", "ten_year"),
                                min_age = 18, max_age = 85) {
  metric <- match.arg(metric)
  f <- switch(metric,
    annual = function(a, g, l) profile_hazard(a, g, params, fdr_age, l),
    ten_year = function(a, g, l) {
      vapply(a, function(x) {
        cumulative_risk(x + 10, g, params, fdr_age, l, from = x)
      }, numeric(1))
    }
  )
  target <- f(ref_age, "average", character())
  step <- if (metric == "annual") 0.05 else 0.5
  grid <- seq(min_age, max_age, by = step)
  vals <- f(grid, group, lifestyle)
  first <- which(vals >= target)[1]
  if (is.na(first)) return(NA_real_)
  if (first == 1) return(min_age)
  # linear interpolation between the two grid points that bracket the target
  a <- grid[first - 1]; b <- grid[first]
  a + (target - vals[first - 1]) / (vals[first] - vals[first - 1]) * (b - a)
}

# Prevalence at the moment of testing -----------------------------------------

#' Symptom likelihood ratio (symptoms combined multiplicatively)
symptom_lr <- function(symptoms, params) {
  switch(symptoms,
    none            = 1,
    rectal_bleeding = param_value(params, "lr_rectal_bleeding"),
    ida             = param_value(params, "lr_ida"),
    both            = param_value(params, "lr_rectal_bleeding") * param_value(params, "lr_ida"),
    stop("Unknown symptom setting: ", symptoms)
  )
}

#' Probability of an undiagnosed, screen-detectable CRC at a given age:
#' annual incidence x mean sojourn time, then updated for symptoms
crc_prevalence <- function(age, group = "average", params, fdr_age = 50,
                           lifestyle = character(), symptoms = "none") {
  prev <- profile_hazard(age, group, params, fdr_age, lifestyle) *
    param_value(params, "sojourn_crc")
  post_test_prob(pmin(prev, 0.99), symptom_lr(symptoms, params))
}

#' Advanced adenoma (without cancer) prevalence
aa_prevalence <- function(age, group = "average", params, lifestyle = character()) {
  tab <- params |>
    dplyr::filter(category == "prevalence", quantity == "aa_prevalence", group == "average") |>
    dplyr::arrange(age_min)
  idx <- findInterval(age, tab$age_min, all.inside = FALSE)
  idx <- pmin(pmax(idx, 1), nrow(tab))
  base <- tab$value[idx]
  rr_group <- if (group == "average") 1 else {
    key <- if (is_lynch(group)) "lynch" else group
    param_value(params, paste0("rr_aa_", key))
  }
  pmin(base * rr_group * lifestyle_rr(lifestyle, params), 0.6)
}
