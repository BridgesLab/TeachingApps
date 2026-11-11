# Spread of outcomes ------------------------------------------------------------
#
# Two sources of variance, drawn once per seed (common random numbers) so the
# breakdown is consistent:
#   1. Coefficient uncertainty: each regression coefficient with a 95% interval
#      in parameters.csv is drawn from a normal distribution matching it
#      (independently; in reality they are correlated).
#   2. Between people: absorption (B), synthesis (C), APOE genotype and LDL
#      receptor activity (F), drawn independently and centred on the average
#      person. D/E (a hypothesis) is not drawn. Baseline LDL-C is held fixed.

#' Ids of the coefficients that carry a 95% interval
coef_ids <- function(params) params$param_id[!is.na(params$lower)]

#' n x k matrix of coefficient draws
draw_coefs <- function(params, n) {
  rows <- params[!is.na(params$lower), ]
  sd <- (rows$upper - rows$lower) / (2 * stats::qnorm(0.975))
  m <- vapply(seq_len(nrow(rows)), function(j) stats::rnorm(n, rows$value[j], sd[j]), numeric(n))
  m <- matrix(m, nrow = n, dimnames = list(NULL, rows$param_id))
  m
}

#' Knob values for n people
draw_people <- function(p, n) {
  abs <- stats::plogis(stats::qlogis(p[["abs_avg"]]) + stats::rnorm(n, 0, p[["pop_abs_logit_sd"]]))
  synth <- exp(stats::rnorm(n, 0, p[["pop_synth_sdlog"]]))
  apoe <- sample(unname(APOE_CHOICES), n, replace = TRUE, prob = p[paste0("pop_freq_", APOE_CHOICES)])
  ldlr <- exp(stats::rnorm(n, 0, p[["pop_ldlr_sdlog"]]))
  list(abs = pmin(pmax(abs, p[["abs_min"]]), p[["abs_max"]]),
       synth = pmin(pmax(synth, p[["synth_min"]]), p[["synth_max"]]),
       apoe = apoe, ldlr = ldlr, de_on = FALSE, de_flux = 1)
}

#' Simulate LDL-C after the diet change under each source of variance
#'
#' @return list of numeric vectors (LDL-C after, mg/dL), one per scenario:
#'   avg_coef (average person, coefficient uncertainty), person_coef (this
#'   person's knobs, coefficient uncertainty), people (point coefficients,
#'   people drawn), both (people and coefficients), and one-knob-at-a-time
#'   vectors: knob_B, knob_C, knob_APOE, knob_LDLR.
simulate_spread <- function(before, after, ldl0, knobs, params, mode = "full", split = FALSE,
                            n = 2000, seed = 630) {
  p <- param_vector(params)
  draws <- withr::with_seed(seed, list(coefs = draw_coefs(params, n), people = draw_people(p, n)))
  comp_point <- avg_components(before, after, p, ldl0, mode, split)
  comp_coef <- t(vapply(seq_len(n), function(i) {
    pp <- p
    pp[colnames(draws$coefs)] <- draws$coefs[i, ]
    avg_components(before, after, pp, ldl0, mode, split)
  }, numeric(length(COMPONENTS))))
  colnames(comp_coef) <- COMPONENTS
  comp_rep <- matrix(comp_point[COMPONENTS], nrow = n, ncol = length(COMPONENTS), byrow = TRUE,
                     dimnames = list(NULL, COMPONENTS))
  avg_k <- default_knobs(p)
  one_knob <- function(field) {
    k <- avg_k
    k[[field]] <- draws$people[[field]]
    person_ldl(comp_rep, ldl0, k, p)
  }
  list(
    ldl0 = ldl0,
    avg_coef = ldl0 + rowSums(comp_coef),
    person_coef = person_ldl(comp_coef, ldl0, knobs, p),
    people = person_ldl(comp_rep, ldl0, draws$people, p),
    both = person_ldl(comp_coef, ldl0, draws$people, p),
    knob_B = one_knob("abs"),
    knob_C = one_knob("synth"),
    knob_APOE = one_knob("apoe"),
    knob_LDLR = one_knob("ldlr")
  )
}

#' 95% (or other) interval of a vector of LDL-C values, as changes from baseline
change_interval <- function(x, ldl0, level = 0.95) {
  a <- (1 - level) / 2
  stats::quantile(x - ldl0, c(a, 1 - a), names = FALSE)
}

#' Table of where the spread comes from (SD and middle-90% range of the change)
variance_table <- function(sp) {
  rows <- list(
    c("Uncertainty in the average-person coefficients", "avg_coef"),
    c("Differences between people (all knobs)", "people"),
    c("   B · absorption only", "knob_B"),
    c("   C · synthesis only", "knob_C"),
    c("   F · APOE genotype only", "knob_APOE"),
    c("   F · LDL receptor activity only", "knob_LDLR"),
    c("Both together", "both")
  )
  do.call(rbind, lapply(rows, function(r) {
    x <- sp[[r[2]]] - sp$ldl0
    q <- stats::quantile(x, c(0.05, 0.95), names = FALSE)
    data.frame(source = r[1], key = r[2], sd = stats::sd(x), lo = q[1], hi = q[2],
               stringsAsFactors = FALSE)
  }))
}

summarise_spread <- function(x, ldl0) {
  list(lo = stats::quantile(x - ldl0, 0.05, names = FALSE),
       hi = stats::quantile(x - ldl0, 0.95, names = FALSE),
       sd = stats::sd(x),
       pct_rise = 100 * mean(x > ldl0),
       pct_160 = 100 * mean(x >= 160),
       pct_190 = 100 * mean(x >= 190))
}
