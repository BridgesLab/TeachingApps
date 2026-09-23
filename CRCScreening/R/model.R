# Scenario model: settings -> inputs (point or Monte Carlo draws) -> outcomes
#
# Pipeline:
#   sc     <- build_scenario(params, age = 25, group = "average", test = "cologuard")
#   inputs <- scenario_inputs(sc, n_draws = 0)     # one row per age (point estimate)
#   inputs <- scenario_inputs(sc, n_draws = 2000)  # one row per age x draw
#   out    <- evaluate_scenario(inputs, sc)
#
# `inputs` is the seam for a future Bayesian extension: any source of draws
# (e.g. posterior draws from a brms model of test accuracy or incidence) can
# be used as long as it returns a tibble with these columns:
#   draw, age, prev_crc, prev_aa,          pre-test state probabilities
#   p_crc, p_aa, p_none,                   P(test +) by state for the chosen test
#   s_crc, s_aa, s_none,                   P(colonoscopy + biopsy positive) by state
#   c_crc, c_aa,                           P(colonoscopy finds lesion) by state
#   perf, bleed                            harms per 10,000 colonoscopies

#' Test accuracy expressed as P(positive) in each of the three states
#'
#' For "cancer only", people with an advanced adenoma test positive at the
#' adenoma sensitivity (stool/blood tests cannot tell the difference), unless
#' the test has a target-specific row (colonoscopy with biopsy).
test_accuracy <- function(params, test, target = c("crc", "crc_aa")) {
  target <- match.arg(target)
  acc <- params |> dplyr::filter(category == "test_accuracy", .data$test == .env$test)
  if (nrow(acc) == 0) stop("No accuracy parameters for test '", test, "'")

  pick <- function(q) {
    rows <- acc[acc$quantity == q, , drop = FALSE]
    specific <- rows[rows$target == target, , drop = FALSE]
    if (nrow(specific) == 1) return(specific)
    generic <- rows[rows$target == "any", , drop = FALSE]
    if (nrow(generic) == 1) generic else NULL
  }

  crc  <- pick("sens_crc")
  aa   <- if (target == "crc") pick("pos_aa_crc_target") %||% pick("sens_aa") else pick("sens_aa")
  spec <- pick("spec")
  if (is.null(crc) || is.null(aa) || is.null(spec)) stop("Incomplete accuracy parameters for ", test)

  tibble::tibble(
    state    = c("crc", "aa", "none"),
    quantity = c(crc$quantity, aa$quantity, "1 - spec"),
    param_id = c(crc$param_id, aa$param_id, spec$param_id),
    p_pos    = c(crc$value, aa$value, 1 - spec$value),
    # P(positive | no neoplasia) = 1 - spec ~ Beta(beta_spec, alpha_spec)
    alpha    = c(crc$alpha, aa$alpha, spec$beta),
    beta     = c(crc$beta, aa$beta, spec$alpha)
  )
}

#' Default slider values (sensitivity for cancer, for adenoma, specificity)
accuracy_defaults <- function(params, test, target = "crc") {
  a <- test_accuracy(params, test, "crc_aa")
  b <- test_accuracy(params, test, target)
  list(sens_crc = a$p_pos[1], sens_aa = a$p_pos[2], spec = 1 - b$p_pos[3])
}

#' Replace point values with user-chosen ones, keeping each Beta's effective
#' sample size so the uncertainty stays comparable
override_accuracy <- function(acc, override) {
  new_p <- c(
    override$sens_crc,
    if (acc$quantity[2] == "sens_aa") override$sens_aa else acc$p_pos[2],
    1 - override$spec
  )
  s <- beta_from_mean_ess(new_p, acc$alpha + acc$beta)
  acc$p_pos <- new_p
  acc$alpha <- s$alpha
  acc$beta  <- s$beta
  acc
}

#' Collect everything needed to evaluate one testing scenario
#'
#' `age` may be a vector (used for PPV-by-age curves).
build_scenario <- function(params, age, group = "average", fdr_age = 50,
                           lifestyle = character(), symptoms = "none",
                           test = "cologuard", target = "crc",
                           prev_override = NULL, accuracy_override = NULL) {
  prev_crc <- if (is.null(prev_override)) {
    crc_prevalence(age, group, params, fdr_age, lifestyle, symptoms)
  } else {
    rep(prev_override, length(age))
  }
  prev_aa <- pmin(aa_prevalence(age, group, params, lifestyle), 1 - prev_crc)

  acc <- test_accuracy(params, test, target)
  if (!is.null(accuracy_override)) acc <- override_accuracy(acc, accuracy_override)

  list(
    settings = list(age = age, group = group, fdr_age = fdr_age, lifestyle = lifestyle,
                    symptoms = symptoms, test = test, target = target,
                    prev_override = prev_override),
    age      = age,
    target   = target,
    prev_crc = prev_crc,
    prev_aa  = prev_aa,
    prev_cv  = c(crc = param_value(params, "prev_cv_crc"), aa = param_value(params, "prev_cv_aa")),
    acc      = acc,
    confirm  = test_accuracy(params, "colonoscopy", target),   # follow-up colonoscopy + biopsy
    detect   = test_accuracy(params, "colonoscopy", "crc_aa"), # what a colonoscopy finds
    harms    = list(perf = param_row(params, "harm_perforation"),
                    bleed = param_row(params, "harm_major_bleed")),
    is_colonoscopy = test == "colonoscopy"
  )
}

#' Point-estimate (n_draws = 0) or Monte Carlo inputs for a scenario
scenario_inputs <- function(sc, n_draws = 0, seed = 2026) {
  n_age <- length(sc$age)
  if (n_draws == 0) {
    return(tibble::tibble(
      draw = 0L, age = sc$age, prev_crc = sc$prev_crc, prev_aa = sc$prev_aa,
      p_crc = sc$acc$p_pos[1], p_aa = sc$acc$p_pos[2], p_none = sc$acc$p_pos[3],
      s_crc = sc$confirm$p_pos[1], s_aa = sc$confirm$p_pos[2], s_none = sc$confirm$p_pos[3],
      c_crc = sc$detect$p_pos[1], c_aa = sc$detect$p_pos[2],
      perf = sc$harms$perf$value, bleed = sc$harms$bleed$value
    ))
  }

  withr::with_seed(seed, {
    rb <- function(tab, i) stats::rbeta(n_draws, tab$alpha[i], tab$beta[i])
    # test accuracy and harms: one draw per simulation, shared across ages
    shared <- tibble::tibble(
      draw = seq_len(n_draws),
      p_crc = rb(sc$acc, 1), p_aa = rb(sc$acc, 2), p_none = rb(sc$acc, 3),
      s_crc = rb(sc$confirm, 1), s_aa = rb(sc$confirm, 2), s_none = rb(sc$confirm, 3),
      c_crc = rb(sc$detect, 1), c_aa = rb(sc$detect, 2),
      perf = draw_param(n_draws, sc$harms$perf),
      bleed = draw_param(n_draws, sc$harms$bleed)
    )
    tidyr::expand_grid(draw = seq_len(n_draws), i = seq_len(n_age)) |>
      dplyr::mutate(
        age = sc$age[i],
        prev_crc = rbeta_mean_cv(dplyr::n(), sc$prev_crc[i], sc$prev_cv[["crc"]]),
        prev_aa  = rbeta_mean_cv(dplyr::n(), sc$prev_aa[i], sc$prev_cv[["aa"]]),
        prev_aa  = pmin(prev_aa, 1 - prev_crc)
      ) |>
      dplyr::select(-i) |>
      dplyr::left_join(shared, by = "draw")
  })
}

#' Outcomes per `n` people tested, for every row of `inputs`
evaluate_scenario <- function(inputs, sc, n = 10000) {
  out <- classify_population(inputs$prev_crc, inputs$prev_aa,
                             inputs$p_crc, inputs$p_aa, inputs$p_none,
                             target = sc$target, n = n)
  # positive test followed by colonoscopy + biopsy (conditionally independent)
  seq_out <- classify_population(inputs$prev_crc, inputs$prev_aa,
                                 inputs$p_crc * inputs$s_crc,
                                 inputs$p_aa * inputs$s_aa,
                                 inputs$p_none * inputs$s_none,
                                 target = sc$target, n = n)

  colo <- sc$is_colonoscopy
  colonoscopies <- if (colo) rep(n, nrow(out)) else out$positives
  crc_found <- if (colo) out$n_crc * inputs$c_crc else out$pos_crc * inputs$c_crc
  aa_found  <- if (colo) out$n_aa * inputs$c_aa else out$pos_aa * inputs$c_aa

  dplyr::bind_cols(
    dplyr::select(inputs, draw, age, prev_crc, prev_aa),
    dplyr::select(out, -prev_target),
    tibble::tibble(
      prev_target = out$prev_target,
      ppv_seq       = if (colo) NA_real_ else seq_out$ppv,
      colonoscopies = colonoscopies,
      crc_found     = crc_found,
      crc_missed    = out$n_crc - crc_found,
      aa_found      = aa_found,
      fp_with_aa    = if (sc$target == "crc") out$pos_aa else 0,
      perforations  = colonoscopies * inputs$perf / 1e4,
      bleeds        = colonoscopies * inputs$bleed / 1e4,
      colos_per_crc_found = colonoscopies / crc_found
    )
  )
}

run_scenario <- function(sc, n_draws = 0, n = 10000, seed = 2026) {
  evaluate_scenario(scenario_inputs(sc, n_draws, seed), sc, n)
}

#' PPV across ages for several risk profiles (optionally with intervals)
#'
#' @param profiles tibble with columns profile, group, fdr_age, lifestyle (list)
ppv_by_age <- function(params, profiles, ages = 20:80, symptoms = "none",
                       test = "cologuard", target = "crc",
                       accuracy_override = NULL, n_draws = 0, seed = 2026) {
  purrr::pmap(profiles, function(profile, group, fdr_age, lifestyle) {
    sc <- build_scenario(params, ages, group, fdr_age, lifestyle, symptoms,
                         test, target, accuracy_override = accuracy_override)
    point <- run_scenario(sc) |> dplyr::select(age, ppv, prev_crc)
    if (n_draws > 0) {
      bands <- run_scenario(sc, n_draws, seed = seed) |>
        dplyr::group_by(age) |>
        dplyr::summarise(lower = stats::quantile(ppv, 0.025, na.rm = TRUE),
                         upper = stats::quantile(ppv, 0.975, na.rm = TRUE),
                         .groups = "drop")
      point <- dplyr::left_join(point, bands, by = "age")
    }
    dplyr::mutate(point, profile = profile, group = group, .before = 1)
  }) |>
    purrr::list_rbind()
}

#' Standard set of comparison profiles for the PPV-by-age plot
standard_profiles <- function(fdr_age = 50, group = "average", lifestyle = character()) {
  base <- tibble::tibble(
    profile = names(GROUP_CHOICES),
    group = unname(GROUP_CHOICES),
    fdr_age = fdr_age,
    lifestyle = list(character())
  )
  if (length(lifestyle) > 0) {
    base <- dplyr::bind_rows(base, tibble::tibble(
      profile = "Your profile (with lifestyle)", group = group,
      fdr_age = fdr_age, lifestyle = list(lifestyle)
    ))
  }
  base
}
