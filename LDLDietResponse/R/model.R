# The model --------------------------------------------------------------------
#
# 1. Average person: linear regression terms (Mensink 2016 etc.), in mg/dL.
#    "full" math (default): SFA, MUFA, PUFA and trans fat vs carbohydrate, SFA
#    subtypes (optional), cholesterol, soluble fiber and plant sterols.
#    "card" (simple) math is the class-card formula: 1.3 x dSFA + 2 x dChol/100.
# 2. This person: LDL-C = production / clearance. Each average term is turned
#    into the share of LDL receptor activity it removes, so that with every knob
#    at its default the person equals the average person exactly. Knobs then
#    scale those shares (B absorption and the compensation it leaves C's
#    synthesis to do, for cholesterol and sterols; F APOE and LDL receptor
#    activity for everything) and D/E scales production (VLDL -> LDL flux).

COMPONENTS <- c("sfa", "fat_type", "chol", "fiber", "sterol")

COMPONENT_LABELS <- c(
  sfa = "A · Saturated fat",
  fat_type = "A · Fat type (MUFA, PUFA, trans, SFA split)",
  chol = "A · Dietary cholesterol",
  fiber = "Soluble fiber",
  sterol = "Plant sterols"
)

#' Fat and carbohydrate (% energy) implied by a diet
diet_fat <- function(d) d$sfa + d$mufa + d$pufa + d$trans
diet_carb <- function(d) 100 - d$protein - diet_fat(d)
diet_sfa_other <- function(d) d$sfa - d$sfa_lm - d$sfa_p - d$sfa_s

#' SFA term from the subtypes (mg/dL), relative to carbohydrate
sfa_split_term <- function(d, p) {
  p[["beta_lm_cho"]] * d$sfa_lm + p[["beta_palmitic_cho"]] * d$sfa_p +
    p[["beta_stearic_cho"]] * d$sfa_s + p[["beta_sfa_cho"]] * diet_sfa_other(d)
}

#' Average-person LDL-C change by component (mg/dL)
#'
#' @param before,after diets (named lists with DIET_FIELDS)
#' @param mode "card" (as on the Activity cards) or "full"
#' @param split use the SFA subtypes (full mode only)
avg_components <- function(before, after, p, ldl0, mode = c("card", "full"), split = FALSE) {
  mode <- match.arg(mode)
  d <- function(x) after[[x]] - before[[x]]
  out <- c(sfa = p[["beta_sfa_cho"]] * d("sfa"),
           fat_type = 0,
           chol = p[["beta_chol"]] * d("chol") / 100,
           fiber = 0, sterol = 0)
  if (mode == "full") {
    b_mufa <- p[["beta_sfa_cho"]] - p[["beta_sfa_mufa"]]
    b_pufa <- p[["beta_sfa_cho"]] - p[["beta_sfa_pufa"]]
    sub_adj <- if (split) {
      (sfa_split_term(after, p) - sfa_split_term(before, p)) - out[["sfa"]]
    } else 0
    out[["fat_type"]] <- sub_adj + b_mufa * d("mufa") + b_pufa * d("pufa") +
      p[["beta_trans_cho"]] * d("trans")
    out[["fiber"]] <- p[["beta_fiber"]] * d("fiber")
    cap <- p[["sterol_cap_g"]]
    out[["sterol"]] <- -p[["sterol_pct_per_g"]] / 100 * ldl0 *
      (min(after$sterol, cap) - min(before$sterol, cap))
  }
  out
}

# Person knobs -------------------------------------------------------------------

#' Knobs for the average person
default_knobs <- function(p) {
  list(abs = p[["abs_avg"]], synth = 1, apoe = "e3e3", ldlr = 1,
       de_on = FALSE, de_flux = 1)
}

apoe_mult <- function(apoe, p) unname(p[paste0("apoe_", apoe)])

#' Compensation: the fraction of extra absorbed cholesterol offset by making less
#'
#' Depends on how much arrives (absorption) against how much synthesis there is
#' to switch off (site C: baseline lathosterol, x typical).
comp_from_lathosterol <- function(lath0, abs, p) {
  ratio <- (abs / p[["abs_avg"]]) / lath0
  comp <- 1 - (1 - p[["comp_avg"]]) * ratio^p[["comp_lath_exponent"]]
  pmin(pmax(comp, 0), p[["comp_max"]])
}

#' Multipliers implied by a set of knobs (vectorised over knob values)
knob_factors <- function(knobs, p) {
  b <- knobs$abs / p[["abs_avg"]]
  c_fac <- (1 - comp_from_lathosterol(knobs$synth, knobs$abs, p)) / (1 - p[["comp_avg"]])
  list(absorption = b, compensation = c_fac, chol = b * c_fac,
       ldlr = apoe_mult(knobs$apoe, p) / knobs$ldlr,
       flux = ifelse(knobs$de_on, knobs$de_flux, 1))
}

#' Components as a matrix (one row per draw)
as_comp_matrix <- function(comp) {
  if (is.matrix(comp)) comp else matrix(comp[COMPONENTS], nrow = 1, dimnames = list(NULL, COMPONENTS))
}

#' Share of LDL receptor activity this person loses to the diet change
#'
#' For the average person, LDL-C after = ldl0 / (1 - share), so
#' share = (sum of average terms) / (average LDL-C after). Negative = gained.
#' Vectorised over rows of comp and over knob values.
person_lost <- function(comp, ldl0, knobs, p) {
  m <- as_comp_matrix(comp)
  share <- m / pmax(ldl0 + rowSums(m), 1)
  f <- knob_factors(knobs, p)
  col <- function(x) unname(share[, x])
  f$ldlr * (col("sfa") + col("fat_type") + col("fiber") + f$chol * (col("chol") + col("sterol")))
}

#' This person's LDL-C after the diet change (mg/dL); vectorised over knobs
person_ldl <- function(comp, ldl0, knobs, p) {
  lost <- person_lost(comp, ldl0, knobs, p)
  ldl0 * knob_factors(knobs, p)$flux / pmax(1 - lost, p[["min_clearance"]])
}

#' Average and person predictions, with the knob steps for the waterfall
#'
#' Knob steps switch knobs on one at a time in decoder order (B, C, D/E, F), so
#' each step is what that site adds once the earlier sites are set.
predict_ldl <- function(before, after, ldl0, knobs, p, mode = "full", split = FALSE) {
  comp <- avg_components(before, after, p, ldl0, mode, split)
  avg_after <- ldl0 + sum(comp)
  k <- default_knobs(p)
  steps <- c()
  prev <- person_ldl(comp, ldl0, k, p)
  k$abs <- knobs$abs;   v <- person_ldl(comp, ldl0, k, p); steps["B"] <- v - prev; prev <- v
  k$synth <- knobs$synth; v <- person_ldl(comp, ldl0, k, p); steps["C"] <- v - prev; prev <- v
  k$de_on <- knobs$de_on; k$de_flux <- knobs$de_flux
  v <- person_ldl(comp, ldl0, k, p); steps["D/E"] <- v - prev; prev <- v
  k$apoe <- knobs$apoe; k$ldlr <- knobs$ldlr
  v <- person_ldl(comp, ldl0, k, p); steps["F"] <- v - prev
  list(ldl0 = ldl0, components = comp, avg_after = avg_after, avg_delta = sum(comp),
       person_after = v, person_delta = v - ldl0, knob_steps = steps,
       capped = 1 - person_lost(comp, ldl0, knobs, p) < p[["min_clearance"]])
}

# Benchmarks ---------------------------------------------------------------------

apob_estimate <- function(ldl, p) p[["apob_intercept"]] + p[["apob_slope"]] * ldl

ldl_category <- function(ldl) {
  cut(ldl, c(-Inf, 100, 130, 160, 190, Inf), right = FALSE,
      labels = c("optimal", "near optimal", "borderline high", "high", "very high"))
}

#' Messages for inputs outside the ranges behind the coefficients
range_warnings <- function(d, p, which = "after") {
  msgs <- c()
  add <- function(cond, txt) if (isTRUE(cond)) msgs <<- c(msgs, paste0(which, ": ", txt))
  add(d$sfa > p[["range_sfa_max"]], sprintf(
    "SFA is %g%% of energy, above the ~%g%% of most trials behind the 1.3 mg/dL coefficient. The straight line is being stretched.",
    d$sfa, p[["range_sfa_max"]]))
  add(diet_carb(d) < p[["range_carb_min"]], sprintf(
    "carbohydrate is %g%% of energy. The fat coefficients were measured by swapping fat for carbohydrate within ordinary diets (above ~%g%%), not keto.",
    round(diet_carb(d), 1), p[["range_carb_min"]]))
  add(d$mufa > p[["range_mufa_max"]], sprintf("MUFA (%g%% of energy) is above most trials.", d$mufa))
  add(d$pufa > p[["range_pufa_max"]], sprintf("PUFA (%g%% of energy) is above most trials.", d$pufa))
  add(d$chol > p[["range_chol_max"]], sprintf(
    "%g mg/day cholesterol is beyond the top egg-trial dose group (Berger 2015: the rise stopped climbing with dose).", d$chol))
  add(d$fiber > p[["range_fiber_max"]], sprintf("%g g/day soluble fiber is above the 2-10 g/day studied.", d$fiber))
  add(d$sterol > p[["range_sterol_max"]], sprintf("%g g/day plant sterols is past the plateau (~3 g/day).", d$sterol))
  add(diet_carb(d) < 0, "fat and protein add up to more than 100% of energy.")
  msgs
}
