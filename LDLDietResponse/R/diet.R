# Food builder and the Fix it menu ---------------------------------------------

#' Build an "after" diet from servings/day of foods added to the before diet
#'
#' Each serving's fat energy replaces the same energy from carbohydrate foods
#' (energy stays the same); fiber from those carbohydrate foods falls in
#' proportion. Protein and the foods' own carbohydrate are ignored.
diet_from_foods <- function(before, foods, servings) {
  servings <- servings[foods$food_id]
  servings[is.na(servings)] <- 0
  g <- function(col) sum(foods[[col]] * servings)
  pct <- function(grams) 100 * grams * 9 / before$energy
  after <- before
  after$sfa <- before$sfa + pct(g("sfa_g"))
  after$sfa_lm <- before$sfa_lm + pct(g("sfa_lm_g"))
  after$sfa_p <- before$sfa_p + pct(g("sfa_p_g"))
  after$sfa_s <- before$sfa_s + pct(g("sfa_s_g"))
  after$mufa <- before$mufa + pct(g("mufa_g"))
  after$pufa <- before$pufa + pct(g("pufa_g"))
  after$trans <- before$trans + pct(g("trans_g"))
  after$chol <- before$chol + g("chol_mg")
  carb_left <- max(diet_carb(after), 0) / diet_carb(before)
  after$fiber <- before$fiber * carb_left + g("fiber_g")
  after$sterol <- before$sterol + g("sterol_g")
  lapply(after, function(x) if (is.numeric(x)) round(x, 1) else x)
}

# Fix it -------------------------------------------------------------------------

FIX_INFO <- data.frame(
  fix = c("swap", "eggs", "psyllium", "sterols", "ezetimibe", "statin", "carbs"),
  label = c("Swap the fat", "Fewer eggs", "Psyllium (fiber)", "Plant sterol spread",
            "Ezetimibe", "Statin", "Add carbs back"),
  site = c("A", "A → B", "B → C", "B", "B", "C → F", "D/E"),
  how = c("SFA out, MUFA/PUFA in: fewer LDL receptors switched off",
          "Less cholesterol reaches the gut",
          "Bile acids trapped; liver uses its own cholesterol and makes more LDL receptors",
          "Plant sterols compete with cholesterol at NPC1L1",
          "Blocks NPC1L1, so less cholesterol is absorbed",
          "Blocks HMG-CoA reductase; SREBP2 turns on more LDL receptors",
          "Ends ketosis and refills glycogen; turns the D/E switch off"),
  stringsAsFactors = FALSE
)

#' Settings with nothing switched on
no_fixes <- function() {
  list(swap_pct = 0, swap_to = "nut", eggs = 0, psyllium = 0, sterols = FALSE,
       ezetimibe = FALSE, statin = FALSE, carbs = FALSE)
}

#' Apply fixes to the after diet and knobs; returns the diet, knobs and drug factors
apply_fixes <- function(after, knobs, fx, p) {
  d <- after
  if (fx$swap_pct > 0 && d$sfa > 0) {
    x <- min(fx$swap_pct, d$sfa)
    keep <- (d$sfa - x) / d$sfa
    d$sfa_lm <- d$sfa_lm * keep; d$sfa_p <- d$sfa_p * keep; d$sfa_s <- d$sfa_s * keep
    d$sfa <- d$sfa - x
    to_mufa <- switch(fx$swap_to, mufa = 1, pufa = 0, nut = 2 / 3)
    d$mufa <- d$mufa + x * to_mufa
    d$pufa <- d$pufa + x * (1 - to_mufa)
  }
  if (fx$eggs > 0) d$chol <- max(d$chol - fx$eggs * p[["egg_chol_mg"]], 0)
  d$fiber <- d$fiber + fx$psyllium
  if (isTRUE(fx$sterols)) d$sterol <- d$sterol + p[["sterol_spread_g"]]
  if (isTRUE(fx$carbs)) {
    x <- 100 * p[["carbs_back_g"]] * 4 / d$energy
    fat <- diet_fat(d)
    keep <- max(fat - x, 0) / fat
    for (f in c("sfa", "sfa_lm", "sfa_p", "sfa_s", "mufa", "pufa", "trans")) d[[f]] <- d[[f]] * keep
    knobs$de_on <- FALSE
  }
  b <- knobs$abs / p[["abs_avg"]]
  drug_avg <- (if (isTRUE(fx$ezetimibe)) 1 - p[["ezetimibe_pct"]] / 100 else 1) *
    (if (isTRUE(fx$statin)) 1 - p[["statin_pct"]] / 100 else 1)
  drug_person <- (if (isTRUE(fx$ezetimibe)) 1 - min(p[["ezetimibe_pct"]] / 100 * b, 0.6) else 1) *
    (if (isTRUE(fx$statin)) 1 - p[["statin_pct"]] / 100 else 1)
  list(diet = d, knobs = knobs, drug_avg = drug_avg, drug_person = drug_person)
}

#' LDL-C with fixes. Before -> after uses the chosen math; the fix itself is
#' always scored with the full model (MUFA/PUFA count), as on the fix menu.
predict_with_fixes <- function(before, after, ldl0, knobs, fx, p, mode = "full", split = FALSE) {
  a <- apply_fixes(after, knobs, fx, p)
  comp <- avg_components(before, after, p, ldl0, mode, split) +
    avg_components(after, a$diet, p, ldl0, "full", split)
  c(average = (ldl0 + sum(comp)) * a$drug_avg,
    person = person_ldl(comp, ldl0, a$knobs, p) * a$drug_person)
}

#' Each fix on its own, plus everything chosen together
evaluate_fixes <- function(before, after, ldl0, knobs, fx, p, mode = "full", split = FALSE) {
  base <- predict_with_fixes(before, after, ldl0, knobs, no_fixes(), p, mode, split)
  one <- function(fix) {
    f <- no_fixes()
    if (fix == "swap") { f$swap_pct <- fx$swap_pct; f$swap_to <- fx$swap_to }
    if (fix == "eggs") f$eggs <- fx$eggs
    if (fix == "psyllium") f$psyllium <- fx$psyllium
    if (fix %in% c("sterols", "ezetimibe", "statin", "carbs")) f[[fix]] <- isTRUE(fx[[fix]])
    predict_with_fixes(before, after, ldl0, knobs, f, p, mode, split)
  }
  chosen <- FIX_INFO$fix[c(fx$swap_pct > 0, fx$eggs > 0, fx$psyllium > 0, isTRUE(fx$sterols),
                           isTRUE(fx$ezetimibe), isTRUE(fx$statin), isTRUE(fx$carbs))]
  rows <- lapply(chosen, function(fix) {
    v <- one(fix)
    info <- FIX_INFO[FIX_INFO$fix == fix, ]
    data.frame(fix = info$label, site = info$site, how = info$how,
               average = v[["average"]] - base[["average"]],
               person = v[["person"]] - base[["person"]], stringsAsFactors = FALSE)
  })
  all <- predict_with_fixes(before, after, ldl0, knobs, fx, p, mode, split)
  list(base = base, all = all,
       table = if (length(rows)) do.call(rbind, rows) else NULL)
}
