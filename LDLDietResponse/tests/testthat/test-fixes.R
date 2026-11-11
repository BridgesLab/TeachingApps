eggs_after <- EGGS

test_that("an SFA -> MUFA/PUFA swap is worth 1.6 / 2.1 mg/dL per 1% energy (fix menu)", {
  for (to in c("mufa", "pufa")) {
    fx <- no_fixes(); fx$swap_pct <- 10; fx$swap_to <- to
    e <- evaluate_fixes(TYPICAL, eggs_after, 95, default_knobs(P), fx, P, "card")
    expect_equal(e$table$average, -10 * P[[paste0("beta_sfa_", to)]], tolerance = 1e-9)
  }
})

test_that("a nut-butter swap of 13% of energy gives about -21 to -27 mg/dL", {
  fx <- no_fixes(); fx$swap_pct <- 13; fx$swap_to <- "nut"
  e <- evaluate_fixes(TYPICAL, diet_from_row(diets, "keto_coconut"), 95, default_knobs(P), fx, P, "card")
  expect_gte(e$table$average, -27)
  expect_lte(e$table$average, -21)
})

test_that("dropping eggs removes 186 mg each", {
  fx <- no_fixes(); fx$eggs <- 2
  a <- apply_fixes(eggs_after, default_knobs(P), fx, P)
  expect_equal(a$diet$chol, 900 - 2 * 186)
  e <- evaluate_fixes(TYPICAL, eggs_after, 95, default_knobs(P), fx, P, "card")
  expect_equal(e$table$average, -2 * 186 / 100 * 2)
})

test_that("fewer eggs helps a high absorber more; adding carbs back undoes D/E", {
  fx <- no_fixes(); fx$eggs <- 3
  eggs_effect <- function(k) evaluate_fixes(TYPICAL, EGGS, 95, k, fx, P)$table$person
  expect_lt(eggs_effect(knobs_with(abs = 0.78, synth = 0.9)), eggs_effect(knobs_with(abs = 0.3, synth = 1.7)))
  fx <- no_fixes(); fx$carbs <- TRUE
  runner <- knobs_with(de_on = TRUE, de_flux = 2.15)
  olive <- diet_from_row(diets, "keto_olive_fish")
  expect_lt(evaluate_fixes(TYPICAL, olive, 95, runner, fx, P)$table$person, -50)
})

test_that("drugs lower LDL-C; ezetimibe works better in a high absorber", {
  fx <- no_fixes(); fx$statin <- TRUE
  e <- evaluate_fixes(TYPICAL, eggs_after, 95, default_knobs(P), fx, P)
  expect_equal(e$all[["average"]], e$base[["average"]] * (1 - P[["statin_pct"]] / 100))
  fx <- no_fixes(); fx$ezetimibe <- TRUE
  hi <- default_knobs(P); hi$abs <- 0.8
  lo <- default_knobs(P); lo$abs <- 0.3
  r_hi <- evaluate_fixes(TYPICAL, eggs_after, 95, hi, fx, P)
  r_lo <- evaluate_fixes(TYPICAL, eggs_after, 95, lo, fx, P)
  expect_lt(r_hi$all[["person"]] / r_hi$base[["person"]], r_lo$all[["person"]] / r_lo$base[["person"]])
})

test_that("no fixes means no change", {
  e <- evaluate_fixes(TYPICAL, eggs_after, 95, default_knobs(P), no_fixes(), P)
  expect_null(e$table)
  expect_equal(e$all, e$base)
})

test_that("the food builder turns eggs into cholesterol and fat into lost carbohydrate", {
  s <- c(egg = 4, butter = 2)
  d <- diet_from_foods(TYPICAL, foods, s)
  expect_equal(d$chol, 300 + 4 * 186 + 2 * 31)
  expect_gt(d$sfa, TYPICAL$sfa)
  expect_lt(diet_carb(d), diet_carb(TYPICAL))
  expect_lt(d$fiber, TYPICAL$fiber)
})
