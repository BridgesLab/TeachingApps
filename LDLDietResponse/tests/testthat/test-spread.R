test_that("no diet change means no spread", {
  sp <- simulate_spread(TYPICAL, TYPICAL, 95, default_knobs(P), params, n = 300, seed = 1)
  for (key in c("avg_coef", "person_coef", "people", "both", "knob_B", "knob_C", "knob_APOE", "knob_LDLR")) {
    expect_equal(sd(sp[[key]]), 0, label = key)
  }
})

test_that("coefficient draws centre on the point prediction", {
  r <- predict_ldl(TYPICAL, EGGS, 95, default_knobs(P), P)
  sp <- simulate_spread(TYPICAL, EGGS, 95, default_knobs(P), params, n = 4000, seed = 2)
  expect_equal(mean(sp$avg_coef), r$avg_after, tolerance = 0.01)
  ci <- change_interval(sp$avg_coef, 95)
  expect_lt(ci[1], r$avg_delta)
  expect_gt(ci[2], r$avg_delta)
})

test_that("between-person spread is centred near the average person", {
  r <- predict_ldl(TYPICAL, EGGS, 95, default_knobs(P), P)
  sp <- simulate_spread(TYPICAL, EGGS, 95, default_knobs(P), params, n = 4000, seed = 3)
  expect_equal(median(sp$people), r$avg_after, tolerance = 0.1)
  expect_gt(sd(sp$both), sd(sp$avg_coef))
})

test_that("variance breakdown points at the right knob for each diet", {
  vt <- function(id) {
    v <- variance_table(simulate_spread(TYPICAL, diet_from_row(diets, id), 95, default_knobs(P),
                                        params, n = 2000, seed = 4))
    k <- v[grepl("^knob_", v$key), ]
    k$key[which.max(k$sd)]
  }
  # egg-heavy: absorption/synthesis; coconut oil (SFA, little cholesterol): LDL receptor side
  expect_true(vt("keto_eggs_butter") %in% c("knob_B", "knob_C"))
  expect_true(vt("keto_coconut") %in% c("knob_APOE", "knob_LDLR"))
})

test_that("simulations are reproducible with a seed", {
  a <- simulate_spread(TYPICAL, EGGS, 95, default_knobs(P), params, n = 100, seed = 5)
  b <- simulate_spread(TYPICAL, EGGS, 95, default_knobs(P), params, n = 100, seed = 5)
  expect_identical(a, b)
})

test_that("simple math has no coefficient uncertainty from fats it ignores", {
  a <- TYPICAL; a$mufa <- 30
  sp <- simulate_spread(TYPICAL, a, 95, default_knobs(P), params, mode = "card", n = 200, seed = 6)
  expect_equal(sd(sp$avg_coef), 0)
})
