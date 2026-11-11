test_that("simple math is the class-card formula and reproduces the card answers", {
  r <- predict_ldl(TYPICAL, EGGS, 95, default_knobs(P), P, "card")
  expect_equal(r$avg_delta, (29 - 12) * 1.3 + (900 - 300) / 100 * 2)
  expect_equal(unname(r$components[c("fat_type", "fiber", "sterol")]), c(0, 0, 0))
  # SFA %, cholesterol mg/day -> card answer (Nov 2026 cards)
  cards <- list(c(29, 900, 34), c(28, 700, 29), c(22, 450, 16), c(38, 400, 36))
  for (cc in cards) {
    a <- TYPICAL; a$sfa <- cc[1]; a$chol <- cc[2]
    expect_equal(round(predict_ldl(TYPICAL, a, 95, default_knobs(P), P, "card")$avg_delta), cc[3])
  }
})

test_that("no diet change means no change, for every knob setting", {
  k <- knobs_with(abs = 0.8, synth = 0.4, apoe = "e4e4", ldlr = 0.5)
  for (mode in c("card", "full")) {
    r <- predict_ldl(TYPICAL, TYPICAL, 95, k, P, mode)
    expect_equal(r$avg_after, 95)
    expect_equal(r$person_after, 95)
  }
})

test_that("knobs at their defaults equal the average-person prediction", {
  for (id in diets$diet_id) for (mode in c("card", "full")) for (split in c(FALSE, TRUE)) {
    for (ldl0 in c(70, 95, 190)) {
      r <- predict_ldl(TYPICAL, diet_from_row(diets, id), ldl0, default_knobs(P), P, mode, split)
      expect_equal(r$person_after, r$avg_after, tolerance = 1e-9)
      expect_equal(unname(r$knob_steps), rep(0, 4), tolerance = 1e-9)
    }
  }
})

test_that("waterfall knob steps add up to the gap", {
  k <- knobs_with(abs = 0.75, synth = 0.8, apoe = "e3e4", ldlr = 0.8, de_on = TRUE, de_flux = 1.5)
  for (id in diets$diet_id) {
    r <- predict_ldl(TYPICAL, diet_from_row(diets, id), 95, k, P)
    expect_equal(sum(r$knob_steps), r$person_delta - r$avg_delta, tolerance = 1e-9)
  }
})

test_that("each knob pushes a rise the expected way", {
  base <- predict_ldl(TYPICAL, EGGS, 95, default_knobs(P), P)$person_after
  up <- function(...) predict_ldl(TYPICAL, EGGS, 95, knobs_with(...), P)$person_after
  expect_gt(up(abs = 0.8), base)
  expect_lt(up(abs = 0.2), base)
  expect_lt(up(synth = 2.5), base)
  expect_gt(up(synth = 0.3), base)
  expect_gt(up(apoe = "e4e4"), base)
  expect_lt(up(apoe = "e2e3"), base)
  expect_gt(up(ldlr = 0.5), base)
  expect_gt(up(de_on = TRUE, de_flux = 2), base)
})

test_that("a high absorber's egg-driven rise is mostly site B", {
  r <- predict_ldl(TYPICAL, EGGS, 95, knobs_with(abs = 0.78), P)
  expect_equal(names(which.max(r$knob_steps)), "B")
})

test_that("a low absorber with lots of synthesis offsets most of the cholesterol term", {
  after <- TYPICAL; after$chol <- 900
  r <- predict_ldl(TYPICAL, after, 95, knobs_with(abs = 0.25, synth = 2), P)
  expect_lt(r$person_delta, 0.1 * r$avg_delta)
})

test_that("LDL falls on the vegan keto diet, more in the full model", {
  vegan <- diet_from_row(diets, "keto_vegan")
  card <- predict_ldl(TYPICAL, vegan, 95, default_knobs(P), P, "card")
  full <- predict_ldl(TYPICAL, vegan, 95, default_knobs(P), P, "full")
  expect_lt(card$avg_delta, 0)
  expect_lt(full$avg_delta, card$avg_delta)
  expect_gt(full$avg_after, 0)
})

test_that("out-of-range warnings fire for SFA above 20% of energy", {
  d <- TYPICAL
  expect_length(range_warnings(d, P), 0)
  d$sfa <- 21
  expect_true(any(grepl("SFA", range_warnings(d, P))))
  for (id in c("keto_eggs_butter", "keto_olive_fish", "keto_coconut")) {
    expect_true(any(grepl("SFA", range_warnings(diet_from_row(diets, id), P))), label = id)
  }
  expect_false(any(grepl("SFA", range_warnings(diet_from_row(diets, "keto_vegan"), P))))
  expect_length(range_warnings(diet_from_row(diets, "mediterranean"), P), 0)
})

test_that("compensation helper behaves", {
  expect_equal(comp_from_lathosterol(1, P[["abs_avg"]], P), P[["comp_avg"]])
  expect_gt(comp_from_lathosterol(1.7, 0.3, P), 0.9)
  expect_lt(comp_from_lathosterol(0.9, 0.78, P), 0.4)
  expect_lte(comp_from_lathosterol(5, 0.2, P), P[["comp_max"]])
  expect_gte(comp_from_lathosterol(0.3, 0.8, P), 0)
})

test_that("person model is vectorised over components and knobs", {
  comp <- predict_ldl(TYPICAL, EGGS, 95, default_knobs(P), P)$components
  m <- rbind(comp, comp * 0.5)
  colnames(m) <- COMPONENTS
  v <- person_ldl(m, 95, default_knobs(P), P)
  expect_equal(v, c(95 + sum(comp), 95 + sum(comp) * 0.5))
  k <- knobs_with(abs = c(0.3, 0.7))
  expect_length(person_ldl(comp, 95, k, P), 2)
})

test_that("ApoB estimate and LDL-C categories", {
  expect_equal(apob_estimate(c(95, 220, 240, 112), P), c(72, 150, 160, 82), tolerance = 0.05)
  expect_equal(as.character(ldl_category(c(99, 100, 159, 160, 190))),
               c("optimal", "near optimal", "borderline high", "high", "very high"))
})
