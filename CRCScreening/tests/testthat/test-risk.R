test_that("baseline hazard reproduces SEER band values at band midpoints", {
  tab <- incidence_table(params)
  expect_equal(baseline_hazard(tab$age_mid, params), tab$rate)
  expect_true(all(diff(baseline_hazard(20:84, params)) >= 0))
})

test_that("FDR relative risk depends on the relative's age at diagnosis", {
  expect_equal(rr_fdr(45, params), param_value(params, "rr_fdr_dx_lt50"))
  expect_equal(rr_fdr(50, params), param_value(params, "rr_fdr_dx_50_59"))
  expect_equal(rr_fdr(59.5, params), param_value(params, "rr_fdr_dx_50_59"))
  expect_equal(rr_fdr(72, params), param_value(params, "rr_fdr_dx_60plus"))
})

test_that("lifestyle relative risks multiply", {
  expect_equal(lifestyle_rr(character(), params), 1)
  expect_equal(lifestyle_rr(c("obesity", "alcohol"), params),
               param_value(params, "rr_obesity") * param_value(params, "rr_alcohol"))
  expect_error(lifestyle_rr("unicorns", params))
})

test_that("Lynch Weibull curve passes through the penetrance inputs", {
  for (g in c("MLH1", "MSH2", "MSH6", "PMS2")) {
    cr <- cumulative_risk(c(50, 75), paste0("lynch_", g), params,
                          from = param_value(params, "lynch_onset_age"), step = 0.01)
    expect_equal(cr, c(param_value(params, paste0("pen_", g, "_50")),
                       param_value(params, paste0("pen_", g, "_75"))), tolerance = 1e-3)
  }
})

test_that("cumulative risk is a valid, increasing probability", {
  for (g in GROUP_CHOICES) {
    cr <- cumulative_risk(20:75, g, params)
    expect_true(all(cr >= 0 & cr < 1))
    expect_true(all(diff(cr) >= 0))
  }
})

test_that("risk-equivalent age is the reference age for average risk", {
  expect_equal(risk_equivalent_age("average", params), 45, tolerance = 0.05)
  expect_equal(risk_equivalent_age("average", params, metric = "ten_year"), 45, tolerance = 0.5)
})

test_that("risk-equivalent ages follow guideline logic (roughly)", {
  fdr <- risk_equivalent_age("fdr", params, fdr_age = 50)
  expect_true(fdr > 30 && fdr < 45)
  # younger affected relative -> earlier equivalent age
  expect_lt(risk_equivalent_age("fdr", params, fdr_age = 40), fdr)
  # Lynch MLH1/MSH2: early 20s or younger; MSH6/PMS2 later
  expect_lte(risk_equivalent_age("lynch_MLH1", params), 25)
  expect_lte(risk_equivalent_age("lynch_MSH2", params), 25)
  expect_gt(risk_equivalent_age("lynch_PMS2", params), risk_equivalent_age("lynch_MLH1", params))
  # lifestyle moves it earlier
  expect_lt(risk_equivalent_age("average", params, lifestyle = c("obesity", "alcohol")), 45)
})

test_that("symptoms raise prevalence via the likelihood ratio", {
  p0 <- crc_prevalence(27, params = params)
  p1 <- crc_prevalence(27, params = params, symptoms = "rectal_bleeding")
  expect_equal(p1, post_test_prob(p0, param_value(params, "lr_rectal_bleeding")))
  expect_gt(crc_prevalence(27, params = params, symptoms = "both"), p1)
})

test_that("advanced adenoma prevalence uses the right age band and multiplier", {
  expect_equal(aa_prevalence(25, params = params), 0.005)
  expect_equal(aa_prevalence(47, params = params), 0.035)
  expect_equal(aa_prevalence(47, "fdr", params), 0.035 * param_value(params, "rr_aa_fdr"))
  expect_equal(aa_prevalence(47, "lynch_PMS2", params), 0.035 * param_value(params, "rr_aa_lynch"))
})
