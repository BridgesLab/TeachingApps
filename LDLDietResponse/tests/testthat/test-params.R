test_that("parameter file has the expected schema and provenance", {
  expect_true(all(PARAM_COLUMNS %in% names(params)))
  expect_false(anyDuplicated(params$param_id) > 0)
  expect_true(all(params$verified %in% c("y", "n")))
  expect_false(any(is.na(params$source) | params$source == ""))
})

test_that("malformed parameter files are rejected", {
  bad <- params
  bad$param_id[2] <- bad$param_id[1]
  expect_error(validate_params(bad), "Duplicate")
  bad <- params
  bad$verified[1] <- "maybe"
  expect_error(validate_params(bad), "verified")
})

test_that("Mensink coefficients are the ones on the cards", {
  expect_equal(unname(P[c("beta_sfa_cho", "beta_sfa_mufa", "beta_sfa_pufa", "beta_chol", "egg_chol_mg")]),
               c(1.3, 1.6, 2.1, 2, 186))
})

test_that("APOE frequencies sum to 1 and every genotype has a multiplier", {
  expect_equal(sum(P[paste0("pop_freq_", APOE_CHOICES)]), 1)
  expect_true(all(paste0("apoe_", APOE_CHOICES) %in% names(P)))
})

test_that("every example diet adds up", {
  for (id in diets$diet_id) {
    d <- diet_from_row(diets, id)
    expect_gte(diet_carb(d), 0)
    expect_gte(diet_sfa_other(d), -1e-9)
  }
})

test_that("coefficient intervals bracket their values", {
  ci <- params[!is.na(params$lower), ]
  expect_gte(nrow(ci), 8)
  expect_true(all(ci$lower <= ci$value & ci$value <= ci$upper))
  bad <- params
  bad$upper[bad$param_id == "beta_chol"] <- 1
  expect_error(validate_params(bad), "interval")
})
