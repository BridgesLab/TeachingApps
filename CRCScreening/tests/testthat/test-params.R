test_that("parameter file has the expected schema", {
  expect_true(all(PARAM_COLUMNS %in% names(params)))
  expect_false(anyDuplicated(params$param_id) > 0)
  expect_true(all(params$dist %in% PARAM_DISTS))
})

test_that("every value is flagged for verification and has a source", {
  # Remove this test (or change the expectation) once values are checked
  expect_true(all(params$status == "VERIFY"))
  expect_false(any(is.na(params$source) | params$source == ""))
})

test_that("probabilities are between 0 and 1", {
  probs <- params |> dplyr::filter(units %in% c("proportion", "cumulative risk"))
  expect_true(all(probs$value >= 0 & probs$value <= 1))
})

test_that("Beta rows are consistent with their point value", {
  b <- params |> dplyr::filter(dist == "beta")
  expect_equal(b$alpha / (b$alpha + b$beta), b$value, tolerance = 0.02)
})

test_that("every test has sensitivity for cancer and adenoma plus specificity", {
  for (t in TEST_CHOICES) {
    for (tg in TARGET_CHOICES) {
      acc <- test_accuracy(params, t, tg)
      expect_equal(acc$state, c("crc", "aa", "none"))
      expect_true(all(acc$p_pos >= 0 & acc$p_pos <= 1))
    }
  }
})

test_that("colonoscopy uses target-specific rows", {
  crc <- test_accuracy(params, "colonoscopy", "crc")
  both <- test_accuracy(params, "colonoscopy", "crc_aa")
  expect_equal(crc$quantity[2], "pos_aa_crc_target")
  expect_equal(both$quantity[2], "sens_aa")
  expect_lt(crc$p_pos[3], both$p_pos[3])
})

test_that("malformed parameter files are rejected", {
  bad <- params
  bad$param_id[2] <- bad$param_id[1]
  expect_error(validate_params(bad), "Duplicate")
  bad <- params
  bad$alpha[bad$dist == "beta"][1] <- NA
  expect_error(validate_params(bad), "Beta")
})

test_that("scenario presets are valid and runnable", {
  expect_gte(nrow(scenarios), 4)
  expect_lte(nrow(scenarios), 6)
  for (i in seq_len(nrow(scenarios))) {
    s <- scenarios[i, ]
    life <- if (is.na(s$lifestyle)) character() else strsplit(s$lifestyle, ";")[[1]]
    sc <- build_scenario(params, s$age, s$group, dplyr::coalesce(s$fdr_age, 50), life,
                         s$symptoms, s$test, s$target)
    r <- run_scenario(sc)
    expect_true(is.finite(r$ppv))
  }
})
