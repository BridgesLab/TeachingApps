test_that("scenario PPV matches the hand calculation from its inputs", {
  sc <- build_scenario(params, 25, test = "cologuard", target = "crc")
  r <- run_scenario(sc)
  sens <- param_value(params, "acc_cologuard_sens_crc")
  sens_aa <- param_value(params, "acc_cologuard_sens_aa")
  spec <- param_value(params, "acc_cologuard_spec")
  expected <- sens * sc$prev_crc /
    (sens * sc$prev_crc + sens_aa * sc$prev_aa + (1 - spec) * (1 - sc$prev_crc - sc$prev_aa))
  expect_equal(r$ppv, expected)
})

test_that("the same test has much lower PPV at 25 than at 55", {
  young <- run_scenario(build_scenario(params, 25, test = "cologuard"))
  older <- run_scenario(build_scenario(params, 55, test = "cologuard"))
  expect_gt(older$ppv / young$ppv, 5)
  expect_gt(young$fp_per_tp, older$fp_per_tp)
})

test_that("prevalence override bypasses the risk model", {
  r <- run_scenario(build_scenario(params, 25, prev_override = 0.05, symptoms = "both"))
  expect_equal(r$prev_crc, 0.05)
})

test_that("accuracy override changes the point values but keeps Beta ESS", {
  sc0 <- build_scenario(params, 40, test = "fit")
  sc1 <- build_scenario(params, 40, test = "fit",
                        accuracy_override = list(sens_crc = 0.8, sens_aa = 0.3, spec = 0.99))
  expect_equal(sc1$acc$p_pos, c(0.8, 0.3, 0.01))
  expect_equal(sc1$acc$alpha + sc1$acc$beta, sc0$acc$alpha + sc0$acc$beta)
  expect_gt(run_scenario(sc1)$ppv, run_scenario(sc0)$ppv)
})

test_that("downstream counts are internally consistent", {
  sc <- build_scenario(params, 55, test = "fit")
  r <- run_scenario(sc)
  expect_equal(r$colonoscopies, r$positives)
  expect_equal(r$crc_found + r$crc_missed, r$n_crc)
  expect_equal(r$perforations, r$colonoscopies * param_value(params, "harm_perforation") / 1e4)
  expect_lte(r$crc_found, r$tp)
})

test_that("primary colonoscopy means everyone gets a colonoscopy", {
  r <- run_scenario(build_scenario(params, 55, test = "colonoscopy"))
  expect_equal(r$colonoscopies, 10000)
  expect_true(is.na(r$ppv_seq))
  expect_gt(r$ppv, 0.5)
})

test_that("sequential PPV equals the product-of-rates calculation", {
  sc <- build_scenario(params, 30, test = "cologuard", target = "crc")
  r <- run_scenario(sc)
  q <- sc$confirm$p_pos
  p <- sc$acc$p_pos
  num <- sc$prev_crc * p[1] * q[1]
  den <- num + sc$prev_aa * p[2] * q[2] + (1 - sc$prev_crc - sc$prev_aa) * p[3] * q[3]
  expect_equal(r$ppv_seq, num / den)
  expect_gt(r$ppv_seq, r$ppv)
})

test_that("PPV-by-age curves cover every profile and age", {
  prof <- standard_profiles(lifestyle = "obesity")
  cur <- ppv_by_age(params, prof, ages = 20:30)
  expect_equal(nrow(cur), nrow(prof) * 11)
  cur_u <- ppv_by_age(params, prof[1:2, ], ages = 40:41, n_draws = 100)
  expect_true(all(cur_u$lower <= cur_u$upper))
})
