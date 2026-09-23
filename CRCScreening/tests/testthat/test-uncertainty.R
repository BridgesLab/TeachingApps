test_that("beta_from_mean_cv recovers the requested mean and cv", {
  for (m in c(1e-4, 0.01, 0.3)) {
    s <- beta_from_mean_cv(m, 0.3)
    mu <- s$alpha / (s$alpha + s$beta)
    v <- s$alpha * s$beta / ((s$alpha + s$beta)^2 * (s$alpha + s$beta + 1))
    expect_equal(mu, m)
    expect_equal(sqrt(v) / mu, 0.3, tolerance = 1e-8)
  }
})

test_that("beta_from_mean_cv shrinks impossible cvs instead of failing", {
  s <- beta_from_mean_cv(0.9, 2)
  expect_true(s$alpha > 0 && s$beta > 0)
})

test_that("beta_from_mean_ess keeps the effective sample size", {
  s <- beta_from_mean_ess(0.9, 100)
  expect_equal(s$alpha + s$beta, 100)
  expect_equal(s$alpha, 90)
})

test_that("draw_param respects each distribution type", {
  set.seed(1)
  b <- draw_param(20000, param_row(params, "acc_cologuard_sens_crc"))
  expect_equal(mean(b), 60.5 / 66, tolerance = 0.01)
  ln <- draw_param(20000, param_row(params, "harm_perforation"))
  expect_equal(median(ln), 3.1, tolerance = 0.05)
  expect_equal(unname(quantile(ln, c(0.025, 0.975))), c(2.3, 4.0), tolerance = 0.05)
  expect_true(all(draw_param(5, param_row(params, "sojourn_crc")) > 0))
  expect_equal(draw_param(3, param_row(params, "guide_avg_start")), rep(45, 3))
})

test_that("simulated PPV is centred on the point estimate", {
  sc <- build_scenario(params, 55, test = "fit")
  pt <- run_scenario(sc)
  sim <- run_scenario(sc, n_draws = 3000, seed = 42)
  s <- summarise_draws(sim$ppv)
  expect_true(s$lower < pt$ppv && pt$ppv < s$upper)
  expect_equal(s$median, pt$ppv, tolerance = 0.2)
})

test_that("simulations are reproducible with a seed and don't touch global RNG", {
  sc <- build_scenario(params, 30, test = "cologuard")
  set.seed(99); before <- runif(1)
  set.seed(99)
  a <- run_scenario(sc, n_draws = 50, seed = 7)
  after <- runif(1)
  b <- run_scenario(sc, n_draws = 50, seed = 7)
  expect_equal(a$ppv, b$ppv)
  expect_equal(before, after)
})
