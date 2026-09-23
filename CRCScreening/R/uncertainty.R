# Beta-distribution helpers for the Monte Carlo uncertainty layer -----------

#' Beta shape parameters with a given mean and effective sample size
#' (alpha + beta = ess)
beta_from_mean_ess <- function(mean, ess) {
  mean <- pmin(pmax(mean, 1e-8), 1 - 1e-8)
  list(alpha = mean * ess, beta = (1 - mean) * ess)
}

#' Beta shape parameters with a given mean and coefficient of variation.
#' var = m(1-m)/(k+1) and cv = sd/m  =>  k = (1-m)/(m cv^2) - 1.
#' If the requested cv is impossible for that mean, it is shrunk so k >= 2.
beta_from_mean_cv <- function(mean, cv) {
  mean <- pmin(pmax(mean, 1e-8), 1 - 1e-8)
  k <- (1 - mean) / (mean * cv^2) - 1
  k <- pmax(k, 2)
  list(alpha = mean * k, beta = (1 - mean) * k)
}

rbeta_mean_cv <- function(n, mean, cv) {
  s <- beta_from_mean_cv(mean, cv)
  stats::rbeta(n, s$alpha, s$beta)
}

#' Draws for a parameter-table row: Beta, lognormal (from 95% CI) or fixed
draw_param <- function(n, row) {
  switch(row$dist,
    beta      = stats::rbeta(n, row$alpha, row$beta),
    lognormal = stats::rlnorm(n, log(row$value), lognormal_sdlog(row$lower, row$upper)),
    fixed     = rep(row$value, n)
  )
}

#' Median and equal-tailed interval of a vector of draws
summarise_draws <- function(x, level = 0.95) {
  a <- (1 - level) / 2
  q <- stats::quantile(x, c(a, 0.5, 1 - a), na.rm = TRUE, names = FALSE)
  tibble::tibble(lower = q[1], median = q[2], upper = q[3])
}
