# Plain-language number formatting -----------------------------------------

#' "1 in 1,600" style, rounded to 2 significant figures
fmt_one_in <- function(p) {
  ifelse(is.na(p) | p <= 0, "—",
         paste0("1 in ", scales::comma(signif(1 / p, 2), accuracy = 1)))
}

#' Percentage with sensible precision for tiny and large probabilities
fmt_pct <- function(p) {
  # near 100%, show enough digits that the gap to 100% is visible
  near_one_acc <- 10^-pmax(1, ceiling(-log10(pmax(1 - p, 1e-9))) - 2)
  vapply(seq_along(p), function(i) {
    x <- p[i]
    if (is.na(x)) return("—")
    acc <- if (x < 0.001) 0.001 else if (x < 0.1) 0.01 else if (x <= 0.99) 0.1 else near_one_acc[i]
    scales::percent(x, accuracy = acc)
  }, character(1))
}

#' Probability as "0.06% (1 in 1,600)"; skips the "1 in" part above 50%
fmt_prob <- function(p) {
  ifelse(p >= 0.5, fmt_pct(p), paste0(fmt_pct(p), " (", fmt_one_in(p), ")"))
}

fmt_count <- function(x) {
  ifelse(is.na(x), "—",
    ifelse(x < 10, format(round(x, 1), nsmall = 1, trim = TRUE),
           scales::comma(x, accuracy = 1)))
}

fmt_interval <- function(lower, upper, f = fmt_count) {
  paste0(f(lower), " to ", f(upper))
}
