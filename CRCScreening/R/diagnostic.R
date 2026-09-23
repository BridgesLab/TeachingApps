# Diagnostic test math -------------------------------------------------------
#
# Two layers:
#   1. Classic two-state formulas (disease yes/no): ppv(), npv(), lr_pos(),
#      post_test_prob(), sequential_ppv(). Used for teaching and tests.
#   2. A three-state population model (cancer / advanced adenoma / neither),
#      classify_population(). Stool and blood tests react to adenomas as well
#      as cancers, so when the target is "cancer only" a positive result in a
#      person with an adenoma counts as a false positive for cancer (even
#      though finding the adenoma is useful). All functions are vectorised.

ppv <- function(sens, spec, prev) {
  sens * prev / (sens * prev + (1 - spec) * (1 - prev))
}

npv <- function(sens, spec, prev) {
  spec * (1 - prev) / (spec * (1 - prev) + (1 - sens) * prev)
}

lr_pos <- function(sens, spec) sens / (1 - spec)
lr_neg <- function(sens, spec) (1 - sens) / spec

prob_to_odds <- function(p) p / (1 - p)
odds_to_prob <- function(o) o / (1 + o)

#' Bayes' rule in odds form: post-test odds = pre-test odds x likelihood ratio
post_test_prob <- function(pre, lr) odds_to_prob(prob_to_odds(pre) * lr)

#' P(disease | test 1 +, test 2 +), assuming the tests are conditionally
#' independent given disease status
sequential_ppv <- function(prev, sens1, spec1, sens2, spec2) {
  ppv(sens2, spec2, ppv(sens1, spec1, prev))
}

#' Expected outcomes when n people are tested
#'
#' @param prev_crc,prev_aa prevalence of (preclinical) cancer and of advanced
#'   adenoma without cancer. Must satisfy prev_crc + prev_aa <= 1.
#' @param p_crc,p_aa,p_none probability of a positive test in each state
#'   (p_none = 1 - specificity).
#' @param target "crc" (cancer only) or "crc_aa" (cancer or advanced adenoma).
#' @return tibble with expected counts (not rounded) and summary metrics.
classify_population <- function(prev_crc, prev_aa, p_crc, p_aa, p_none,
                                target = c("crc", "crc_aa"), n = 10000) {
  target <- match.arg(target)
  if (any(prev_crc + prev_aa > 1 + 1e-12)) stop("prev_crc + prev_aa must be <= 1")

  n_crc  <- n * prev_crc
  n_aa   <- n * prev_aa
  n_none <- n - n_crc - n_aa

  pos_crc  <- n_crc * p_crc
  pos_aa   <- n_aa * p_aa
  pos_none <- n_none * p_none

  if (target == "crc") {
    tp <- pos_crc
    fn <- n_crc - pos_crc
    fp <- pos_aa + pos_none
    tn <- n_aa + n_none - fp
  } else {
    tp <- pos_crc + pos_aa
    fn <- n_crc + n_aa - tp
    fp <- pos_none
    tn <- n_none - pos_none
  }

  tibble::tibble(
    n_crc, n_aa, n_none, pos_crc, pos_aa, pos_none,
    tp, fp, fn, tn,
    positives   = tp + fp,
    prev_target = (tp + fn) / n,
    ppv         = tp / (tp + fp),
    npv         = tn / (tn + fn),
    post_neg    = fn / (fn + tn),
    fp_per_tp   = fp / tp,
    sens_eff    = tp / (tp + fn),
    spec_eff    = tn / (tn + fp)
  )
}

#' Round expected counts to whole people while keeping the total fixed
#' (largest-remainder method)
round_counts <- function(x, total = round(sum(x))) {
  fl <- floor(x)
  short <- total - sum(fl)
  if (short > 0) {
    idx <- order(x - fl, decreasing = TRUE)[seq_len(short)]
    fl[idx] <- fl[idx] + 1
  }
  fl
}
