test_that("PPV and NPV match hand calculations", {
  # 90% sens, 90% spec, 50% prevalence: symmetric, PPV = NPV = 0.9
  expect_equal(ppv(0.9, 0.9, 0.5), 0.9)
  expect_equal(npv(0.9, 0.9, 0.5), 0.9)
  # classic mammography-style example: 1% prevalence, 90% sens, 91% spec
  expect_equal(ppv(0.9, 0.91, 0.01), 0.009 / (0.009 + 0.0891))
  expect_equal(npv(0.9, 0.91, 0.01), 0.9009 / (0.9009 + 0.001))
  # perfect specificity -> PPV 1; perfect sensitivity -> NPV 1
  expect_equal(ppv(0.5, 1, 0.01), 1)
  expect_equal(npv(1, 0.5, 0.01), 1)
})

test_that("PPV rises with prevalence and is vectorised", {
  p <- ppv(0.92, 0.87, c(1e-4, 1e-3, 1e-2, 0.1))
  expect_length(p, 4)
  expect_true(all(diff(p) > 0))
})

test_that("odds-form Bayes equals the PPV formula", {
  prev <- c(1e-4, 0.003, 0.2)
  expect_equal(post_test_prob(prev, lr_pos(0.92, 0.87)), ppv(0.92, 0.87, prev))
  expect_equal(post_test_prob(prev, lr_neg(0.92, 0.87)), 1 - npv(0.92, 0.87, prev))
  expect_equal(post_test_prob(0.3, 1), 0.3)
})

test_that("sequential testing = applying Bayes twice, and beats one test", {
  prev <- 0.001
  s <- sequential_ppv(prev, 0.92, 0.87, 0.95, 0.999)
  expect_equal(s, post_test_prob(prev, lr_pos(0.92, 0.87) * lr_pos(0.95, 0.999)))
  expect_gt(s, ppv(0.92, 0.87, prev))
})

test_that("classify_population counts add up", {
  out <- classify_population(0.002, 0.05, 0.92, 0.42, 0.13, target = "crc", n = 10000)
  expect_equal(out$tp + out$fp + out$fn + out$tn, 10000)
  expect_equal(out$tp + out$fn, 20)
  expect_equal(out$tp, 20 * 0.92)
  expect_equal(out$ppv, out$tp / (out$tp + out$fp))
})

test_that("with no adenomas, the three-state model reduces to two-state PPV", {
  prev <- c(1e-4, 0.01)
  out <- classify_population(prev, 0, 0.92, 0.42, 1 - 0.87, target = "crc")
  expect_equal(out$ppv, ppv(0.92, 0.87, prev))
  expect_equal(out$npv, npv(0.92, 0.87, prev))
})

test_that("adenomas count as false positives for cancer but true positives for crc_aa", {
  crc <- classify_population(0.002, 0.05, 0.92, 0.42, 0.13, target = "crc")
  both <- classify_population(0.002, 0.05, 0.92, 0.42, 0.13, target = "crc_aa")
  expect_equal(crc$positives, both$positives)
  expect_equal(crc$fp - both$fp, 10000 * 0.05 * 0.42)
  expect_gt(both$ppv, crc$ppv)
})

test_that("classify_population rejects impossible prevalences", {
  expect_error(classify_population(0.6, 0.5, 0.9, 0.4, 0.1))
})

test_that("round_counts keeps the total and rounds by largest remainder", {
  x <- c(tp = 0.8, fn = 0.2, fp = 1300.4, tn = 8698.6)
  r <- round_counts(x, 10000)
  expect_equal(sum(r), 10000)
  expect_equal(unname(r), c(1, 0, 1300, 8699))
  expect_named(r, names(x))
})
