test_that("k = 0 reproduces the OLS estimate", {
  d <- plm_test_data()
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  ols <- stats::coef(stats::lm(Y ~ D + X, data = d))[["D"]]
  expect_equal(plm_estimate(fit, k = 0), ols)
})

test_that("m = 1 reproduces difference-in-differences computed by hand", {
  d <- plm_test_data()
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  # With a perfect placebo and m = 1, the paper's Equation 21 reduces to
  # beta_Y~D - beta_P~D.
  b_yd <- stats::coef(stats::lm(Y ~ D + X, data = d))[["D"]]
  b_pd <- stats::coef(stats::lm(P ~ D + X, data = d))[["D"]]
  expect_equal(plm_estimate(fit, m = 1), b_yd - b_pd)
})

test_that("m and k round-trip through the scale factor", {
  d <- plm_test_data()
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  k <- c(-1, 0, 0.5, 1, 3.7)
  expect_equal(plm_m_to_k(fit, plm_k_to_m(fit, k)), k)
  expect_equal(plm_estimate(fit, k = k),
               plm_estimate(fit, m = plm_k_to_m(fit, k)))
})

test_that("supplying both or neither of k and m is an error", {
  d <- plm_test_data()
  fit <- placebo_lm(d, "Y", "D", "P", structure = "placebo_outcome")
  expect_error(plm_estimate(fit), "exactly one")
  expect_error(plm_estimate(fit, k = 1, m = 1), "not both")
})

test_that("the estimator is vectorised over both parameters", {
  d <- plm_test_data()
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  expect_length(plm_estimate(fit, k = seq(0, 1, by = 0.25)), 5L)
  expect_length(plm_estimate(fit, k = 1, imperfection = c(0, 0.1, 0.2)), 3L)
})

# Oracle recovery lives in test-recovery.R, which uses a DGP matched to each
# structure's causal graph. The version that used to sit here relied on one
# shared DGP and so could only assert the weak claim that the adjustment moved
# the estimate in the right direction for four of the five structures.

test_that("plm_solve() inverts plm_estimate()", {
  d <- plm_test_data()
  for (s in names(plm_structures)) {
    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X", structure = s)
    for (target in c(-2, 0, 0.5, 1.7)) {
      k <- plm_solve(fit, target = target)$k
      expect_equal(plm_estimate(fit, k = k), target, info = s)
    }
  }
})

test_that("plm_solve() reports m alongside k", {
  d <- plm_test_data()
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  s <- plm_solve(fit, target = 0)
  expect_equal(s$m, s$k * fit$SF)
})

test_that("plm_solve() errors when the estimate does not vary with k", {
  d <- plm_test_data()
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  # Setting imperfection equal to the observed sensitivity coefficient makes
  # the bias adjustment identically zero.
  expect_error(
    plm_solve(fit, target = 0, imperfection = fit$coefs$sens$estimate),
    "does not vary with k"
  )
})

test_that("plm_benchmarks() returns the three reference points", {
  d <- plm_test_data()
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  bm <- plm_benchmarks(fit)
  expect_equal(nrow(bm), 3L)
  expect_equal(bm$k[1], 0)          # no unobserved confounding
  expect_equal(bm$m[2], 1)          # m = 1 row
  expect_equal(bm$k[3], 1)          # equiconfounding after rescaling
  expect_equal(bm$adjusted_coefficient, plm_estimate(fit, k = bm$k))
})

test_that("bounds are the extremes over the assumption region", {
  d <- plm_test_data()
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  b <- plm_bounds(fit, k = c(0.5, 1), n_boot = 0)
  brute <- range(plm_estimate(fit, k = seq(0.5, 1, length.out = 501)))
  expect_equal(c(b$lower, b$upper), brute, tolerance = 1e-8)
})

test_that("bounds widen when the imperfection range widens", {
  d <- plm_test_data()
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  narrow <- plm_bounds(fit, k = c(0.5, 1), imperfection = 0, n_boot = 0)
  wide   <- plm_bounds(fit, k = c(0.5, 1), imperfection = c(-0.2, 0.2),
                       n_boot = 0)
  expect_lte(wide$lower, narrow$lower)
  expect_gte(wide$upper, narrow$upper)
})

test_that("bounds require a length-2 range", {
  d <- plm_test_data()
  fit <- placebo_lm(d, "Y", "D", "P", structure = "placebo_outcome")
  expect_error(plm_bounds(fit, k = 1, n_boot = 0), "length-2")
})
