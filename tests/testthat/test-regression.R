# The revised paper shows the m-adjustment is exactly an OLS fit on the
# pseudo-outcome (Y - m*P). These tests assert that identity, since it is what
# licenses analytic (and cluster-robust) standard errors.

test_that("the pseudo-outcome regression reproduces the adjusted estimate", {
  d <- plm_test_data()
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")

  for (m in c(-1.2, 0, 0.5, 1, 2.3)) {
    mod <- plm_regression(fit, m = m)
    expect_equal(unname(stats::coef(mod)[["D"]]),
                 plm_estimate(fit, m = m), info = paste("m =", m))
  }
})

test_that("the offset accounts for a non-zero imperfection", {
  d <- plm_test_data()
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  mod <- plm_regression(fit, m = 1.4, imperfection = 0.3)
  est <- unname(stats::coef(mod)[["D"]]) + attr(mod, "plm_offset")
  expect_equal(est, plm_estimate(fit, m = 1.4, imperfection = 0.3))
})

test_that("plm_analytic() agrees with plm_estimate() on the point estimate", {
  d <- plm_test_data()
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  a <- plm_analytic(fit, m = 1, imperfection = 0.1)
  expect_equal(a$adjusted_coefficient, plm_estimate(fit, m = 1, imperfection = 0.1))
  expect_gt(a$std_error, 0)
  expect_lt(a$ci_lower, a$adjusted_coefficient)
  expect_gt(a$ci_upper, a$adjusted_coefficient)
})

test_that("at m = 0 the analytic SE equals the plain short-regression SE", {
  # With no adjustment the pseudo-outcome is just Y, so the standard error must
  # be the ordinary one for the treatment coefficient.
  d <- plm_test_data()
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  a <- plm_analytic(fit, m = 0)
  ct <- stats::coef(summary(stats::lm(Y ~ D + X, data = d)))
  expect_equal(a$std_error, unname(ct["D", "Std. Error"]))
  expect_equal(a$adjusted_coefficient,  unname(ct["D", "Estimate"]))
})

test_that("analytic and bootstrap standard errors are close at fixed m", {
  d <- plm_test_data(n = 2000)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  m <- 1
  a <- plm_analytic(fit, m = m)
  set.seed(1)
  b <- plm_grid(fit, k = m / fit$SF, n_boot = 400, cores = 1)
  expect_equal(a$adjusted_coefficient, b$adjusted_coefficient)
  expect_equal(a$std_error, b$std_error, tolerance = 0.15)
})

test_that("a custom vcov is honoured", {
  d <- plm_test_data(n = 500)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  # A crude robust-style estimator: just inflate the classical one.
  infl <- function(mod) stats::vcov(mod) * 4
  a <- plm_analytic(fit, m = 1)
  r <- plm_analytic(fit, m = 1, vcov = infl)
  expect_equal(r$std_error, a$std_error * 2)
  expect_equal(r$adjusted_coefficient, a$adjusted_coefficient)
})

test_that("the representation is refused where it does not hold", {
  d <- plm_test_data(n = 300)
  for (s in setdiff(names(plm_structures), "placebo_outcome")) {
    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X", structure = s)
    expect_error(plm_regression(fit, m = 1), "structure only", info = s)
  }
})
