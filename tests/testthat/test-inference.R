test_that("plm_grid() returns point estimates when n_boot = 0", {
  d <- plm_test_data(n = 400)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  g <- plm_grid(fit, k = c(0, 1), n_boot = 0)
  expect_equal(names(g), c("k", "m", "imperfection", "adjusted_coefficient"))
  expect_equal(nrow(g), 2L)
})

test_that("plm_grid() adds interval columns when bootstrapping", {
  d <- plm_test_data(n = 400)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  set.seed(1)
  g <- plm_grid(fit, k = c(0, 0.5, 1), n_boot = 100, cores = 1)
  expect_true(all(c("std_error", "ci_lower", "ci_upper") %in% names(g)))
  expect_true(all(g$ci_lower <= g$adjusted_coefficient))
  expect_true(all(g$adjusted_coefficient <= g$ci_upper))
  expect_true(all(g$std_error > 0))
})

test_that("point estimates are unaffected by the bootstrap", {
  d <- plm_test_data(n = 400)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  set.seed(1)
  a <- plm_grid(fit, k = c(0, 1), n_boot = 50, cores = 1)
  set.seed(2)
  b <- plm_grid(fit, k = c(0, 1), n_boot = 50, cores = 1)
  # Different resamples, identical point estimates.
  expect_equal(a$adjusted_coefficient, b$adjusted_coefficient)
})

test_that("the grid is crossed over k and imperfection", {
  d <- plm_test_data(n = 300)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  g <- plm_grid(fit, k = c(0, 1, 2), imperfection = c(-0.1, 0, 0.1),
                n_boot = 0)
  expect_equal(nrow(g), 9L)
})

test_that("normal and percentile intervals are both available", {
  d <- plm_test_data(n = 400)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  set.seed(1)
  p <- plm_grid(fit, k = 1, n_boot = 200, ci_type = "percentile", cores = 1)
  set.seed(1)
  nrm <- plm_grid(fit, k = 1, n_boot = 200, ci_type = "normal", cores = 1)
  expect_equal(p$adjusted_coefficient, nrm$adjusted_coefficient)
  # The normal interval is symmetric about the estimate by construction.
  expect_equal(nrm$adjusted_coefficient - nrm$ci_lower, nrm$ci_upper - nrm$adjusted_coefficient)
})

test_that("bootstrap intervals narrow as n grows", {
  small <- placebo_lm(plm_test_data(n = 200, seed = 1), "Y", "D", "P",
                      covariates = "X", structure = "placebo_outcome")
  large <- placebo_lm(plm_test_data(n = 4000, seed = 1), "Y", "D", "P",
                      covariates = "X", structure = "placebo_outcome")
  set.seed(1); s <- plm_grid(small, k = 1, n_boot = 200, cores = 1)
  set.seed(1); l <- plm_grid(large, k = 1, n_boot = 200, cores = 1)
  expect_lt(l$std_error, s$std_error)
})

test_that("plm_bounds() intervals contain the point bound", {
  d <- plm_test_data(n = 500)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  set.seed(1)
  b <- plm_bounds(fit, k = c(0.5, 1), n_boot = 200, cores = 1)
  # Bootstrap quantiles of the bound must sit outside the point bound. Note
  # these are NOT a confidence interval for the identified set -- see ?plm_bounds.
  expect_lte(b$lower_boot_q, b$lower)
  expect_gte(b$upper_boot_q, b$upper)
})
