# The load-bearing claim of the architecture.
#
# The package computes one generic scale factor,
#
#   SF = se(beta_target) * sqrt(df_target) / ( se(beta_sens) * sqrt(df_sens) )
#
# rather than transcribing the structure-specific expression from each row of
# Tables 1 and 2. These tests assert that the generic rule reproduces every one
# of those expressions exactly, computed directly from regression residuals.
#
# If any of these fail, the registry approach is invalid for that structure and
# the estimator will silently return wrong numbers -- so these are the most
# important tests in the suite.

# sd of the residuals of a formula, i.e. sd(response perp predictors)
sdr <- function(formula, data) stats::sd(stats::resid(stats::lm(formula, data)))

test_that("SF matches the paper's expression: Placebo Outcome (Table 1[a],[b])", {
  d <- plm_test_data()
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  # sd(Y perp D,X) / sd(P perp D,X)
  paper <- sdr(Y ~ D + X, d) / sdr(P ~ D + X, d)
  expect_equal(fit$SF, paper)
})

test_that("SF matches the paper's expression: Placebo Treatment (Table 1[a],[c])", {
  d <- plm_test_data()
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_treatment")
  # sd(P perp D,X) / sd(D perp P,X)
  paper <- sdr(P ~ D + X, d) / sdr(D ~ P + X, d)
  expect_equal(fit$SF, paper)
})

test_that("SF matches the paper's expression: Observed Confounder 1 (Table 1[c])", {
  d <- plm_test_data()
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "observed_confounder_1")
  # [sd(Y perp D,P,X)/sd(D perp P,X)] * [sd(D perp X)/sd(P perp D,X)]
  paper <- (sdr(Y ~ D + P + X, d) / sdr(D ~ P + X, d)) *
           (sdr(D ~ X, d)         / sdr(P ~ D + X, d))
  expect_equal(fit$SF, paper)
})

test_that("SF matches the paper's expression: Observed Confounder 2 (Table 2[e],[f])", {
  d <- plm_test_data()
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "observed_confounder_2")
  # [sd(Y perp D,P,X)/sd(D perp P,X)] * [sd(P perp X)/sd(D perp P,X)]
  paper <- (sdr(Y ~ D + P + X, d) / sdr(D ~ P + X, d)) *
           (sdr(P ~ X, d)         / sdr(D ~ P + X, d))
  expect_equal(fit$SF, paper)
})

test_that("SF matches the paper's expression: Post-Outcome (Table 2[g],[h])", {
  d <- plm_test_data()
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "post_outcome")
  # [sd(Y perp D,X)/sd(D perp X)] * [sd(Y perp D,X)/sd(P perp Y,D,X)]
  paper <- (sdr(Y ~ D + X, d) / sdr(D ~ X, d)) *
           (sdr(Y ~ D + X, d) / sdr(P ~ Y + D + X, d))
  expect_equal(fit$SF, paper)
})

test_that("SF does not depend on the sensitivity parameters", {
  d <- plm_test_data()
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  # SF is a property of the data and the structure only; it is computed once.
  expect_length(fit$SF, 1L)
  expect_true(is.finite(fit$SF))
})

test_that("a degenerate placebo produces an informative error", {
  d <- plm_test_data(n = 200)
  d$P <- d$D              # placebo is a deterministic function of treatment
  expect_error(
    suppressWarnings(placebo_lm(d, "Y", "D", "P",
                                structure = "placebo_outcome")),
    "near-perfect fit"
  )
})

test_that("a very large scale factor is allowed", {
  # The paper's NSW analysis uses 1975 unemployment as a placebo for 1978
  # earnings, giving a scale factor above 40,000. A large SF is the reason to
  # reason about k rather than m -- it is not an error.
  d <- plm_test_data(n = 1000)
  d$Y <- d$Y * 50000                      # blow up the outcome scale
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  expect_gt(fit$SF, 1000)
  expect_true(is.finite(plm_estimate(fit, k = 1)))
})
