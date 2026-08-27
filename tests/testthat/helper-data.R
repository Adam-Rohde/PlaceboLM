# Shared synthetic data for the test suite.
#
# DGP: U is an unobserved confounder shared by D, P, and Y; X is an observed
# covariate. The true causal effect of D on Y is 1. Because U is recorded in the
# returned frame, "oracle" regressions that condition on it can be run to
# recover the true value of each structure's sensitivity parameter.

plm_test_data <- function(n = 4000, seed = 20240101, beta_D = 1) {
  set.seed(seed)
  U <- stats::rnorm(n)
  X <- stats::rnorm(n)
  D <- X + U + stats::rnorm(n)
  P <- X + U + stats::rnorm(n)
  Y <- beta_D * D + X + U + stats::rnorm(n)
  data.frame(Y = Y, D = D, P = P, X = X, U = U)
}

# The oracle value of a structure's sensitivity parameter: the same coefficient
# the structure reads from its sensitivity regression, but refit with the
# unobserved confounder U included.
plm_oracle_imperfection <- function(fit, data) {
  spec <- fit$spec
  loc  <- spec$sens_coef(fit$vars)
  # The sensitivity coefficient may live in the target regression (e.g. the
  # placebo-treatment structure), so pick whichever formula it came from.
  f <- fit$formulas[[loc$reg]]
  f_oracle <- stats::update(f, . ~ . + U)
  stats::coef(stats::lm(f_oracle, data = data))[[loc$coef]]
}
