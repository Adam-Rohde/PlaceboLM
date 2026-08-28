# Behaviour under non-linear data-generating processes.
#
# The rest of the suite is linear, additive, Gaussian and constant-effect. These
# tests depart from that in three directions, and separate two questions that
# are easy to conflate:
#
#   * The point estimator's recovery identity is pure OLS algebra. It cannot be
#     broken by any DGP, however hostile, and the first test here pins that.
#   * Whether the INTERVALS are calibrated is a genuine statistical question
#     that the linear fixtures cannot answer. That is what the Monte Carlo
#     tests below are for.
#
# The second group is slow and gated behind PLACEBOLM_SLOW_TESTS.

kinds <- c("nl_confounding", "interaction", "nl_treatment")

skip_unless_slow <- function() {
  if (!nzchar(Sys.getenv("PLACEBOLM_SLOW_TESTS")))
    testthat::skip("slow Monte Carlo test; set PLACEBOLM_SLOW_TESTS to run")
}


# ---- deterministic: the algebra is DGP-free ---------------------------------

test_that("recovery is exact under non-linear DGPs", {
  for (k in kinds) {
    d   <- plm_dgp_nonlinear(k, n = 3000)
    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                      structure = "placebo_outcome")
    tp  <- plm_true_params(fit, d)
    expect_equal(plm_estimate(fit, k = tp$k, imperfection = tp$imperfection),
                 tp$target_long, tolerance = 1e-10, info = k)
  }
})


test_that("recovery is exact even for a deliberately hostile DGP", {
  # Gamma confounder, exponential treatment, Z^2 + sin(3Z) placebo, t3 errors
  # scaled by (1 + |Z|), and a heterogeneous D*X term. This generator is NOT
  # usable as a coverage fixture -- its population m does not converge -- but the
  # in-sample identity holds regardless, which is exactly the point.
  set.seed(3); n <- 4000
  Z <- stats::rgamma(n, 2, 1) - 2
  X <- stats::rbinom(n, 1, 0.4)
  D <- exp(0.4 * Z) + 2 * X * Z + stats::rt(n, 3)
  P <- Z^2 + sin(3 * Z) + 0.3 * D + stats::rt(n, 4)
  Y <- 2 * D + 0.5 * D * X + Z^3 / 5 + exp(X) +
       stats::rt(n, 3) * (1 + abs(Z))
  d <- data.frame(Y = Y, D = D, P = P, X = X, Z = Z)

  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  tp  <- plm_true_params(fit, d)
  expect_equal(plm_estimate(fit, k = tp$k, imperfection = tp$imperfection),
               tp$target_long, tolerance = 1e-10)
})


test_that("under non-linearity the projection is not the structural coefficient", {
  # A guard on interpretation rather than on code. nl_confounding has structural
  # effect 2, but Y loads on Z through 1.5*Z + 0.8*Z^2 while P loads through
  # Z + 0.5*Z^2, so the linear projection is a different number entirely.
  pop <- plm_population_of(function(n, seed)
    plm_dgp_nonlinear("nl_confounding", n = n, seed = seed), n = 200000)
  expect_gt(abs(pop$target_long - 2), 0.3)
  expect_gt(abs(pop$m - 1), 0.2)
})


# ---- the estimand: projection, not ATE --------------------------------------

test_that("the projection and the ATE genuinely differ in the heterogeneous DGP", {
  pop <- plm_population_of(plm_dgp_heterogeneous, n = 200000)
  # ATE is 3 by construction; the variance-weighted projection is near 3.86.
  expect_gt(abs(pop$target_long - PLM_HET_ATE), 0.5)
})


test_that("intervals cover the projection and NOT the ATE", {
  # The sharpest available statement of what the method does and does not
  # estimate. Coverage of the projection should be nominal; coverage of the ATE
  # should be essentially zero, because the ATE is simply not the target.
  skip_unless_slow()
  pop <- plm_population_of(plm_dgp_heterogeneous)
  S <- 300; n <- 2000

  res <- t(vapply(seq_len(S), function(s) {
    d   <- plm_dgp_heterogeneous(n = n, seed = 70000 + s)
    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                      structure = "placebo_outcome")
    g <- plm_grid(fit, m = pop$m, imperfection = pop$imperfection,
                  n_boot = 300, cores = 1, engine = "matrix")
    c(proj = g$ci_lower <= pop$target_long && pop$target_long <= g$ci_upper,
      ate  = g$ci_lower <= PLM_HET_ATE     && PLM_HET_ATE     <= g$ci_upper)
  }, numeric(2)))

  se <- sqrt(0.95 * 0.05 / S)
  expect_gt(mean(res[, "proj"]), 0.95 - 4 * se)
  expect_lt(mean(res[, "proj"]), 0.95 + 4 * se)
  expect_lt(mean(res[, "ate"]), 0.10)
})


# ---- coverage under non-linearity -------------------------------------------

test_that("bootstrap intervals stay calibrated under non-linearity", {
  skip_unless_slow()
  S <- 300; n <- 1000
  se <- sqrt(0.95 * 0.05 / S)

  for (k in kinds) {
    gen <- local({ kk <- k; function(n, seed) plm_dgp_nonlinear(kk, n = n, seed = seed) })
    pop <- plm_population_of(gen, n = 400000)

    cov <- mean(vapply(seq_len(S), function(s) {
      d   <- plm_dgp_nonlinear(k, n = n, seed = 80000 + s)
      fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                        structure = "placebo_outcome")
      g <- plm_grid(fit, m = pop$m, imperfection = pop$imperfection,
                    n_boot = 300, cores = 1, engine = "matrix")
      isTRUE(g$ci_lower <= pop$target_long && pop$target_long <= g$ci_upper)
    }, logical(1)))

    expect_gt(cov, 0.95 - 4 * se, label = paste(k, "coverage", round(cov, 3)))
    expect_lt(cov, 0.95 + 4 * se, label = paste(k, "coverage", round(cov, 3)))
  }
})


# ---- the convergence guard ---------------------------------------------------

test_that("a target whose m does not converge is refused, not silently used", {
  # This is what stops a coverage test being built on a meaningless quantity.
  skip_unless_slow()
  hostile <- function(n, seed) {
    set.seed(seed)
    Z <- stats::rgamma(n, 2, 1) - 2
    X <- stats::rbinom(n, 1, 0.4)
    D <- exp(0.4 * Z) + 2 * X * Z + stats::rt(n, 3)
    data.frame(Y = 2 * D + 0.5 * D * X + Z^3 / 5 + exp(X) +
                   stats::rt(n, 3) * (1 + abs(Z)),
               D = D, P = Z^2 + sin(3 * Z) + 0.3 * D + stats::rt(n, 4),
               X = X, Z = Z)
  }
  expect_error(plm_population_of(hostile, n = 200000), "does not converge")
})


test_that("the well-behaved generators pass the convergence guard", {
  skip_unless_slow()
  for (k in kinds) {
    gen <- local({ kk <- k; function(n, seed) plm_dgp_nonlinear(kk, n = n, seed = seed) })
    pop <- plm_population_of(gen, n = 400000)
    expect_lt(pop$cv_m, 1.0, label = paste(k, "cv(m)"))
  }
})
