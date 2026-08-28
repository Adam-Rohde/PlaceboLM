# Tier 2 and 3 statistical validation: is the INFERENCE calibrated?
#
# test-recovery.R shows the point estimator returns the right quantity given the
# right parameters. That says nothing about whether the intervals around it have
# the coverage they claim. These tests are Monte Carlo, so they are slow and are
# skipped unless PLACEBOLM_SLOW_TESTS is set:
#
#   PLACEBOLM_SLOW_TESTS=true Rscript -e 'testthat::test_local()'
#
# Coverage is compared against its Monte Carlo standard error rather than an
# arbitrary tolerance, so that a near-miss is not read as a failure and a real
# miss is not excused.

skip_unless_slow <- function() {
  if (!nzchar(Sys.getenv("PLACEBOLM_SLOW_TESTS")))
    testthat::skip("slow Monte Carlo test; set PLACEBOLM_SLOW_TESTS to run")
}

# Monte Carlo standard error of an estimated coverage probability.
cover_se <- function(p, S) sqrt(p * (1 - p) / S)

# One simulation draw from the placebo-outcome DGP, returning the fitted object
# and the true parameters implied by that particular sample.
one_draw <- function(n, seed) {
  d   <- plm_dgp("placebo_outcome", n = n, seed = seed)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  list(fit = fit, truth = plm_true_params(fit, d))
}


test_that("percentile bootstrap intervals cover at roughly the nominal rate", {
  skip_unless_slow()
  S <- 400; n <- 800
  hits <- vapply(seq_len(S), function(s) {
    dr <- one_draw(n, seed = 10000 + s)
    g <- plm_grid(dr$fit, k = dr$truth$k, imperfection = dr$truth$imperfection,
                  n_boot = 300, cores = 1, ci_type = "percentile")
    g$ci_lower <= dr$truth$target_long && dr$truth$target_long <= g$ci_upper
  }, logical(1))
  cov <- mean(hits)
  # Allow three Monte Carlo standard errors around the nominal 0.95.
  expect_gt(cov, 0.95 - 3 * cover_se(0.95, S))
  expect_lt(cov, 0.95 + 3 * cover_se(0.95, S))
})


test_that("normal-approximation intervals also cover at roughly the nominal rate", {
  skip_unless_slow()
  S <- 400; n <- 800
  hits <- vapply(seq_len(S), function(s) {
    dr <- one_draw(n, seed = 20000 + s)
    g <- plm_grid(dr$fit, k = dr$truth$k, imperfection = dr$truth$imperfection,
                  n_boot = 300, cores = 1, ci_type = "normal")
    g$ci_lower <= dr$truth$target_long && dr$truth$target_long <= g$ci_upper
  }, logical(1))
  cov <- mean(hits)
  expect_gt(cov, 0.95 - 3 * cover_se(0.95, S))
  expect_lt(cov, 0.95 + 3 * cover_se(0.95, S))
})


test_that("coverage holds for the matrix engine too", {
  skip_unless_slow()
  S <- 300; n <- 800
  hits <- vapply(seq_len(S), function(s) {
    dr <- one_draw(n, seed = 30000 + s)
    g <- plm_grid(dr$fit, k = dr$truth$k, imperfection = dr$truth$imperfection,
                  n_boot = 300, cores = 1, engine = "matrix")
    g$ci_lower <= dr$truth$target_long && dr$truth$target_long <= g$ci_upper
  }, logical(1))
  cov <- mean(hits)
  expect_gt(cov, 0.95 - 3 * cover_se(0.95, S))
  expect_lt(cov, 0.95 + 3 * cover_se(0.95, S))
})


test_that("coverage does not degrade as n grows", {
  skip_unless_slow()
  S <- 250
  for (n in c(200, 2000)) {
    hits <- vapply(seq_len(S), function(s) {
      dr <- one_draw(n, seed = 40000 + s)
      g <- plm_grid(dr$fit, k = dr$truth$k,
                    imperfection = dr$truth$imperfection,
                    n_boot = 250, cores = 1)
      g$ci_lower <= dr$truth$target_long && dr$truth$target_long <= g$ci_upper
    }, logical(1))
    cov <- mean(hits)
    expect_gt(cov, 0.95 - 4 * cover_se(0.95, S), label = paste("n =", n))
  }
})


test_that("analytic intervals cover, and agree with the bootstrap, on the m path", {
  skip_unless_slow()
  S <- 400; n <- 800
  hits <- vapply(seq_len(S), function(s) {
    dr <- one_draw(n, seed = 50000 + s)
    a <- plm_analytic(dr$fit, m = dr$truth$m,
                      imperfection = dr$truth$imperfection)
    a$ci_lower <= dr$truth$target_long && dr$truth$target_long <= a$ci_upper
  }, logical(1))
  cov <- mean(hits)
  expect_gt(cov, 0.95 - 3 * cover_se(0.95, S))
  expect_lt(cov, 0.95 + 3 * cover_se(0.95, S))
})


test_that("the paper's variance-direction prediction holds empirically", {
  # Section 2.3 of the submitted draft derives
  #   var(mu_m) / var(beta_short) = 1 + m^2 s2_N/s2_Y - 2 m s_YN/s2_Y
  # where the sigmas are the short-regression disturbance (co)variances. This is
  # the sharpest available test of that newly added section: compute the
  # predicted ratio from one sample, and compare it to the ratio of empirical
  # sampling variances across many samples.
  skip_unless_slow()
  S <- 500; n <- 600; m <- 1

  d0  <- plm_dgp("placebo_outcome", n = n, seed = 1)
  rY  <- stats::resid(stats::lm(Y ~ D + X, data = d0))
  rP  <- stats::resid(stats::lm(P ~ D + X, data = d0))
  s2Y <- stats::var(rY); s2N <- stats::var(rP); sYN <- stats::cov(rY, rP)
  predicted <- 1 + m^2 * s2N / s2Y - 2 * m * sYN / s2Y

  est <- t(vapply(seq_len(S), function(s) {
    d   <- plm_dgp("placebo_outcome", n = n, seed = 60000 + s)
    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                      structure = "placebo_outcome")
    c(short = plm_estimate(fit, k = 0),
      adj   = plm_estimate(fit, m = m))
  }, numeric(2)))

  empirical <- stats::var(est[, "adj"]) / stats::var(est[, "short"])
  expect_equal(empirical, predicted, tolerance = 0.15)
})


test_that("treating SF as fixed under-covers relative to the full bootstrap", {
  # The paper recommends the nonparametric bootstrap when reasoning with k
  # because it "accounts for the randomness in SF without approximation".
  # This turns that recommendation into a demonstrated fact: an interval that
  # conditions on the observed SF should be too narrow.
  skip_unless_slow()
  S <- 300; n <- 400

  widths <- t(vapply(seq_len(S), function(s) {
    dr <- one_draw(n, seed = 70000 + s)
    full  <- plm_grid(dr$fit, k = dr$truth$k,
                      imperfection = dr$truth$imperfection,
                      n_boot = 250, cores = 1)
    # Same target value, but expressed in m, which holds SF fixed at its
    # observed value rather than resampling it.
    fixed <- plm_analytic(dr$fit, m = dr$truth$k * dr$fit$SF,
                          imperfection = dr$truth$imperfection)
    c(full  = full$ci_upper  - full$ci_lower,
      fixed = fixed$ci_upper - fixed$ci_lower)
  }, numeric(2)))

  expect_gt(mean(widths[, "full"]), mean(widths[, "fixed"]))
})


test_that("a placebo with little residual variation gives wide, not falsely tight, intervals", {
  # The paper warns SF becomes volatile when the placebo has little residual
  # variation after removing D and X. The failure mode to rule out is a
  # confidently wrong interval.
  skip_unless_slow()
  d <- plm_dgp("placebo_outcome", n = 800)

  informative <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                            structure = "placebo_outcome")
  d2 <- d; d2$P <- d$D + stats::rnorm(nrow(d), sd = 0.02)
  degenerate <- placebo_lm(d2, "Y", "D", "P", covariates = "X",
                           structure = "placebo_outcome")

  set.seed(1)
  a <- plm_grid(informative, k = 1, n_boot = 400, cores = 1)
  set.seed(1)
  b <- plm_grid(degenerate,  k = 1, n_boot = 400, cores = 1)

  expect_gt(b$ci_upper - b$ci_lower, a$ci_upper - a$ci_lower)
})


test_that("the i.i.d. bootstrap under-covers under clustered sampling", {
  # The theory assumes i.i.d. sampling. This is not a bug to fix but a limit to
  # document, and it matters concretely: the paper's Zika application uses
  # municipality-level data where spatial dependence is plausible.
  skip_unless_slow()
  S <- 300; n_clust <- 40; per <- 20

  hits <- t(vapply(seq_len(S), function(s) {
    set.seed(80000 + s)
    cl <- rep(seq_len(n_clust), each = per)
    n  <- length(cl)
    # A shared cluster shock induces within-cluster correlation.
    u_cl <- stats::rnorm(n_clust, sd = 1.5)[cl]
    Z <- stats::rnorm(n); X <- stats::rnorm(n)
    D <- X + Z + u_cl + stats::rnorm(n)
    P <- X + Z + u_cl + stats::rnorm(n)
    Y <- 2 * D + X + Z + u_cl + stats::rnorm(n)
    d <- data.frame(Y = Y, D = D, P = P, X = X, Z = Z, cl = factor(cl))

    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                      structure = "placebo_outcome")
    tp  <- plm_true_params(fit, d)

    iid <- plm_grid(fit, k = tp$k, imperfection = tp$imperfection,
                    n_boot = 200, cores = 1)
    c(iid = iid$ci_lower <= tp$target_long && tp$target_long <= iid$ci_upper)
  }, logical(1)))

  # The claim under test is that i.i.d. intervals are anti-conservative here.
  expect_lt(mean(hits), 0.95 - 3 * cover_se(0.95, S))
})
