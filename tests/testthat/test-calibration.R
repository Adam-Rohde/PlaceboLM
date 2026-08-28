# Tier 2 and 3 statistical validation: is the INFERENCE calibrated?
#
# test-recovery.R shows the point estimator returns the right quantity given the
# right parameters. That says nothing about whether the intervals around it have
# the coverage they claim.
#
# A trap worth recording, because the first version of this file fell into it.
# The recovery identity is EXACT in sample: m is defined as the ratio of the two
# omitted-variable biases, so if the "true" parameters are recomputed from the
# same sample the estimator is fitted on, the point estimate equals the target
# to machine precision every single draw and coverage is 1 by construction. The
# tests then pass or fail for reasons that have nothing to do with inference.
#
# So the target and the sensitivity parameters here are POPULATION quantities,
# taken once from a very large draw (see plm_population() in helper-dgp.R) and
# held fixed across simulations. The estimator then has genuine sampling
# variability around a fixed target, which is what coverage is about.
#
# These are Monte Carlo and slow, so they are skipped unless
# PLACEBOLM_SLOW_TESTS is set:
#
#   PLACEBOLM_SLOW_TESTS=true Rscript -e 'testthat::test_local()'
#
# Coverage is judged against its Monte Carlo standard error, so a near-miss is
# not read as a failure and a real miss is not excused.

skip_unless_slow <- function() {
  if (!nzchar(Sys.getenv("PLACEBOLM_SLOW_TESTS")))
    testthat::skip("slow Monte Carlo test; set PLACEBOLM_SLOW_TESTS to run")
}

cover_se <- function(p, S) sqrt(p * (1 - p) / S)

# Run S simulations and return the proportion of intervals covering `target`.
# `interval` maps a fitted object to c(lower, upper).
coverage_of <- function(interval, target, S, n, seed0,
                        structure = "placebo_outcome") {
  hits <- vapply(seq_len(S), function(s) {
    d   <- plm_dgp(structure, n = n, seed = seed0 + s)
    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                      structure = structure)
    ci  <- interval(fit)
    isTRUE(ci[1] <= target && target <= ci[2])
  }, logical(1))
  mean(hits)
}

expect_near_nominal <- function(cov, S, nominal = 0.95, mult = 3, info = NULL) {
  se <- cover_se(nominal, S)
  testthat::expect_gt(cov, nominal - mult * se)
  testthat::expect_lt(cov, nominal + mult * se)
}


test_that("population parameters recover the values built into the DGP", {
  # A guard on the fixtures themselves: if this drifts, every coverage number
  # below is measuring the wrong thing.
  skip_unless_slow()
  pop <- plm_population("placebo_outcome")
  expect_equal(pop$target_long,  2.0, tolerance = 0.01)   # DGP effect
  expect_equal(pop$imperfection, 0.3, tolerance = 0.02)   # DGP D -> P path
})


test_that("percentile bootstrap intervals cover at roughly the nominal rate", {
  skip_unless_slow()
  pop <- plm_population("placebo_outcome")
  S <- 300; n <- 600
  cov <- coverage_of(function(fit) {
    g <- plm_grid(fit, k = pop$k, imperfection = pop$imperfection,
                  n_boot = 250, cores = 1, ci_type = "percentile",
                  engine = "matrix")
    c(g$ci_lower, g$ci_upper)
  }, pop$target_long, S, n, seed0 = 100000)
  expect_near_nominal(cov, S)
})


test_that("normal-approximation intervals cover at roughly the nominal rate", {
  skip_unless_slow()
  pop <- plm_population("placebo_outcome")
  S <- 300; n <- 600
  cov <- coverage_of(function(fit) {
    g <- plm_grid(fit, k = pop$k, imperfection = pop$imperfection,
                  n_boot = 250, cores = 1, ci_type = "normal",
                  engine = "matrix")
    c(g$ci_lower, g$ci_upper)
  }, pop$target_long, S, n, seed0 = 200000)
  expect_near_nominal(cov, S)
})


test_that("the default lm engine covers just as the matrix engine does", {
  # The matrix engine is used above for speed. The default is what most users
  # will run, so it gets its own check.
  skip_unless_slow()
  pop <- plm_population("placebo_outcome")
  S <- 200; n <- 600
  cov <- coverage_of(function(fit) {
    g <- plm_grid(fit, k = pop$k, imperfection = pop$imperfection,
                  n_boot = 250, cores = 1, engine = "lm")
    c(g$ci_lower, g$ci_upper)
  }, pop$target_long, S, n, seed0 = 300000)
  expect_near_nominal(cov, S)
})


test_that("analytic intervals cover on the m path", {
  skip_unless_slow()
  pop <- plm_population("placebo_outcome")
  S <- 300; n <- 600
  cov <- coverage_of(function(fit) {
    a <- plm_analytic(fit, m = pop$m, imperfection = pop$imperfection)
    c(a$ci_lower, a$ci_upper)
  }, pop$target_long, S, n, seed0 = 400000)
  expect_near_nominal(cov, S)
})


test_that("coverage does not degrade as n grows", {
  skip_unless_slow()
  pop <- plm_population("placebo_outcome")
  S <- 200
  for (n in c(200, 2000)) {
    cov <- coverage_of(function(fit) {
      g <- plm_grid(fit, k = pop$k, imperfection = pop$imperfection,
                    n_boot = 250, cores = 1, engine = "matrix")
      c(g$ci_lower, g$ci_upper)
    }, pop$target_long, S, n, seed0 = 500000 + n)
    expect_near_nominal(cov, S, mult = 4, info = paste("n =", n))
  }
})


test_that("the paper's variance-direction prediction holds empirically", {
  # Section 2.3 of the submitted draft derives
  #   var(mu_m) / var(beta_short) = 1 + m^2 s2_N/s2_Y - 2 m s_YN/s2_Y
  # with the sigmas taken from the short-regression disturbances. This is the
  # sharpest available test of that newly added section: compare the predicted
  # ratio against the ratio of empirical sampling variances.
  skip_unless_slow()
  S <- 400; n <- 600; m <- 1

  d0  <- plm_dgp("placebo_outcome", n = 200000, seed = 4242)
  rY  <- stats::resid(stats::lm(Y ~ D + X, data = d0))
  rP  <- stats::resid(stats::lm(P ~ D + X, data = d0))
  s2Y <- stats::var(rY); s2N <- stats::var(rP); sYN <- stats::cov(rY, rP)
  predicted <- 1 + m^2 * s2N / s2Y - 2 * m * sYN / s2Y

  est <- t(vapply(seq_len(S), function(s) {
    d   <- plm_dgp("placebo_outcome", n = n, seed = 600000 + s)
    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                      structure = "placebo_outcome")
    c(short = plm_estimate(fit, k = 0), adj = plm_estimate(fit, m = m))
  }, numeric(2)))

  empirical <- stats::var(est[, "adj"]) / stats::var(est[, "short"])
  expect_equal(empirical, predicted, tolerance = 0.2)
})


test_that("resampling SF gives wider intervals than holding it fixed", {
  # The paper recommends the nonparametric bootstrap when reasoning with k
  # because it "accounts for the randomness in SF without approximation".
  # Demonstrated rather than quoted: an interval conditioning on the observed
  # SF should be narrower than one that lets it vary.
  skip_unless_slow()
  pop <- plm_population("placebo_outcome")
  S <- 200; n <- 400

  w <- t(vapply(seq_len(S), function(s) {
    d   <- plm_dgp("placebo_outcome", n = n, seed = 700000 + s)
    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                      structure = "placebo_outcome")
    full  <- plm_grid(fit, k = pop$k, imperfection = pop$imperfection,
                      n_boot = 250, cores = 1, engine = "matrix")
    fixed <- plm_analytic(fit, m = pop$k * fit$SF,
                          imperfection = pop$imperfection)
    c(full = full$ci_upper - full$ci_lower,
      fixed = fixed$ci_upper - fixed$ci_lower)
  }, numeric(2)))

  expect_gt(mean(w[, "full"]), mean(w[, "fixed"]))
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

  set.seed(1); a <- plm_grid(informative, k = 1, n_boot = 400, cores = 1)
  set.seed(1); b <- plm_grid(degenerate,  k = 1, n_boot = 400, cores = 1)
  expect_gt(b$ci_upper - b$ci_lower, a$ci_upper - a$ci_lower)
})


# --- Dependent sampling ------------------------------------------------------
#
# Regression guards on how the interval behaves when rows are not independent.
# The two generators place the cluster structure differently; the expectations
# below record current behaviour so that a change to the resampling scheme
# shows up here.

test_that("clustered sampling, cluster structure in the confounder", {
  # Cluster structure confined to the confounder.
  skip_unless_slow()
  pop <- plm_population_clustered(shock = "confounder")
  S <- 250

  hits <- vapply(seq_len(S), function(s) {
    d   <- plm_dgp_clustered(n_clust = 40, per = 20, seed = 800000 + s,
                             shock = "confounder")
    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                      structure = "placebo_outcome")
    g <- plm_grid(fit, k = pop$k, imperfection = pop$imperfection,
                  n_boot = 250, cores = 1, engine = "matrix")
    isTRUE(g$ci_lower <= pop$target_long && pop$target_long <= g$ci_upper)
  }, logical(1))

  expect_near_nominal(mean(hits), S, mult = 4)
})


test_that("clustered sampling, cluster shock also in the outcome", {
  # Z has a cluster component and Y carries a further cluster shock that P does
  # not share.
  skip_unless_slow()
  pop <- plm_population_clustered(shock = "outcome")
  S <- 250

  hits <- vapply(seq_len(S), function(s) {
    d   <- plm_dgp_clustered(n_clust = 40, per = 20, seed = 900000 + s,
                             shock = "outcome")
    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                      structure = "placebo_outcome")
    g <- plm_grid(fit, k = pop$k, imperfection = pop$imperfection,
                  n_boot = 250, cores = 1, engine = "matrix")
    isTRUE(g$ci_lower <= pop$target_long && pop$target_long <= g$ci_upper)
  }, logical(1))

  expect_lt(mean(hits), 0.95 - 2 * cover_se(0.95, S))
})
