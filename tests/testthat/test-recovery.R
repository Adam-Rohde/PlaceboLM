# Tier 1 statistical validation: does the method recover the truth?
#
# The rest of the suite checks that the code implements the paper's formulas.
# These tests check something stronger and more useful: that under data
# generated from each structure's assumed causal graph, feeding the TRUE
# sensitivity parameters into the estimator returns the quantity the method
# claims to partially identify -- the coefficient from the infeasible long
# regression that includes the unobserved confounder.
#
# The recovery identity is exact IN SAMPLE, not asymptotic: m is defined as the
# ratio of the two omitted-variable biases, so substituting it back cancels the
# bias term exactly. Tolerances are therefore machine precision, not a Monte
# Carlo band, and these tests are deterministic.

structures <- c("placebo_outcome", "placebo_treatment",
                "observed_confounder_1", "observed_confounder_2",
                "post_outcome")


test_that("the true parameters recover the long-regression coefficient", {
  for (s in structures) {
    d   <- plm_dgp(s)
    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X", structure = s)
    tp  <- plm_true_params(fit, d)

    expect_equal(
      plm_estimate(fit, k = tp$k, imperfection = tp$imperfection),
      tp$target_long,
      tolerance = 1e-10,
      info = s
    )
  }
})


test_that("recovery also holds through the m parameterization", {
  for (s in structures) {
    d   <- plm_dgp(s)
    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X", structure = s)
    tp  <- plm_true_params(fit, d)

    expect_equal(
      plm_estimate(fit, m = tp$m, imperfection = tp$imperfection),
      tp$target_long,
      tolerance = 1e-10,
      info = s
    )
  }
})


test_that("recovery is not an artefact of one sample or one effect size", {
  for (s in structures) {
    for (sd in c(2, 7)) {
      for (b in c(0, 2, -1.5)) {
        d   <- plm_dgp(s, n = 1500, seed = sd, beta_D = b)
        fit <- placebo_lm(d, "Y", "D", "P", covariates = "X", structure = s)
        tp  <- plm_true_params(fit, d)
        expect_equal(
          plm_estimate(fit, k = tp$k, imperfection = tp$imperfection),
          tp$target_long, tolerance = 1e-10,
          info = paste(s, "seed", sd, "beta", b)
        )
      }
    }
  }
})


test_that("assuming no unobserved confounding is biased, as it should be", {
  # A sanity check on the fixtures themselves: if the naive estimate already
  # equalled the long-regression coefficient there would be no confounding to
  # remove and the recovery test above would be vacuous.
  for (s in structures) {
    d   <- plm_dgp(s)
    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X", structure = s)
    tp  <- plm_true_params(fit, d)
    naive <- plm_estimate(fit, k = 0)
    expect_gt(abs(naive - tp$target_long), 0.05)
  }
})


test_that("the true k is recovered by plm_solve() from the true target", {
  # The inverse direction: given the answer, plm_solve() should return the
  # relative confounding that produces it. This is how the paper backs out
  # k = 0.812 from the NSW experimental benchmark.
  for (s in structures) {
    d   <- plm_dgp(s)
    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X", structure = s)
    tp  <- plm_true_params(fit, d)
    got <- plm_solve(fit, target = tp$target_long,
                     imperfection = tp$imperfection)
    expect_equal(got$k, tp$k, tolerance = 1e-10, info = s)
    expect_equal(got$m, tp$m, tolerance = 1e-10, info = s)
  }
})


# ---- The scale factor, validated substantively -------------------------------
#
# The recovery tests above do NOT validate SF. They compute k = m / SF using the
# package's own SF and the package then multiplies it back out, so SF cancels.
# The paper's own definition of k gives an independent check: with a single
# omitted confounder, k is a ratio of partial correlations (placebo outcome) or
# of Cohen's f values (placebo treatment). Those identities involve SF, so
# matching them pins SF from a direction the recovery tests cannot.

test_that("SF matches the paper's partial-correlation definition of k (placebo outcome)", {
  # k = R_{Y~Z|D,X} / R_{P~Z|D,X}
  d   <- plm_dgp("placebo_outcome", n = 200000, seed = 3)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  k_pkg   <- plm_true_params(fit, d)$k
  k_paper <- plm_pcor("Y", "Z", c("D", "X"), d) /
             plm_pcor("P", "Z", c("D", "X"), d)
  expect_equal(k_pkg, k_paper, tolerance = 1e-8)
})


test_that("SF matches the paper's Cohen's f definition of k (placebo treatment)", {
  # k = f_{D~Z|P,X} / f_{P~Z|D,X}
  d   <- plm_dgp("placebo_treatment", n = 200000, seed = 3)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_treatment")
  k_pkg   <- plm_true_params(fit, d)$k
  k_paper <- plm_cohen_f("D", "Z", c("P", "X"), d) /
             plm_cohen_f("P", "Z", c("D", "X"), d)
  expect_equal(k_pkg, k_paper, tolerance = 1e-8)
})


# ---- Invariance and boundary properties --------------------------------------

test_that("the k-parameterized estimate is invariant to rescaling the placebo", {
  # Rescaling P must leave the k-parameterized answer untouched while SF absorbs
  # the scale change. This is the whole reason k exists alongside m, and it is
  # what makes the paper's binary-employment placebo (SF > 40,000) usable.
  d  <- plm_dgp("placebo_outcome")
  f1 <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                   structure = "placebo_outcome")

  d2 <- d; d2$P <- d$P * 1000
  f2 <- placebo_lm(d2, "Y", "D", "P", covariates = "X",
                   structure = "placebo_outcome")

  for (k in c(-1, 0, 0.5, 1, 2.5))
    expect_equal(plm_estimate(f1, k = k), plm_estimate(f2, k = k),
                 tolerance = 1e-10)

  expect_equal(f2$SF, f1$SF / 1000, tolerance = 1e-10)
})


test_that("m is NOT scale-free, which is why k exists", {
  # The complementary negative: the same rescaling changes what m = 1 means.
  d  <- plm_dgp("placebo_outcome")
  f1 <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                   structure = "placebo_outcome")
  d2 <- d; d2$P <- d$P * 1000
  f2 <- placebo_lm(d2, "Y", "D", "P", covariates = "X",
                   structure = "placebo_outcome")
  expect_false(isTRUE(all.equal(plm_estimate(f1, m = 1),
                                plm_estimate(f2, m = 1))))
})


test_that("k = 0 returns the short-regression coefficient exactly", {
  for (s in structures) {
    d   <- plm_dgp(s)
    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X", structure = s)
    expect_equal(plm_estimate(fit, k = 0), fit$coefs$target$estimate,
                 tolerance = 1e-12, info = s)
  }
})


test_that("no adjustment is made when the placebo is exactly as imperfect as observed", {
  # If the postulated imperfection equals the observed sensitivity coefficient
  # there is no bias signal to scale, so the estimate must be the unadjusted one
  # for every k.
  for (s in structures) {
    d   <- plm_dgp(s)
    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X", structure = s)
    imp <- fit$coefs$sens$estimate
    for (k in c(-2, 0, 1, 5))
      expect_equal(plm_estimate(fit, k = k, imperfection = imp),
                   fit$coefs$target$estimate, tolerance = 1e-12, info = s)
  }
})


test_that("the estimate is monotone in k, with sign set by the bias direction", {
  for (s in structures) {
    d   <- plm_dgp(s)
    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X", structure = s)
    ks  <- seq(-2, 2, length.out = 50)
    est <- plm_estimate(fit, k = ks)
    dif <- diff(est)
    expect_true(all(dif > 0) || all(dif < 0), info = s)
  }
})
