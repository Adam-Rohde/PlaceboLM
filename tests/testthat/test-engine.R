# The matrix bootstrap engine must be numerically indistinguishable from the
# lm engine, which is the path whose numbers back the published results.
#
# The engine is opt-in, but opt-in is not a licence to test it lightly: a user
# who switches it on is entitled to identical answers. Everything here is a
# precondition for the fast path being usable at all. If any case cannot be made
# to agree, the right response is to report that, not to loosen the tolerance.

TOL <- 1e-10

# Compare the two engines on the same resamples by fixing the seed either side.
engines_agree <- function(fit, k = 1, imperfection = 0, n_boot = 120,
                          tol = TOL, seed = 99, info = NULL) {
  set.seed(seed)
  a <- plm_grid(fit, k = k, imperfection = imperfection, n_boot = n_boot,
                cores = 1, engine = "lm")
  set.seed(seed)
  b <- plm_grid(fit, k = k, imperfection = imperfection, n_boot = n_boot,
                cores = 1, engine = "matrix")
  testthat::expect_equal(a, b, tolerance = tol, info = info)
}


test_that("the two engines agree for every structure", {
  for (s in names(plm_structures)) {
    d   <- plm_dgp(s, n = 800)
    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X", structure = s)
    engines_agree(fit, info = s)
  }
})


test_that("agreement holds across a grid of k and imperfection, including negatives", {
  d   <- plm_dgp("placebo_outcome", n = 800)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  engines_agree(fit, k = c(-2, -0.5, 0, 0.75, 3),
                imperfection = c(-0.4, 0, 0.6))
})


test_that("agreement holds for plm_bounds, including its bootstrap quantiles", {
  d   <- plm_dgp("placebo_outcome", n = 800)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  set.seed(5)
  a <- plm_bounds(fit, k = c(0.5, 1), n_boot = 150, cores = 1, engine = "lm")
  set.seed(5)
  b <- plm_bounds(fit, k = c(0.5, 1), n_boot = 150, cores = 1, engine = "matrix")
  expect_equal(a, b, tolerance = TOL)
})


test_that("the per-replicate quantities themselves agree, not just the summaries", {
  # A summary could agree by luck or by cancellation. Compare the underlying
  # coefficient, standard error, df and scale factor replicate by replicate.
  for (s in names(plm_structures)) {
    d   <- plm_dgp(s, n = 600)
    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X", structure = s)
    prep <- PlaceboLM:::.plm_matrix_prep(fit)

    set.seed(11)
    for (i in 1:15) {
      ii <- sample.int(nrow(d), nrow(d), replace = TRUE)
      a <- PlaceboLM:::.plm_refit(fit, d[ii, , drop = FALSE])
      b <- PlaceboLM:::.plm_refit_matrix(fit, prep, ii)
      expect_equal(a$target$estimate, b$target$estimate, tolerance = TOL, info = s)
      expect_equal(a$target$se,       b$target$se,       tolerance = TOL, info = s)
      expect_equal(a$target$df,       b$target$df,                        info = s)
      expect_equal(a$sens$estimate,   b$sens$estimate,   tolerance = TOL, info = s)
      expect_equal(a$sens$se,         b$sens$se,         tolerance = TOL, info = s)
      expect_equal(a$SF,              b$SF,              tolerance = TOL, info = s)
    }
  }
})


test_that("agreement holds at small and large n", {
  for (n in c(30, 20000)) {
    d   <- plm_dgp("placebo_outcome", n = n)
    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                      structure = "placebo_outcome")
    engines_agree(fit, n_boot = if (n > 5000) 40 else 120,
                  info = paste("n =", n))
  }
})


test_that("agreement holds with factor covariates", {
  d <- plm_dgp("placebo_outcome", n = 900)
  d$g <- factor(sample(letters[1:4], nrow(d), replace = TRUE))
  fit <- placebo_lm(d, "Y", "D", "P", covariates = c("X", "g"),
                    structure = "placebo_outcome")
  engines_agree(fit)
})


test_that("agreement holds when a resample drops a factor level", {
  # A rare level will be missing from some resamples, leaving an all-zero column
  # in the precomputed design matrix. lm() would drop the level; the matrix
  # engine must handle the resulting rank deficiency the same way.
  d <- plm_dgp("placebo_outcome", n = 400)
  d$g <- factor(c(rep("common", nrow(d) - 3), rep("rare", 3)))
  fit <- placebo_lm(d, "Y", "D", "P", covariates = c("X", "g"),
                    structure = "placebo_outcome")

  prep <- PlaceboLM:::.plm_matrix_prep(fit)
  # Force a resample that omits every "rare" row.
  ii <- sample(which(d$g == "common"), nrow(d), replace = TRUE)
  a <- tryCatch(PlaceboLM:::.plm_refit(fit, d[ii, , drop = FALSE]),
                error = function(e) NULL)
  b <- PlaceboLM:::.plm_refit_matrix(fit, prep, ii)

  # Both must reach the same verdict: either both usable and equal, or both
  # rejected. What must not happen is one silently returning a number.
  if (is.null(a)) {
    expect_null(b)
  } else {
    expect_equal(a$target$estimate, b$target$estimate, tolerance = TOL)
    expect_equal(a$target$se,       b$target$se,       tolerance = TOL)
    expect_equal(a$SF,              b$SF,              tolerance = TOL)
  }
})


test_that("agreement holds with a placebo of tiny residual variance", {
  # The paper warns SF becomes volatile here. Volatile is fine; disagreeing
  # between engines is not.
  d <- plm_dgp("placebo_outcome", n = 800)
  d$P <- d$D + stats::rnorm(nrow(d), sd = 1e-3)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  engines_agree(fit, n_boot = 80, tol = 1e-8)
})


test_that("agreement holds with no covariates", {
  d   <- plm_dgp("placebo_outcome", n = 600)
  fit <- placebo_lm(d, "Y", "D", "P", structure = "placebo_outcome")
  engines_agree(fit)
})


test_that("the matrix engine is reproducible and core-count invariant", {
  skip_on_os("windows")
  d   <- plm_dgp("placebo_outcome", n = 600)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  set.seed(3); a <- plm_grid(fit, k = 1, n_boot = 120, cores = 1, engine = "matrix")
  set.seed(3); b <- plm_grid(fit, k = 1, n_boot = 120, cores = 2, engine = "matrix")
  expect_equal(a, b)
})


test_that("the design-sharing optimisation applies where the paper's cases need it", {
  # placebo_outcome runs two regressions on the same right-hand side, so one QR
  # serves both. That is the case both empirical applications use.
  d <- plm_dgp("placebo_outcome", n = 300)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  expect_length(PlaceboLM:::.plm_matrix_prep(fit), 1L)

  # post_outcome does not: its two regressions differ.
  d2 <- plm_dgp("post_outcome", n = 300)
  f2 <- placebo_lm(d2, "Y", "D", "P", covariates = "X",
                   structure = "post_outcome")
  expect_length(PlaceboLM:::.plm_matrix_prep(f2), 2L)
})


test_that("engine is validated rather than silently ignored", {
  d   <- plm_dgp("placebo_outcome", n = 300)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  expect_error(plm_grid(fit, k = 1, n_boot = 10, engine = "fast"),
               "should be one of")
})
