# Bootstrap reproducibility.
#
# parallel::mclapply does NOT respect set.seed() for RNG drawn inside the forked
# workers, which is a classic way for a bootstrap to become irreproducible the
# moment someone sets cores > 1. This package avoids the problem by drawing all
# bootstrap indices in the parent process, before forking, so the workers do no
# random number generation at all.
#
# That is a property of the implementation, not an accident to be rediscovered,
# so it is pinned here.

test_that("bootstrap results are reproducible at cores = 1", {
  d <- plm_dgp("placebo_outcome", n = 600)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  set.seed(42); a <- plm_grid(fit, k = c(0, 1), n_boot = 150, cores = 1)
  set.seed(42); b <- plm_grid(fit, k = c(0, 1), n_boot = 150, cores = 1)
  expect_equal(a, b)
})

test_that("bootstrap results are reproducible at cores = 2", {
  skip_on_os("windows")
  d <- plm_dgp("placebo_outcome", n = 600)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  set.seed(42); a <- plm_grid(fit, k = c(0, 1), n_boot = 150, cores = 2)
  set.seed(42); b <- plm_grid(fit, k = c(0, 1), n_boot = 150, cores = 2)
  expect_equal(a, b)
})

test_that("the same seed gives the same answer regardless of core count", {
  # Indices are drawn in the parent, so the resamples themselves do not depend
  # on how the work is later divided among workers.
  skip_on_os("windows")
  d <- plm_dgp("placebo_outcome", n = 600)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  set.seed(7); one <- plm_grid(fit, k = 1, n_boot = 150, cores = 1)
  set.seed(7); two <- plm_grid(fit, k = 1, n_boot = 150, cores = 2)
  expect_equal(one$std_error, two$std_error)
  expect_equal(one$ci_lower,  two$ci_lower)
})

test_that("different seeds give different resamples", {
  # Guards against the opposite failure: a bootstrap that is reproducible
  # because it is not actually random.
  d <- plm_dgp("placebo_outcome", n = 600)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  set.seed(1); a <- plm_grid(fit, k = 1, n_boot = 150, cores = 1)
  set.seed(2); b <- plm_grid(fit, k = 1, n_boot = 150, cores = 1)
  expect_false(isTRUE(all.equal(a$std_error, b$std_error)))
  # ... but the point estimate is not random at all.
  expect_equal(a$adjusted_coefficient, b$adjusted_coefficient)
})

test_that("plm_bounds is reproducible too", {
  d <- plm_dgp("placebo_outcome", n = 600)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  set.seed(3); a <- plm_bounds(fit, k = c(0.5, 1), n_boot = 150, cores = 1)
  set.seed(3); b <- plm_bounds(fit, k = c(0.5, 1), n_boot = 150, cores = 1)
  expect_equal(a, b)
})
