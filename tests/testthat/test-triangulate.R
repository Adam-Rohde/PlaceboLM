# Triangulation: combining several placebos, and comparing several readings of
# one placebo. These are different operations and are tested separately.

mkdata <- function(n = 800, seed = 1) {
  set.seed(seed)
  U <- stats::rnorm(n); D <- U + stats::rnorm(n)
  data.frame(Y  = 2 * D + U + stats::rnorm(n), D = D,
             P1 = U + stats::rnorm(n),
             P2 = U + stats::rnorm(n),
             P3 = stats::rbinom(n, 1, stats::plogis(U)),
             X  = stats::rnorm(n))
}
fit_on <- function(d, p, cov = NULL)
  placebo_lm(d, "Y", "D", p, covariates = cov, structure = "placebo_outcome")


# ---- plm_triangulate ---------------------------------------------------------

test_that("the intersection is the tightest interval inside every bound", {
  d <- mkdata()
  fits <- list(P1 = fit_on(d, "P1"), P2 = fit_on(d, "P2"))
  out <- plm_triangulate(fits, k = c(0.5, 1))

  expect_equal(nrow(out), 3L)
  expect_equal(out$placebo, c("P1", "P2", "(intersection)"))

  own <- out[out$placebo != "(intersection)", ]
  inter <- out[out$placebo == "(intersection)", ]
  expect_equal(inter$lower, max(own$lower))
  expect_equal(inter$upper, min(own$upper))
  # and it is no wider than any contributing bound
  expect_lte(inter$upper - inter$lower, min(own$upper - own$lower))
})


test_that("each row matches plm_bounds() run on that fit alone", {
  d <- mkdata()
  f1 <- fit_on(d, "P1"); f2 <- fit_on(d, "P2")
  out <- plm_triangulate(list(P1 = f1, P2 = f2), k = c(0.5, 1))
  b1  <- plm_bounds(f1, k = c(0.5, 1), n_boot = 0)
  expect_equal(out$lower[1], b1$lower)
  expect_equal(out$upper[1], b1$upper)
})


test_that("per-placebo ranges are honoured", {
  # The paper argues a different plausible range for different placebos, so the
  # ranges must be specifiable per fit rather than only globally.
  d <- mkdata()
  fits <- list(P1 = fit_on(d, "P1"), P2 = fit_on(d, "P2"))
  out <- plm_triangulate(fits, k = list(P1 = c(0, 1), P2 = c(0.5, 1.5)))
  expect_equal(out$k_low[1:2],  c(0, 0.5))
  expect_equal(out$k_high[1:2], c(1, 1.5))
})


test_that("a non-overlapping set warns and reports no intersection", {
  # Informative rather than an error: it means at least one assumed range is
  # wrong, which is something the analyst needs to see.
  d <- mkdata()
  a <- fit_on(d, "P1"); b <- fit_on(d, "P3")
  expect_warning(
    out <- plm_triangulate(list(P1 = a, P3 = b),
                           k = list(P1 = c(0, 0.2), P3 = c(3, 4))),
    "do not overlap")
  inter <- out[out$placebo == "(intersection)", ]
  expect_true(is.na(inter$lower))
  expect_true(is.na(inter$upper))
})


test_that("triangulation refuses fits that are not the same analysis", {
  d <- mkdata()
  expect_error(plm_triangulate(list(a = fit_on(d, "P1")), k = c(0, 1)),
               "at least two")
  expect_error(plm_triangulate(list(fit_on(d, "P1"), fit_on(d, "P2")),
                               k = c(0, 1)), "must be named")
  # differing covariates
  expect_error(
    plm_triangulate(list(a = fit_on(d, "P1"), b = fit_on(d, "P2", cov = "X")),
                    k = c(0, 1)), "same covariates")
  # the same placebo twice is not triangulation
  expect_error(
    plm_triangulate(list(a = fit_on(d, "P1"), b = fit_on(d, "P1")),
                    k = c(0, 1)), "different ones")
  # a different outcome
  d2 <- d; d2$Y2 <- d$Y + 1
  f_other <- placebo_lm(d2, "Y2", "D", "P2", structure = "placebo_outcome")
  expect_error(plm_triangulate(list(a = fit_on(d, "P1"), b = f_other),
                               k = c(0, 1)), "same outcome and treatment")
})


test_that("bootstrap columns are per placebo and are not combined", {
  # Deliberate: the fits share rows, so their sampling errors are dependent and
  # combining the quantiles would understate uncertainty.
  d <- mkdata(n = 400)
  set.seed(1)
  out <- plm_triangulate(list(P1 = fit_on(d, "P1"), P2 = fit_on(d, "P2")),
                         k = c(0.5, 1), n_boot = 100, cores = 1)
  expect_true(all(c("lower_boot_q", "upper_boot_q") %in% names(out)))
  own <- out[out$placebo != "(intersection)", ]
  expect_true(all(is.finite(own$lower_boot_q)))
  inter <- out[out$placebo == "(intersection)", ]
  expect_true(is.na(inter$lower_boot_q))
  expect_true(is.na(inter$upper_boot_q))
})


test_that("k and m are handled, and exactly one is required", {
  d <- mkdata()
  fits <- list(P1 = fit_on(d, "P1"), P2 = fit_on(d, "P2"))
  expect_s3_class(plm_triangulate(fits, m = c(0.5, 1)), "data.frame")
  expect_error(plm_triangulate(fits), "exactly one")
})


# ---- plm_compare_structures --------------------------------------------------

test_that("comparing structures returns one row per structure", {
  d <- mkdata()
  out <- plm_compare_structures(d, "Y", "D", "P2")
  expect_equal(nrow(out), length(plm_structures))
  expect_setequal(out$structure, names(plm_structures))
  expect_true(all(c("SF", "at_k0", "at_k1", "m_equals_1",
                    "tipping_k") %in% names(out)))
  expect_false("placebo" %in% names(out))
})


test_that("each row matches fitting that structure directly", {
  d <- mkdata()
  out <- plm_compare_structures(d, "Y", "D", "P2",
                                structures = c("placebo_outcome",
                                               "post_outcome"))
  for (i in seq_len(nrow(out))) {
    f <- placebo_lm(d, "Y", "D", "P2", structure = out$structure[i])
    expect_equal(out$SF[i],    f$SF)
    expect_equal(out$at_k0[i], plm_estimate(f, k = 0))
    expect_equal(out$at_k1[i], plm_estimate(f, k = 1))
    expect_equal(out$tipping_k[i], plm_solve(f, target = 0)$k)
  }
})


test_that("the structure subset is validated", {
  d <- mkdata()
  expect_error(plm_compare_structures(d, "Y", "D", "P2",
                                      structures = c("placebo_outcome", "nope")),
               "Unknown structure")
})


test_that("comparing structures respects covariates and imperfection", {
  d <- mkdata()
  out <- plm_compare_structures(d, "Y", "D", "P2", covariates = "X",
                                structures = "placebo_outcome",
                                imperfection = 0.2)
  f <- placebo_lm(d, "Y", "D", "P2", covariates = "X",
                  structure = "placebo_outcome")
  expect_equal(out$at_k1, plm_estimate(f, k = 1, imperfection = 0.2))
})
