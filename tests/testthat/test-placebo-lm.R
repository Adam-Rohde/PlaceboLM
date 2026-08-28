test_that("placebo_lm() validates its inputs", {
  d <- plm_test_data(n = 200)

  expect_error(placebo_lm("not a frame", "Y", "D", "P"), "must be a data frame")
  expect_error(placebo_lm(d[0, ], "Y", "D", "P"), "no rows")
  expect_error(placebo_lm(d, "nope", "D", "P"), "not found in `data`")
  expect_error(placebo_lm(d, "Y", "nope", "P"), "not found in `data`")
  expect_error(placebo_lm(d, "Y", "D", "nope"), "not found in `data`")
  expect_error(placebo_lm(d, "Y", "D", "P", covariates = c("X", "nope")),
               "not found in `data`")
  expect_error(placebo_lm(d, "Y", "D", "P", structure = "bogus"),
               "should be one of")
})

test_that("placebo_lm() rejects a variable used in two roles", {
  d <- plm_test_data(n = 200)
  expect_error(placebo_lm(d, "Y", "D", "D"), "distinct variables")
  expect_error(placebo_lm(d, "Y", "D", "P", covariates = "D"),
               "distinct variables")
})

test_that("placebo_lm() fits each regression exactly once", {
  d <- plm_test_data(n = 500)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  expect_named(fit$regressions, c("target", "sens"))
  expect_s3_class(fit$regressions$target, "lm")
  expect_s3_class(fit$regressions$sens, "lm")
  expect_s3_class(fit, "placebo_lm")
})

test_that("stored coefficients match the fitted regressions", {
  d <- plm_test_data(n = 500)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  ct <- stats::coef(summary(fit$regressions$target))
  expect_equal(fit$coefs$target$estimate, unname(ct["D", "Estimate"]))
  expect_equal(fit$coefs$target$se,       unname(ct["D", "Std. Error"]))
  expect_equal(fit$coefs$target$df,       fit$regressions$target$df.residual)
})

test_that("covariates are optional", {
  d <- plm_test_data(n = 300)
  fit <- placebo_lm(d, "Y", "D", "P", structure = "placebo_outcome")
  expect_equal(deparse1(fit$formulas$target), "Y ~ D")
})

test_that("every structure fits without error", {
  d <- plm_test_data(n = 500)
  for (s in names(plm_structures)) {
    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X", structure = s)
    expect_s3_class(fit, "placebo_lm")
    expect_true(is.finite(fit$SF), info = s)
    expect_true(is.finite(plm_estimate(fit, k = 1)), info = s)
  }
})

test_that("imperfection must be numeric", {
  d <- plm_test_data(n = 200)
  fit <- placebo_lm(d, "Y", "D", "P", structure = "placebo_outcome")
  expect_error(plm_estimate(fit, k = 1, imperfection = "zero"),
               "must be numeric")
  expect_error(plm_estimate(fit, k = 1, imperfection = NA_real_),
               "must not contain NA")
})

test_that("print and summary do not dump the dataset", {
  d <- plm_test_data(n = 1000)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  out <- utils::capture.output(print(fit))
  # The point is that printing summarises rather than dumping 1000 rows of data.
  expect_lt(length(out), 60L)
  expect_lt(length(out), nrow(d) / 10)
  expect_true(any(grepl("Placebo Outcome", out)))
  expect_true(any(grepl("scale factor", out)))
  expect_true(any(grepl("Estimand", out)))
  expect_true(any(grepl("Assumptions implied", out)))

  s <- utils::capture.output(print(summary(fit)))
  expect_true(any(grepl("Tipping point", s)))
})

test_that("coef() exposes the estimated quantities", {
  d <- plm_test_data(n = 300)
  fit <- placebo_lm(d, "Y", "D", "P", structure = "placebo_outcome")
  cf <- stats::coef(fit)
  expect_named(cf, c("target", "sens", "SF"))
  expect_equal(unname(cf[["SF"]]), fit$SF)
})
