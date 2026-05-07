# Tests use a synthetic dataset where confounding is fully controlled by X and Z
# so that the "true" causal effect (D -> Y = 1) is recoverable and identifiable.
#
# DGP:
#   U ~ N(0,1)  (unobserved confounder)
#   X ~ N(0,1)  (observed covariate)
#   D = X + U + N(0,1)
#   P = X + U + N(0,1)  (placebo with same confounders as D)
#   Y = D + X + U + N(0,1)   (true causal effect of D is 1)
#   N = D + X + U + N(0,1)   (placebo outcome, same confounders)
#
# When we condition on X and Z=U (i.e. the unobserved confounder is treated as
# observed in the "oracle" regression), coef_P_D_given_XZ = 1 and kDID can be
# recovered analytically.

make_test_data <- function(seed = 42, n = 2000) {
  set.seed(seed)
  U <- stats::rnorm(n)
  X <- stats::rnorm(n)
  D <- X + U + stats::rnorm(n)
  P <- X + U + stats::rnorm(n)
  Y <- D + X + U + stats::rnorm(n)
  N <- D + X + U + stats::rnorm(n)
  data.frame(Y = Y, D = D, P = P, N = N, X = X, U = U)
}

dat <- make_test_data()


# ---- placeboLM() input validation -------------------------------------------

test_that("placeboLM() rejects non-data-frame data", {
  expect_error(
    placeboLM(data = "lalonde", outcome = "Y", treatment = "D",
              placebo_outcome = "P"),
    "`data` must be a data frame"
  )
})

test_that("placeboLM() rejects missing variables", {
  expect_error(
    placeboLM(dat, outcome = "Z_missing", treatment = "D",
              placebo_outcome = "P"),
    "not found in `data`"
  )
  expect_error(
    placeboLM(dat, outcome = "Y", treatment = "D_missing",
              placebo_outcome = "P"),
    "not found in `data`"
  )
  expect_error(
    placeboLM(dat, outcome = "Y", treatment = "D",
              placebo_outcome = "NOTHERE"),
    "not found in `data`"
  )
})

test_that("placeboLM() requires exactly one placebo", {
  expect_error(
    placeboLM(dat, outcome = "Y", treatment = "D"),
    "Provide exactly one"
  )
  expect_error(
    placeboLM(dat, outcome = "Y", treatment = "D",
              placebo_outcome = "N", placebo_treatment = "P"),
    "Provide exactly one"
  )
})

test_that("placeboLM() rejects cyclic PY/DP combination", {
  expect_error(
    placeboLM(dat, outcome = "Y", treatment = "D",
              placebo_treatment = "P", DP = "<-", PY = "<-"),
    "cycle"
  )
})

test_that("placeboLM() rejects invalid DP/PY values via match.arg", {
  expect_error(
    placeboLM(dat, outcome = "Y", treatment = "D",
              placebo_outcome = "N", PY = "bad"),
    "should be one of"
  )
})

test_that("placeboLM() rejects missing covariate names", {
  expect_error(
    placeboLM(dat, outcome = "Y", treatment = "D",
              placebo_outcome = "N", observed_covariates = c("X", "NOPE")),
    "not found in `data`"
  )
})


# ---- placeboLM() type detection ---------------------------------------------

test_that("placeboLM() returns correct type: No Direct Relationships, Placebo Outcome", {
  plm <- suppressMessages(placeboLM(dat, "Y", "D", placebo_outcome = "N",
                                     observed_covariates = "X",
                                     partialIDparam_minmax = list(k = c(-2, 2),
                                                                   coef_P_D_given_XZ = c(-2, 2))))
  expect_equal(plm$type,
               "Single Placebo, No Direct Relationships, Placebo Outcome")
  expect_named(plm$regressions, c("reg_Y_on_D", "reg_P_on_D"))
  expect_s3_class(plm$regressions$reg_Y_on_D, "formula")
})

test_that("placeboLM() returns correct type: No Direct Relationships, Placebo Treatment", {
  plm <- suppressMessages(placeboLM(dat, "Y", "D", placebo_treatment = "P",
                                     observed_covariates = "X",
                                     partialIDparam_minmax = list(k = c(-2, 2),
                                                                   coef_Y_P_given_DXZ = c(-1, 1))))
  expect_equal(plm$type,
               "Single Placebo, No Direct Relationships, Placebo Treatment")
  expect_named(plm$regressions, "reg_Y_on_D_plus_P")
})

test_that("placeboLM() returns correct type: Treatment causes Placebo", {
  plm <- suppressMessages(placeboLM(dat, "Y", "D", placebo_outcome = "N",
                                     DP = "->",
                                     observed_covariates = "X",
                                     partialIDparam_minmax = list(k = c(-2, 2),
                                                                   coef_P_D_given_XZ = c(-2, 2))))
  expect_equal(plm$type, "Single Placebo, Treatment causes Placebo")
})

test_that("placeboLM() returns correct type: Placebo causes Outcome, Placebo Treatment", {
  plm <- suppressMessages(placeboLM(dat, "Y", "D", placebo_treatment = "P",
                                     PY = "->",
                                     observed_covariates = "X",
                                     partialIDparam_minmax = list(k = c(-2, 2),
                                                                   coef_Y_P_given_DXZ = c(-1, 1))))
  expect_equal(plm$type,
               "Single Placebo, Placebo causes Outcome, Placebo Treatment")
})

test_that("placeboLM() returns correct type: Placebo causes Outcome, Placebo Outcome", {
  plm <- suppressMessages(placeboLM(dat, "Y", "D", placebo_outcome = "N",
                                     PY = "->",
                                     observed_covariates = "X",
                                     partialIDparam_minmax = list(k = c(-2, 2),
                                                                   coef_P_D_given_XZ = c(-2, 2))))
  expect_equal(plm$type,
               "Single Placebo, Placebo causes Outcome, Placebo Outcome")
  expect_named(plm$regressions, c("reg_Y_on_D_plus_P", "reg_P_on_D"))
})

test_that("placeboLM() returns correct type: Mediator, Placebo Outcome", {
  plm <- suppressMessages(placeboLM(dat, "Y", "D", placebo_outcome = "N",
                                     DP = "->", PY = "->",
                                     observed_covariates = "X",
                                     partialIDparam_minmax = list(k = c(-2, 2),
                                                                   coef_P_D_given_XZ = c(-2, 2))))
  expect_equal(plm$type,
               "Single Placebo, Placebo is Mediator, Placebo Outcome")
})

test_that("placeboLM() returns correct type: Mediator, Placebo Treatment", {
  plm <- suppressMessages(placeboLM(dat, "Y", "D", placebo_treatment = "P",
                                     DP = "->", PY = "->",
                                     observed_covariates = "X",
                                     partialIDparam_minmax = list(k = c(-2, 2),
                                                                   coef_Y_P_given_DXZ = c(-1, 1))))
  expect_equal(plm$type,
               "Single Placebo, Placebo is Mediator, Placebo Treatment")
})

test_that("placeboLM() returns correct type: Observed Confounder", {
  plm <- suppressMessages(placeboLM(dat, "Y", "D", placebo_treatment = "P",
                                     DP = "<-",
                                     observed_covariates = "X",
                                     partialIDparam_minmax = list(k = c(-2, 2),
                                                                   coef_D_P_given_XZ = c(-1, 1))))
  expect_equal(plm$type,
               "Single Placebo, Placebo is Observed Confounder")
  expect_named(plm$regressions, c("reg_Y_on_D_plus_P", "reg_D_on_P"))
})

test_that("placeboLM() returns correct type: Outcome causes Placebo", {
  plm <- suppressMessages(placeboLM(dat, "Y", "D", placebo_outcome = "N",
                                     PY = "<-",
                                     observed_covariates = "X",
                                     partialIDparam_minmax = list(k = c(-2, 2),
                                                                   coef_P_Y_given_DXZ = c(-1, 1))))
  expect_equal(plm$type, "Single Placebo, Outcome causes Placebo")
  expect_named(plm$regressions, c("reg_Y_on_D", "reg_P_on_Y_plus_D"))
})


# ---- estimate_regs() --------------------------------------------------------

test_that("estimate_regs() returns betas/ses/df for each regression", {
  plm <- suppressMessages(placeboLM(dat, "Y", "D", placebo_outcome = "N",
                                     observed_covariates = "X",
                                     partialIDparam_minmax = list(k = c(-2, 2),
                                                                   coef_P_D_given_XZ = c(-2, 2))))
  regs <- estimate_regs(plm)
  expect_named(regs, c("reg_Y_on_D", "reg_P_on_D"))
  expect_named(regs$reg_Y_on_D, c("betas", "ses", "df"))
  expect_true(is.numeric(regs$reg_Y_on_D$betas))
  expect_true(is.numeric(regs$reg_Y_on_D$ses))
  expect_true(is.numeric(regs$reg_Y_on_D$df))
})

test_that("estimate_regs() accepts an external data frame", {
  plm <- suppressMessages(placeboLM(dat, "Y", "D", placebo_outcome = "N",
                                     observed_covariates = "X",
                                     partialIDparam_minmax = list(k = c(-2, 2),
                                                                   coef_P_D_given_XZ = c(-2, 2))))
  sub_dat <- dat[1:500, ]
  regs_sub  <- estimate_regs(plm, data = sub_dat)
  regs_full <- estimate_regs(plm)
  # Estimates differ because sample sizes differ
  expect_false(isTRUE(all.equal(regs_sub$reg_Y_on_D$betas,
                                 regs_full$reg_Y_on_D$betas)))
})


# ---- estimate_PLM() ---------------------------------------------------------

test_that("estimate_PLM() returns 'estimate' at SOO (k=0) close to confounded value", {
  # With k=0, the PLM estimate equals the OLS estimate (no adjustment).
  plm <- suppressMessages(placeboLM(dat, "Y", "D", placebo_outcome = "N",
                                     observed_covariates = "X",
                                     partialIDparam_minmax = list(k = c(-2, 2),
                                                                   coef_P_D_given_XZ = c(-2, 2))))
  regs     <- estimate_regs(plm)
  zero_param <- list(k = 0, coef_P_D_given_XZ = 0)
  est      <- estimate_PLM(plm, zero_param, regs, "estimate")
  ols      <- regs$reg_Y_on_D$betas[["D"]]
  expect_equal(est, ols)
})

test_that("estimate_PLM() match.arg catches bad 'returned' values", {
  plm <- suppressMessages(placeboLM(dat, "Y", "D", placebo_outcome = "N",
                                     observed_covariates = "X",
                                     partialIDparam_minmax = list(k = c(-2, 2),
                                                                   coef_P_D_given_XZ = c(-2, 2))))
  regs <- estimate_regs(plm)
  expect_error(
    estimate_PLM(plm, list(k = 0, coef_P_D_given_XZ = 0), regs, "bad"),
    "should be one of"
  )
})

test_that("estimate_PLM() oracle recovery: No Direct Relationships, Placebo Outcome", {
  # When coef_P_D_given_XZ is set to its oracle value (the D coefficient in the
  # regression of P on D, X, U), the PLM estimate with k=1 should be close to
  # the true effect of 1.
  plm  <- suppressMessages(placeboLM(dat, "Y", "D", placebo_outcome = "N",
                                      observed_covariates = "X",
                                      partialIDparam_minmax = list(k = c(-2, 2),
                                                                    coef_P_D_given_XZ = c(-2, 2))))
  regs <- estimate_regs(plm)
  # Oracle: fit P ~ D + X + U to get the "true" residual placebo association
  oracle_reg  <- lm(N ~ D + X + U, data = dat)
  oracle_coef <- coef(oracle_reg)[["D"]]
  est <- estimate_PLM(plm, list(k = 1, coef_P_D_given_XZ = oracle_coef), regs, "estimate")
  expect_equal(est, 1, tolerance = 0.1)
})

test_that("estimate_PLM() oracle recovery: Outcome causes Placebo", {
  plm  <- suppressMessages(placeboLM(dat, "Y", "D", placebo_outcome = "N",
                                      PY = "<-",
                                      observed_covariates = "X",
                                      partialIDparam_minmax = list(k = c(-2, 2),
                                                                    coef_P_Y_given_DXZ = c(-2, 2))))
  regs <- estimate_regs(plm)
  oracle_reg  <- lm(N ~ Y + D + X + U, data = dat)
  oracle_coef <- coef(oracle_reg)[["Y"]]
  est <- estimate_PLM(plm, list(k = 1, coef_P_Y_given_DXZ = oracle_coef), regs, "estimate")
  expect_equal(est, 1, tolerance = 0.1)
})


# ---- beta_expression_convert() ----------------------------------------------

test_that("beta_expression_convert() parses standard single-character names", {
  expr <- beta_expression_convert("coef_P_D_given_XZ")
  expect_type(expr, "language")
})

test_that("beta_expression_convert() parses multi-character variable names", {
  # This was broken in the original implementation.
  expr <- beta_expression_convert("coef_re74_treat_given_XZ")
  expect_type(expr, "language")
})

test_that("beta_expression_convert() returns input unchanged for non-matching strings", {
  expect_equal(beta_expression_convert("k"), "k")
  expect_equal(beta_expression_convert("lambda"), "lambda")
})


# ---- placeboLM_point_estimate() ---------------------------------------------

test_that("placeboLM_point_estimate() returns correct matrix columns without bootstrap", {
  plm <- suppressMessages(placeboLM(dat, "Y", "D", placebo_outcome = "N",
                                     observed_covariates = "X",
                                     partialIDparam_minmax = list(k = c(-2, 2),
                                                                   coef_P_D_given_XZ = c(-2, 2))))
  res <- placeboLM_point_estimate(plm, list(k = 0, coef_P_D_given_XZ = 0),
                                   bootstrap = FALSE)
  expect_equal(colnames(res), "Estimate")
  expect_equal(nrow(res), 1L)
})

test_that("placeboLM_point_estimate() returns four columns with bootstrap", {
  plm <- suppressMessages(placeboLM(dat, "Y", "D", placebo_outcome = "N",
                                     observed_covariates = "X",
                                     partialIDparam_minmax = list(k = c(-2, 2),
                                                                   coef_P_D_given_XZ = c(-2, 2))))
  set.seed(1)
  res <- placeboLM_point_estimate(plm, list(k = 0, coef_P_D_given_XZ = 0),
                                   bootstrap = TRUE, n_boot = 50)
  expect_equal(colnames(res), c("Estimate", "Std. Error", "CI Low", "CI High"))
  expect_true(res[1, "CI Low"] < res[1, "Estimate"])
  expect_true(res[1, "Estimate"] < res[1, "CI High"])
})

test_that("placeboLM_point_estimate() errors when n_boot missing with bootstrap=TRUE", {
  plm <- suppressMessages(placeboLM(dat, "Y", "D", placebo_outcome = "N",
                                     observed_covariates = "X",
                                     partialIDparam_minmax = list(k = c(-2, 2),
                                                                   coef_P_D_given_XZ = c(-2, 2))))
  expect_error(
    placeboLM_point_estimate(plm, list(k = 0, coef_P_D_given_XZ = 0),
                              bootstrap = TRUE),
    "`n_boot` must be supplied"
  )
})


# ---- placeboLM_contour_plot() -----------------------------------------------

test_that("placeboLM_contour_plot() warns and returns NULL for != 2 params", {
  plm1 <- suppressMessages(placeboLM(dat, "Y", "D", placebo_outcome = "N",
                                      observed_covariates = "X",
                                      partialIDparam_minmax = list(k = c(-2, 2))))
  expect_warning(res <- placeboLM_contour_plot(plm1), "exactly 2")
  expect_null(res)

  plm3 <- suppressMessages(placeboLM(dat, "Y", "D", placebo_outcome = "N",
                                      observed_covariates = "X",
                                      partialIDparam_minmax = list(
                                        k = c(-2, 2),
                                        coef_P_D_given_XZ = c(-1, 1),
                                        extra = c(0, 1))))
  expect_warning(placeboLM_contour_plot(plm3), "exactly 2")
})
