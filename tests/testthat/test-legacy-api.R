# The paper's Appendix G ("Example R Code") tells readers to install this
# package from GitHub and run code written against the pre-0.2.0 interface.
# These tests run that published code, so that a future change cannot silently
# break the replication path for the published results.

test_that("the paper's Appendix G.2 simulation code runs and reproduces its figures", {
  set.seed(1)
  n <- 5000
  ses  <- runif(n, 0, 1)
  phy  <- rbinom(size = 1, n = n, prob = pnorm(2 * ses))
  diet <- rbinom(size = 1, n = n, prob = pnorm(-1 + ses))
  HD   <- rbinom(size = 1, n = n, prob = pnorm(-2 + -0.4 * phy + 4 * ses))
  health_data <<- data.frame(SES = ses, Physical_Activity = phy,
                             Dietary_Supplements = diet, Heart_Disease = HD)
  on.exit(rm(health_data, envir = globalenv()), add = TRUE)

  out <- suppressWarnings(suppressMessages(placeboLM(
    data = "health_data", outcome = "Heart_Disease",
    treatment = "Physical_Activity", placebo_treatment = "Dietary_Supplements",
    PY = "->",
    partialIDparam_minmax = list(k = c(-0.1, 3),
                                 coef_Y_P_given_DXZ = c(-0.5, 0.5)))))

  regs <- estimate_regs(out)
  SF <- estimate_PLM(plm = out,
                     partialIDparam = list(k = 1, coef_Y_P_given_DXZ = 0),
                     estimated_regs = regs, returned = "SF")

  # Paper reports SF = 1.17 and a no-unobserved-confounding estimate of +17pp.
  # The paper reports SF = 1.17 and +17pp, both rounded to two figures.
  expect_equal(unname(SF), 1.17, tolerance = 0.01)
  expect_equal(unname(regs$reg_Y_on_D_plus_P$betas[["Physical_Activity"]]),
               0.17, tolerance = 0.05)
})

test_that("legacy and current interfaces agree numerically", {
  d <- plm_test_data(n = 800)
  legacy_dat <<- d
  on.exit(rm(legacy_dat, envir = globalenv()), add = TRUE)

  old <- suppressWarnings(suppressMessages(placeboLM(
    data = "legacy_dat", outcome = "Y", treatment = "D",
    placebo_outcome = "P", observed_covariates = "X",
    partialIDparam_minmax = list(k = c(-2, 2),
                                 coef_P_D_given_XZ = c(-2, 2)))))
  oreg <- estimate_regs(old)
  new <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")

  for (k in c(-1, 0, 0.5, 1.3)) {
    for (imp in c(-0.4, 0, 0.2)) {
      expect_equal(
        unname(estimate_PLM(old, list(k = k, coef_P_D_given_XZ = imp),
                            oreg, "estimate")),
        plm_estimate(new, k = k, imperfection = imp)
      )
    }
  }
})

test_that("legacy entry points announce that they are deprecated", {
  d <- plm_test_data(n = 200)
  legacy_dat2 <<- d
  on.exit(rm(legacy_dat2, envir = globalenv()), add = TRUE)
  PlaceboLM:::.plm_deprecate_reset()
  expect_message(
    placeboLM(data = "legacy_dat2", outcome = "Y",
              treatment = "D", placebo_outcome = "P"),
    "deprecated"
  )
})

test_that("the paper's published code survives options(warn = 2)", {
  # The reason the deprecation notice is a message rather than a warning. Under
  # warn = 2 a warning becomes an error, and the code printed in the paper's
  # appendix would fail on its first call.
  d <- plm_test_data(n = 200)
  legacy_warn2 <<- d
  on.exit({ rm(legacy_warn2, envir = globalenv()); options(warn = 0) }, add = TRUE)

  # Note the legacy code prints via message(cat(...)); cat() writes straight to
  # stdout, so expect_silent() is not the right assertion. What matters is that
  # nothing is promoted to an error.
  PlaceboLM:::.plm_deprecate_reset()
  options(warn = 2)
  out <- NULL
  expect_error(
    invisible(utils::capture.output(suppressMessages(
      out <- placeboLM(data = "legacy_warn2", outcome = "Y", treatment = "D",
                       placebo_outcome = "P")))),
    NA)
  expect_equal(out$type,
               "Single Placebo, No Direct Relationships, Placebo Outcome")
})


test_that("double placebos are refused by the legacy interface too", {
  d <- plm_test_data(n = 200)
  d$N <- d$Y + stats::rnorm(nrow(d))
  legacy_dat3 <<- d
  on.exit(rm(legacy_dat3, envir = globalenv()), add = TRUE)
  expect_error(
    suppressWarnings(suppressMessages(
      placeboLM(data = "legacy_dat3", outcome = "Y", treatment = "D",
                placebo_outcome = "N", placebo_treatment = "P"))),
    "Double placebos are not supported"
  )
})
