# Numerical equivalence with the pre-rewrite implementation.
#
# The rewrite changed the interface, not the arithmetic. The paper's published
# results were produced with the previous implementation, so any divergence
# here is a regression regardless of how much cleaner the new code reads.
#
# The reference implementations below are transcribed verbatim from the
# arithmetic of the previous `estimate_PLM()`, so this test is self-contained
# and does not depend on git history.

legacy_estimate <- function(data, structure, k, imperfection, cov = "X") {
  rhs <- function(...) paste(c(..., cov), collapse = " + ")
  fitq <- function(f) {
    m <- stats::lm(stats::as.formula(f), data = data)
    ct <- stats::coef(summary(m))
    list(b = ct[, "Estimate"], se = ct[, "Std. Error"], df = m$df.residual)
  }

  switch(structure,
    placebo_outcome = {
      a <- fitq(paste("Y ~", rhs("D"))); b <- fitq(paste("P ~", rhs("D")))
      SF <- (a$se[["D"]] * sqrt(a$df)) / (b$se[["D"]] * sqrt(b$df))
      a$b[["D"]] - k * (b$b[["D"]] - imperfection) * SF
    },
    placebo_treatment = {
      a <- fitq(paste("Y ~", rhs("D", "P")))
      SF <- (a$se[["D"]] * sqrt(a$df)) / (a$se[["P"]] * sqrt(a$df))
      a$b[["D"]] - k * (a$b[["P"]] - imperfection) * SF
    },
    observed_confounder_1 = {
      a <- fitq(paste("Y ~", rhs("D", "P"))); b <- fitq(paste("P ~", rhs("D")))
      SF <- (a$se[["D"]] * sqrt(a$df)) / (b$se[["D"]] * sqrt(b$df))
      a$b[["D"]] - k * (b$b[["D"]] - imperfection) * SF
    },
    observed_confounder_2 = {
      a <- fitq(paste("Y ~", rhs("D", "P"))); b <- fitq(paste("D ~", rhs("P")))
      SF <- (a$se[["D"]] * sqrt(a$df)) / (b$se[["P"]] * sqrt(b$df))
      a$b[["D"]] - k * (b$b[["P"]] - imperfection) * SF
    },
    post_outcome = {
      a <- fitq(paste("Y ~", rhs("D"))); b <- fitq(paste("P ~", rhs("Y", "D")))
      SF <- (a$se[["D"]] * sqrt(a$df)) / (b$se[["Y"]] * sqrt(b$df))
      a$b[["D"]] - k * (b$b[["Y"]] - imperfection) * SF
    }
  )
}

test_that("estimates match the previous implementation exactly", {
  d <- plm_test_data(n = 1500, seed = 99)
  grid <- expand.grid(k = c(-1.5, 0, 0.37, 1, 2.4),
                      imperfection = c(-0.6, 0, 0.25))

  for (s in names(plm_structures)) {
    fit <- placebo_lm(d, "Y", "D", "P", covariates = "X", structure = s)
    for (i in seq_len(nrow(grid))) {
      expect_equal(
        plm_estimate(fit, k = grid$k[i], imperfection = grid$imperfection[i]),
        legacy_estimate(d, s, grid$k[i], grid$imperfection[i]),
        info = paste(s, "k =", grid$k[i], "imp =", grid$imperfection[i])
      )
    }
  }
})

test_that("the legacy DID benchmark k = 1/SF is preserved", {
  d <- plm_test_data(n = 800)
  fit <- placebo_lm(d, "Y", "D", "P", covariates = "X",
                    structure = "placebo_outcome")
  # The previous implementation derived its DID row as k = 1 / scale_factor.
  bm <- plm_benchmarks(fit)
  expect_equal(bm$k[bm$benchmark == "DID (m = 1)"], 1 / fit$SF)
})
