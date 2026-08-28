# Matrix bootstrap engine ------------------------------------------------------
#
# The default "lm" engine re-runs stats::lm() on a freshly subset data frame for
# every bootstrap replicate, then calls summary.lm() purely to recover a
# standard error. Measured at n = 2000 with 6 covariates, that is 3.55 ms per
# replicate, of which roughly 55% is re-parsing the formula and rebuilding the
# model matrix -- work that is identical across replicates.
#
# This engine does that work once:
#
#   * build each regression's model matrix and response vector a single time;
#   * per replicate, index rows and take a QR directly;
#   * recover the standard error from sigma * sqrt(diag((X'X)^-1)) rather than
#     by constructing a summary.lm object.
#
# And one further saving: where two regressions share a right-hand side -- the
# placebo_outcome case, which covers both of the paper's applications and the
# difference-in-differences connection -- the design matrix is literally the
# same, so a single QR decomposition serves both responses.
#
# Measured end-to-end speedup (single core, 400 replicates):
#
#   n =  2000, placebo_outcome (shared design)   1.77s -> 0.44s   4.0x
#   n = 20000, placebo_outcome (shared design)   5.06s -> 2.33s   2.2x
#   n =  2000, post_outcome    (no sharing)      1.21s -> 0.42s   2.9x
#
# The gain shrinks as n grows: the fixed per-replicate overhead this engine
# removes is constant, while the QR it still has to do grows with n. An earlier
# projection of ~7x came from summing component microbenchmarks and was too
# optimistic about the end-to-end result.
#
# This engine is opt-in (`engine = "matrix"`). The "lm" path is the one whose
# numbers back the published results, so it stays the default until the fast
# path has accrued mileage. test-engine.R requires agreement to 1e-10 across
# every structure and a set of awkward cases before this is usable at all.


# Group the fit's regressions by identical design matrix, so a shared QR can
# serve several responses. Returns a list of groups, each with the design `X`
# and the responses `ys` (named by regression).
.plm_matrix_prep <- function(fit) {
  parts <- lapply(fit$formulas, function(f) {
    mf <- stats::model.frame(f, fit$data)
    list(X = stats::model.matrix(f, mf),
         y = stats::model.response(mf))
  })

  # lm() rebuilds contrasts from the subset data, and errors outright if a
  # factor has fewer than two levels present ("contrasts can be applied only to
  # factors with 2 or more levels"). The matrix engine works from a design built
  # once on the full data, so it would instead see an all-zero column, detect
  # rank deficiency, and happily return a coefficient.
  #
  # That difference is arguably in the matrix engine's favour -- the treatment
  # coefficient is perfectly well identified, only the absent contrast is not,
  # and dropping such replicates conditions the bootstrap on resamples that
  # happen to contain the rare level. But the contract for an opt-in fast path
  # is identical answers, so the check is reproduced here rather than quietly
  # improved. Changing which replicates are usable is a statistical decision and
  # should be made deliberately, not smuggled in via an engine switch.
  fac <- Filter(is.factor, do.call(cbind.data.frame, lapply(
    fit$formulas, function(f) stats::model.frame(f, fit$data))))

  groups <- list()
  for (nm in names(parts)) {
    p <- parts[[nm]]
    placed <- FALSE
    for (g in seq_along(groups)) {
      if (identical(groups[[g]]$X, p$X)) {
        groups[[g]]$ys[[nm]] <- p$y
        placed <- TRUE
        break
      }
    }
    if (!placed)
      groups[[length(groups) + 1L]] <- list(X = p$X, ys = stats::setNames(list(p$y), nm))
  }
  attr(groups, "factors") <- fac
  groups
}


# Solve one design against one or more responses on a given row subset, and
# return coefficient and standard error for each response.
#
# Mirrors what summary.lm does: the QR is pivoted, so the inverse cross-product
# is formed from the leading `rank` columns of R and mapped back through the
# pivot. Aliased columns are reported as NA, matching lm()'s behaviour of
# dropping them from the coefficient table.
.plm_solve_group <- function(X, ys, idx) {
  Xs <- X[idx, , drop = FALSE]
  n  <- nrow(Xs)
  qrx <- qr(Xs)
  r   <- qrx$rank
  if (r < 1L) return(NULL)

  piv  <- qrx$pivot[seq_len(r)]
  df   <- n - r
  if (df < 1L) return(NULL)

  # (X'X)^-1 restricted to the columns actually used, in pivot order.
  R    <- chol2inv(qrx$qr[seq_len(r), seq_len(r), drop = FALSE])
  dR   <- diag(R)
  nms  <- colnames(X)

  lapply(ys, function(y) {
    ysub <- y[idx]
    cf   <- qr.coef(qrx, ysub)              # NA for aliased columns
    res  <- qr.resid(qrx, ysub)
    resvar <- sum(res^2) / df
    se <- rep(NA_real_, length(nms))
    se[piv] <- sqrt(dR * resvar)
    names(se) <- nms
    list(coef = cf, se = se, df = df)
  })
}


# One bootstrap replicate under the matrix engine. Returns the same shape as
# .plm_refit() so that everything downstream is engine-agnostic, or NULL if the
# replicate is degenerate (rank deficient in a way that drops a coefficient the
# estimator needs).
.plm_refit_matrix <- function(fit, prep, idx) {
  # Match lm()'s refusal to fit when a factor loses all but one level; see the
  # note in .plm_matrix_prep().
  fac <- attr(prep, "factors")
  if (!is.null(fac) && ncol(fac) > 0L) {
    for (j in seq_len(ncol(fac))) {
      if (nlevels(droplevels(fac[[j]][idx])) < 2L) return(NULL)
    }
  }

  solved <- list()
  for (g in prep) {
    out <- .plm_solve_group(g$X, g$ys, idx)
    if (is.null(out)) return(NULL)
    solved[names(out)] <- out
  }

  grab <- function(loc) {
    s <- solved[[loc$reg]]
    if (is.null(s)) return(NULL)
    est <- s$coef[[loc$coef]]
    se  <- s$se[[loc$coef]]
    if (is.null(est) || is.null(se) || !is.finite(est) || !is.finite(se))
      return(NULL)
    list(estimate = unname(est), se = unname(se), df = s$df, name = loc$coef)
  }

  target <- grab(fit$spec$target_coef(fit$vars))
  sens   <- grab(fit$spec$sens_coef(fit$vars))
  if (is.null(target) || is.null(sens)) return(NULL)

  sf <- (target$se * sqrt(target$df)) / (sens$se * sqrt(sens$df))
  if (!is.finite(sf)) return(NULL)

  list(target = target, sens = sens, SF = sf)
}
