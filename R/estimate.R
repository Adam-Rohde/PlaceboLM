# Estimation -------------------------------------------------------------------
#
# Every structure in the registry shares one estimator:
#
#   adjusted = beta_target - k * (beta_sens - imperfection) * SF
#
# which is the "Target Coefficient Expression" column of Tables 1 and 2 of the
# paper. Because the structure-specific detail is confined to the registry, the
# arithmetic below is written once.


#' Partially identified estimate at given sensitivity parameters
#'
#' @description
#' Evaluates the adjusted causal estimate at postulated values of the
#' relative-confounding parameter (`k` or `m`) and the placebo imperfection.
#'
#' Setting `k = 0` gives the selection-on-observables estimate (no unobserved
#' confounding). Setting `m = 1` with `imperfection = 0` gives the conventional
#' difference-in-differences estimate. Setting `k = 1` gives equiconfounding
#' after rescaling.
#'
#' Arguments are vectorised and recycled, so a whole grid can be evaluated in
#' one call.
#'
#' @param fit A `placebo_lm` object from [placebo_lm()].
#' @param k Numeric. Scale-free relative confounding. Supply exactly one of
#'   `k` or `m`.
#' @param m Numeric. Raw ratio of biases. Conventional difference-in-differences
#'   is `m = 1`.
#' @param imperfection Numeric. The postulated value of the structure's
#'   sensitivity parameter (see `fit$spec$sens_param`); zero means a perfect
#'   placebo. Defaults to `0`.
#'
#' @return A numeric vector of adjusted estimates.
#'
#' @examples
#' set.seed(1)
#' n <- 500
#' U <- rnorm(n); D <- U + rnorm(n)
#' P <- U + rnorm(n); Y <- D + U + rnorm(n)
#' fit <- placebo_lm(data.frame(Y = Y, D = D, P = P),
#'                   "Y", "D", "P", structure = "placebo_outcome")
#'
#' plm_estimate(fit, k = 0)   # no unobserved confounding
#' plm_estimate(fit, m = 1)   # conventional difference-in-differences
#' plm_estimate(fit, k = 1)   # equiconfounding after rescaling
#' plm_estimate(fit, k = seq(0, 2, by = 0.5))
#'
#' @export
plm_estimate <- function(fit, k = NULL, m = NULL, imperfection = 0) {
  .plm_check_fit(fit)
  k <- .plm_resolve_k(fit, k, m)
  imperfection <- .plm_check_imperfection(fit, imperfection)

  fit$coefs$target$estimate -
    k * (fit$coefs$sens$estimate - imperfection) * fit$SF
}


# Same arithmetic against a refit (bootstrap replicate) rather than the
# original object.
.plm_estimate_from <- function(parts, k, imperfection) {
  parts$target$estimate - k * (parts$sens$estimate - imperfection) * parts$SF
}


#' Benchmark estimates
#'
#' @description
#' Returns the three reference points the paper reports alongside every
#' analysis, as data rather than as plot annotations:
#'
#' \describe{
#'   \item{No unobserved confounding}{`k = 0`; the ordinary least-squares
#'     estimate.}
#'   \item{DID}{`m = 1`; conventional difference-in-differences, i.e. parallel
#'     trends on the observed scale.}
#'   \item{Equiconfounding}{`k = 1`; equal confounding after rescaling.}
#' }
#'
#' All three assume a perfect placebo unless `imperfection` is given.
#'
#' @param fit A `placebo_lm` object.
#' @param imperfection Numeric scalar. Postulated placebo imperfection.
#'   Defaults to `0`.
#'
#' @return A data frame with columns `benchmark`, `k`, `m`, `imperfection`,
#'   and `estimate`.
#'
#' @examples
#' set.seed(1)
#' dat <- data.frame(Y = rnorm(300), D = rnorm(300), P = rnorm(300))
#' fit <- placebo_lm(dat, "Y", "D", "P", structure = "placebo_outcome")
#' plm_benchmarks(fit)
#'
#' @export
plm_benchmarks <- function(fit, imperfection = 0) {
  .plm_check_fit(fit)
  imperfection <- .plm_check_imperfection(fit, imperfection)

  # m = 1 is equiconfounding on the raw scale. It additionally has a
  # difference-in-differences reading only where the placebo is a pre-treatment
  # measure of the outcome, which is the `did_equivalent` flag in the registry.
  # Labelling every structure "DID" would assert a reading that does not exist
  # for, say, a placebo treatment or a post-outcome placebo.
  m1_label <- if (isTRUE(fit$spec$did_equivalent))
    "DID (m = 1)" else "Equiconfounding, raw scale (m = 1)"

  k_vals <- c(0, 1 / fit$SF, 1)
  names(k_vals) <- c("No unobserved confounding", m1_label,
                     "Equiconfounding, rescaled (k = 1)")

  data.frame(
    benchmark    = names(k_vals),
    k            = unname(k_vals),
    m            = unname(k_vals) * fit$SF,
    imperfection = imperfection,
    adjusted_coefficient = plm_estimate(fit, k = unname(k_vals),
                                        imperfection = imperfection),
    row.names    = NULL,
    stringsAsFactors = FALSE
  )
}


#' Bounds on the effect over a range of assumptions
#'
#' @description
#' The headline output of the method. Given a *range* of relative confounding
#' (and optionally a range of placebo imperfection), returns the interval of
#' adjusted estimates consistent with those assumptions -- the package
#' equivalent of the paper's "assuming only that 0.5 < k < 1 places the results
#' between -1,249 USD and +3,428 USD."
#'
#' The estimator is linear in `k` and in `imperfection`, so the extremes over a
#' rectangular assumption region are always attained at its corners; the bound
#' is computed exactly rather than by searching a grid.
#'
#' When `n_boot > 0`, the bound is recomputed on each bootstrap resample and the
#' reported interval widens to reflect sampling uncertainty *in the bound
#' itself*: `ci_lower` is the lower quantile of the bootstrapped lower bounds
#' and `ci_upper` the upper quantile of the bootstrapped upper bounds.
#'
#' @param fit A `placebo_lm` object.
#' @param k Numeric vector of length 2, `c(min, max)`. Supply exactly one of
#'   `k` or `m`.
#' @param m Numeric vector of length 2, `c(min, max)`.
#' @param imperfection Numeric vector of length 1 or 2. A single value fixes the
#'   placebo imperfection; two values give a range. Defaults to `0` (perfect
#'   placebo).
#' @param n_boot Non-negative integer. Bootstrap replicates. `0` skips
#'   uncertainty quantification and returns the point bound only.
#' @param alpha Numeric. Significance level for the bootstrap interval.
#'   Defaults to `0.05`.
#' @param ci_type `"percentile"` (default) or `"normal"`. The paper notes both
#'   the nonparametric bootstrap and the normal approximation to bootstrap
#'   standard errors are available.
#' @param cores Integer. Cores for the bootstrap. Defaults to one less than
#'   detected, minimum 1.
#'
#' @section What the bootstrap columns are, and are not:
#' Four distinct objects are easy to confuse here.
#' \enumerate{
#'   \item A confidence interval for the estimate at one fixed choice of the
#'     sensitivity parameters -- that is [plm_grid()] or [plm_analytic()].
#'   \item The point bounds: the range of estimates over the assumption region,
#'     holding the data fixed. These are `lower` and `upper`.
#'   \item Bootstrap uncertainty about each bound separately. These are
#'     `lower_boot_q` and `upper_boot_q`: the lower quantile of the bootstrapped
#'     lower bounds, and the upper quantile of the bootstrapped upper bounds.
#'   \item A confidence region with a coverage guarantee for the whole
#'     identified set, in the sense of Imbens and Manski (2004).
#' }
#' This package provides the first three. It does **not** provide the fourth,
#' and `lower_boot_q`/`upper_boot_q` should not be reported as though it did.
#' They are a descriptive summary of sampling variability in the bounds, not a
#' procedure with an established coverage property.
#'
#' @return A one-row data frame with columns `k_low`, `k_high`, `m_low`,
#'   `m_high`, `imperfection_low`, `imperfection_high`, `lower`, `upper`, and
#'   -- when `n_boot > 0` -- `lower_boot_q` and `upper_boot_q`.
#'
#' @examples
#' set.seed(1)
#' n <- 400
#' U <- rnorm(n); D <- U + rnorm(n)
#' P <- U + rnorm(n); Y <- D + U + rnorm(n)
#' fit <- placebo_lm(data.frame(Y = Y, D = D, P = P),
#'                   "Y", "D", "P", structure = "placebo_outcome")
#'
#' plm_bounds(fit, k = c(0.5, 1), n_boot = 0)
#'
#' @export
plm_bounds <- function(fit, k = NULL, m = NULL, imperfection = 0,
                       n_boot = 1000, alpha = 0.05,
                       ci_type = c("percentile", "normal"),
                       cores = NULL) {
  .plm_check_fit(fit)
  ci_type <- match.arg(ci_type)

  supplied <- if (is.null(k)) m else k
  if (length(supplied) != 2L)
    stop("`", if (is.null(k)) "m" else "k",
         "` must be a length-2 vector giving the assumed range, c(min, max).",
         call. = FALSE)

  k_rng <- sort(.plm_resolve_k(fit, k, m))
  imperfection <- .plm_check_imperfection(fit, imperfection)
  if (length(imperfection) == 1L) imperfection <- rep(imperfection, 2L)
  if (length(imperfection) != 2L)
    stop("`imperfection` must have length 1 (a fixed value) or 2 (a range).",
         call. = FALSE)
  imp_rng <- sort(imperfection)

  corners <- expand.grid(k = k_rng, imperfection = imp_rng)

  point <- range(plm_estimate(fit, k = corners$k,
                              imperfection = corners$imperfection))

  out <- data.frame(
    k_low = k_rng[1], k_high = k_rng[2],
    m_low = k_rng[1] * fit$SF, m_high = k_rng[2] * fit$SF,
    imperfection_low = imp_rng[1], imperfection_high = imp_rng[2],
    lower = point[1], upper = point[2],
    row.names = NULL
  )

  if (n_boot > 0) {
    reps <- .plm_boot_replicates(fit, n_boot = n_boot, cores = cores)
    # Recompute the whole bound on each replicate.
    lows <- vapply(reps, function(p) {
      if (is.null(p)) return(NA_real_)
      min(.plm_estimate_from(p, corners$k, corners$imperfection))
    }, numeric(1))
    highs <- vapply(reps, function(p) {
      if (is.null(p)) return(NA_real_)
      max(.plm_estimate_from(p, corners$k, corners$imperfection))
    }, numeric(1))
    lows  <- lows[is.finite(lows)]
    highs <- highs[is.finite(highs)]

    if (ci_type == "percentile") {
      out$lower_boot_q <- unname(stats::quantile(lows,  probs = alpha / 2))
      out$upper_boot_q <- unname(stats::quantile(highs, probs = 1 - alpha / 2))
    } else {
      z <- stats::qnorm(1 - alpha / 2)
      out$lower_boot_q <- point[1] - z * stats::sd(lows)
      out$upper_boot_q <- point[2] + z * stats::sd(highs)
    }
  }

  out
}


#' Solve for the relative confounding that produces a given estimate
#'
#' @description
#' The inverse problem, which the paper uses twice: to locate a tipping point
#' ("the effect estimates are negative once k > 0.7") and to back out the true
#' relative confounding from an external benchmark ("we can use the experimental
#' benchmark to back out an estimate of the true, otherwise unobservable, value
#' of k at k = 0.812").
#'
#' Because the estimator is linear in `k`, this is closed-form:
#' `k* = (beta_target - target) / ((beta_sens - imperfection) * SF)`.
#'
#' @param fit A `placebo_lm` object.
#' @param target Numeric. The estimate to solve for. Defaults to `0`, giving the
#'   value of `k` at which the adjusted estimate changes sign.
#' @param imperfection Numeric scalar. Postulated placebo imperfection.
#'   Defaults to `0`.
#'
#' @return A one-row data frame with columns `target`, `k`, `m`, and
#'   `imperfection`.
#'
#' @examples
#' set.seed(1)
#' n <- 400
#' U <- rnorm(n); D <- U + rnorm(n)
#' P <- U + rnorm(n); Y <- D + U + rnorm(n)
#' fit <- placebo_lm(data.frame(Y = Y, D = D, P = P),
#'                   "Y", "D", "P", structure = "placebo_outcome")
#'
#' plm_solve(fit)            # tipping point: where the estimate crosses zero
#' plm_solve(fit, target = 1)  # k reproducing an external benchmark of 1
#'
#' @export
plm_solve <- function(fit, target = 0, imperfection = 0) {
  .plm_check_fit(fit)
  imperfection <- .plm_check_imperfection(fit, imperfection)
  if (length(target) != 1L || !is.numeric(target))
    stop("`target` must be a single number.", call. = FALSE)

  denom <- (fit$coefs$sens$estimate - imperfection) * fit$SF
  if (isTRUE(all.equal(denom, 0)))
    stop("Cannot solve for k: the adjusted estimate does not vary with k at ",
         "this value of\n`imperfection` (the sensitivity coefficient equals ",
         "the postulated imperfection,\nso the bias adjustment is identically ",
         "zero).", call. = FALSE)

  k <- (fit$coefs$target$estimate - target) / denom

  data.frame(
    target = target, k = k, m = k * fit$SF, imperfection = imperfection,
    row.names = NULL
  )
}
