# Regression representation ----------------------------------------------------
#
# Rohde and Hazlett show that the m-parameterized adjustment is exactly an
# ordinary least squares fit on a pseudo-outcome. By the Frisch-Waugh-Lovell
# theorem,
#
#   mu_m := beta_Y~D|X - m * beta_P~D|X
#
# is precisely the coefficient on D in the regression of (Y - m*P) on (D, X).
# Since the adjusted estimate differs from mu_m only by the constant
# m * imperfection at postulated parameter values, the two have the same
# sampling distribution. Conditional on (m, imperfection), therefore, the
# standard error of the adjusted estimate is just the standard error reported
# for D in that pseudo-outcome regression -- under any standard error model:
# classical, heteroskedasticity-robust, or cluster-robust.
#
# This gives exact analytic inference for the m parameterization, and it is why
# clustered inference comes essentially for free: hand the returned model to
# whatever variance estimator you already use.
#
# It applies only where the target and sensitivity coefficients are the
# coefficient on the same regressor in two regressions sharing a right-hand
# side, which is the placebo-outcome case the paper derives. When reasoning
# with k rather than m the scale factor is itself estimated, and the paper
# recommends the nonparametric bootstrap instead -- see plm_grid().


# Structures for which the pseudo-outcome representation is exact.
.plm_regression_ok <- "placebo_outcome"


#' Regression representation of the adjusted estimate
#'
#' @description
#' Returns the ordinary least squares fit whose coefficient on the treatment
#' *is* the adjusted estimate: the regression of the pseudo-outcome
#' `Y - m * P` on the treatment and covariates.
#'
#' This is useful because it hands the problem back to ordinary regression
#' machinery. Conditional on the postulated sensitivity parameters, the standard
#' error, confidence interval, and test for the adjusted estimate are exactly
#' those of the treatment coefficient in this model — under whatever variance
#' estimator you care to apply, including heteroskedasticity-robust and
#' cluster-robust estimators from packages such as `sandwich` and `lmtest`.
#'
#' The representation is exact for the `"placebo_outcome"` structure. For other
#' structures, and whenever you reason with `k` rather than `m` (where the scale
#' factor is itself estimated), use the bootstrap via [plm_grid()] instead.
#'
#' @param fit A `placebo_lm` object with structure `"placebo_outcome"`.
#' @param m Numeric scalar. The raw ratio of biases. Supply exactly one of `m`
#'   or `k`; note that fixing `k` fixes `m` only up to the estimated scale
#'   factor, so the resulting standard errors condition on `SF`.
#' @param k Numeric scalar. Converted to `m` via `m = k * SF`.
#' @param imperfection Numeric scalar. Postulated placebo imperfection.
#'   Defaults to `0`. It shifts the estimate by `m * imperfection` but does not
#'   affect the standard error.
#'
#' @return An `lm` object, with an attribute `"plm_offset"` giving the constant
#'   `m * imperfection` that must be added to its treatment coefficient to
#'   recover the adjusted estimate. When `imperfection = 0` the offset is zero
#'   and the coefficient is the adjusted estimate directly.
#'
#' @examples
#' set.seed(1)
#' n <- 400
#' U <- rnorm(n); D <- U + rnorm(n)
#' P <- U + rnorm(n); Y <- D + U + rnorm(n)
#' fit <- placebo_lm(data.frame(Y = Y, D = D, P = P),
#'                   "Y", "D", "P", structure = "placebo_outcome")
#'
#' mod <- plm_regression(fit, m = 1)
#' coef(summary(mod))["D", ]
#'
#' # matches the estimate from plm_estimate()
#' plm_estimate(fit, m = 1)
#'
#' @export
plm_regression <- function(fit, m = NULL, k = NULL, imperfection = 0) {
  .plm_check_fit(fit)

  if (!fit$structure %in% .plm_regression_ok)
    stop("The regression representation is available for the '",
         paste(.plm_regression_ok, collapse = "', '"), "' structure only; ",
         "this fit uses '", fit$structure, "'.\n",
         "It requires the target and sensitivity coefficients to be the ",
         "coefficient on the same\nregressor in two regressions sharing a ",
         "right-hand side. Use plm_grid() for bootstrap\ninference instead.",
         call. = FALSE)

  m_val <- if (is.null(m)) .plm_resolve_k(fit, k, NULL) * fit$SF else m
  if (length(m_val) != 1L)
    stop("`m` (or `k`) must be a single value.", call. = FALSE)
  imperfection <- .plm_check_imperfection(fit, imperfection)
  if (length(imperfection) != 1L)
    stop("`imperfection` must be a single value.", call. = FALSE)

  d <- fit$data
  pseudo <- ".plm_pseudo_outcome"
  d[[pseudo]] <- d[[fit$vars$Y]] - m_val * d[[fit$vars$P]]

  f <- .plm_formula(pseudo, fit$vars$D, fit$vars$X)
  mod <- stats::lm(f, data = d)

  attr(mod, "plm_offset") <- m_val * imperfection
  mod
}


#' Analytic standard error and interval for the adjusted estimate
#'
#' @description
#' Exact analytic inference for the `m` parameterization, via the regression
#' representation of [plm_regression()]. Conditional on the postulated
#' sensitivity parameters, the adjusted estimate is an ordinary regression
#' coefficient, so its standard error is available in closed form rather than by
#' resampling.
#'
#' Supply `vcov` to use a heteroskedasticity- or cluster-robust variance
#' estimator; the default is the classical one.
#'
#' When reasoning with `k`, the scale factor is itself estimated and these
#' intervals condition on it. The paper recommends the nonparametric bootstrap
#' in that case — see [plm_grid()].
#'
#' @param fit A `placebo_lm` object with structure `"placebo_outcome"`.
#' @param m,k Numeric scalar relative-confounding parameter. Supply one.
#' @param imperfection Numeric scalar. Postulated placebo imperfection.
#' @param alpha Numeric. Significance level. Defaults to `0.05`.
#' @param vcov Optional function taking the fitted model and returning a
#'   variance-covariance matrix, e.g. `sandwich::vcovHC` or a call to
#'   `sandwich::vcovCL`. Defaults to [stats::vcov()].
#'
#' @return A one-row data frame with columns `m`, `k`, `imperfection`,
#'   `adjusted_coefficient`, `std_error`, `ci_lower`, and `ci_upper`.
#'
#' @examples
#' set.seed(1)
#' n <- 400
#' U <- rnorm(n); D <- U + rnorm(n)
#' P <- U + rnorm(n); Y <- D + U + rnorm(n)
#' fit <- placebo_lm(data.frame(Y = Y, D = D, P = P),
#'                   "Y", "D", "P", structure = "placebo_outcome")
#'
#' plm_analytic(fit, m = 1)
#'
#' @export
plm_analytic <- function(fit, m = NULL, k = NULL, imperfection = 0,
                         alpha = 0.05, vcov = NULL) {
  mod <- plm_regression(fit, m = m, k = k, imperfection = imperfection)
  offset <- attr(mod, "plm_offset")

  m_val <- if (is.null(m)) .plm_resolve_k(fit, k, NULL) * fit$SF else m

  V <- if (is.null(vcov)) stats::vcov(mod) else vcov(mod)
  d_name <- fit$vars$D
  est <- unname(stats::coef(mod)[[d_name]]) + offset
  se  <- sqrt(V[d_name, d_name])
  tq  <- stats::qt(1 - alpha / 2, df = mod$df.residual)

  data.frame(
    m = m_val, k = m_val / fit$SF, imperfection = imperfection,
    adjusted_coefficient = est, std_error = se,
    ci_lower = est - tq * se, ci_upper = est + tq * se,
    row.names = NULL
  )
}
