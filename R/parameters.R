# Sensitivity parameters -------------------------------------------------------
#
# The paper reasons in two parameterizations of relative confounding:
#
#   m  the raw ratio of biases, bias(YD.X) / bias(PD.X). Conventional
#      difference-in-differences is exactly m = 1 ("parallel trends on the
#      observed scale").
#
#   k  the scale-free version, k = R_{Y~Z|D,X} / R_{P~Z|D,X}, which compares
#      partial correlations rather than raw biases and so is invariant to the
#      scales of Y and P.
#
# They are related by m = k * SF, where SF is the scale factor computed once in
# placebo_lm(). Both are accepted throughout the package: supply exactly one.
#
# Which to use is a substantive choice. m is interpretable when the placebo and
# the outcome are on the same scale (pre- and post-treatment measures of the
# same variable). k remains interpretable when they are not -- the paper's NSW
# example uses 1975 unemployment as a placebo for 1978 earnings, where the scale
# factor exceeds 40,000 and reasoning about m is meaningless.


#' Convert between the m and k parameterizations
#'
#' @description
#' `plm_m_to_k()` and `plm_k_to_m()` convert between the two relative-confounding
#' parameterizations used in the paper, using the scale factor of a fitted model.
#' They are related by `m = k * SF`.
#'
#' @param fit A `placebo_lm` object from [placebo_lm()].
#' @param m,k Numeric vector of parameter values to convert.
#'
#' @return A numeric vector.
#'
#' @examples
#' set.seed(1)
#' dat <- data.frame(Y = rnorm(200), D = rnorm(200), P = rnorm(200))
#' fit <- placebo_lm(dat, "Y", "D", "P", structure = "placebo_outcome")
#'
#' # Conventional difference-in-differences is m = 1
#' plm_m_to_k(fit, 1)
#'
#' @name plm_convert
#' @export
plm_m_to_k <- function(fit, m) {
  .plm_check_fit(fit)
  m / fit$SF
}

#' @rdname plm_convert
#' @export
plm_k_to_m <- function(fit, k) {
  .plm_check_fit(fit)
  k * fit$SF
}


# Resolve the relative-confounding argument. Exactly one of k or m must be
# given; the result is always on the k scale, which is what the estimator uses.
.plm_resolve_k <- function(fit, k = NULL, m = NULL) {
  if (is.null(k) && is.null(m))
    stop("Supply exactly one of `k` or `m` (the relative-confounding ",
         "parameter).\n",
         "  k is scale-free; m is the raw ratio of biases, for which ",
         "conventional\n  difference-in-differences corresponds to m = 1.",
         call. = FALSE)
  if (!is.null(k) && !is.null(m))
    stop("Supply exactly one of `k` or `m`, not both. They are related by ",
         "m = k * SF.", call. = FALSE)

  val <- if (is.null(k)) m / fit$SF else k
  if (!is.numeric(val))
    stop("`", if (is.null(k)) "m" else "k", "` must be numeric.", call. = FALSE)
  if (anyNA(val))
    stop("`", if (is.null(k)) "m" else "k", "` must not contain NA.",
         call. = FALSE)
  val
}


# Validate the placebo-imperfection argument.
.plm_check_imperfection <- function(fit, imperfection) {
  if (!is.numeric(imperfection))
    stop("`imperfection` must be numeric. For this structure it is ",
         fit$spec$sens_param, ",\nthe value the sensitivity coefficient would ",
         "take if the unobserved confounders\nwere included in the regression. ",
         "Zero means a perfect placebo.", call. = FALSE)
  if (anyNA(imperfection))
    stop("`imperfection` must not contain NA.", call. = FALSE)
  imperfection
}


.plm_check_fit <- function(fit) {
  if (!inherits(fit, "placebo_lm"))
    stop("`fit` must be a `placebo_lm` object, as returned by placebo_lm().",
         call. = FALSE)
  invisible(TRUE)
}
