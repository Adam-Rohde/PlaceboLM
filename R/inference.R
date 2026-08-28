# Inference --------------------------------------------------------------------
#
# Nonparametric bootstrap over rows. The paper notes that "inference can be
# conducted using the non-parametric bootstrap, or by constructing bootstrap
# standard errors and applying the normal approximation"; both are offered via
# `ci_type`.
#
# The design point here is that resampling happens ONCE per call and the
# replicate coefficients are reused across every requested combination of
# sensitivity parameters. The quantities that vary across a sensitivity grid
# (k, imperfection) are postulated, not estimated, so re-drawing the bootstrap
# at each grid point would be pure waste -- and would additionally make the
# grid non-smooth by giving each point independent Monte Carlo error.


# Draw n_boot resamples and return the refit coefficient triples for each.
# Replicates that fail (e.g. a resample that drops a factor level) come back as
# NULL and are filtered downstream rather than aborting the run.
.plm_boot_replicates <- function(fit, n_boot, cores = NULL,
                                 engine = c("lm", "matrix")) {
  engine <- match.arg(engine)
  n <- nrow(fit$data)

  # Indices are drawn HERE, in the parent process, before any forking. That is
  # what makes set.seed() reproducible at any core count: the workers do no
  # random number generation of their own.
  idx <- lapply(seq_len(n_boot), function(i) sample.int(n, n, replace = TRUE))

  one <- if (engine == "matrix") {
    prep <- .plm_matrix_prep(fit)
    function(ii) tryCatch(.plm_refit_matrix(fit, prep, ii),
                          error = function(e) NULL)
  } else {
    function(ii) tryCatch(.plm_refit(fit, fit$data[ii, , drop = FALSE]),
                          error = function(e) NULL)
  }

  cores <- .plm_cores(cores)
  if (cores > 1L && .Platform$OS.type != "windows") {
    parallel::mclapply(idx, one, mc.cores = cores)
  } else {
    lapply(idx, one)
  }
}


.plm_cores <- function(cores = NULL) {
  if (!is.null(cores)) return(max(1L, as.integer(cores)))
  detected <- tryCatch(parallel::detectCores(logical = TRUE),
                       error = function(e) 1L)
  if (is.na(detected)) detected <- 1L
  max(1L, detected - 1L)
}


#' Bootstrap confidence intervals over a sensitivity grid
#'
#' @description
#' Evaluates the adjusted estimate with a bootstrap confidence interval at every
#' requested combination of the relative-confounding parameter and the placebo
#' imperfection. Resampling is performed once and reused across the whole grid.
#'
#' @param fit A `placebo_lm` object.
#' @param k,m Numeric vectors of relative-confounding values. Supply exactly one.
#' @param imperfection Numeric vector of placebo-imperfection values. Crossed
#'   with `k`/`m` to form the grid. Defaults to `0`.
#' @param n_boot Non-negative integer. Bootstrap replicates. `0` returns point
#'   estimates only.
#' @param alpha Numeric. Significance level. Defaults to `0.05`.
#' @param ci_type `"percentile"` (default) or `"normal"`.
#' @param cores Integer. Cores for the bootstrap.
#' @param engine `"lm"` (default) or `"matrix"`. The `"matrix"` engine builds
#'   each model matrix once and solves by QR directly instead of re-running
#'   `lm()` per replicate. Measured 2-4x faster, with the larger gains at
#'   smaller `n` (the overhead it removes is fixed, while the QR it still
#'   performs grows with `n`). It is opt-in: `"lm"` is the path whose numbers
#'   back the published results. The two agree to within 1e-10; see
#'   `test-engine.R`.
#'
#' @section Sampling assumptions:
#' The bootstrap resamples rows independently, and the supporting asymptotic
#' theory assumes i.i.d. sampling. If your design has clustered, panel,
#' time-series or spatially dependent sampling, choose an inference method
#' suited to it: [plm_analytic()] accepts any `vcov` estimator, including
#' cluster-robust ones, on the `m` path.
#'
#' @return A data frame with one row per grid point and columns `k`, `m`,
#'   `imperfection`, `adjusted_coefficient`, and -- when `n_boot > 0` --
#'   `std_error`, `ci_lower`, `ci_upper`.
#'
#' @examples
#' set.seed(1)
#' n <- 300
#' U <- rnorm(n); D <- U + rnorm(n)
#' P <- U + rnorm(n); Y <- D + U + rnorm(n)
#' fit <- placebo_lm(data.frame(Y = Y, D = D, P = P),
#'                   "Y", "D", "P", structure = "placebo_outcome")
#'
#' plm_grid(fit, k = c(0, 0.5, 1), n_boot = 50)
#'
#' @export
plm_grid <- function(fit, k = NULL, m = NULL, imperfection = 0,
                     n_boot = 1000, alpha = 0.05,
                     ci_type = c("percentile", "normal"), cores = NULL,
                     engine = c("lm", "matrix")) {
  .plm_check_fit(fit)
  ci_type <- match.arg(ci_type)
  engine  <- match.arg(engine)

  k_vals <- .plm_resolve_k(fit, k, m)
  imperfection <- .plm_check_imperfection(fit, imperfection)

  grid <- expand.grid(k = k_vals, imperfection = imperfection,
                      KEEP.OUT.ATTRS = FALSE)
  grid$m <- grid$k * fit$SF
  grid$adjusted_coefficient <- plm_estimate(fit, k = grid$k,
                                            imperfection = grid$imperfection)
  grid <- grid[, c("k", "m", "imperfection", "adjusted_coefficient")]

  if (n_boot > 0) {
    reps <- .plm_boot_replicates(fit, n_boot = n_boot, cores = cores,
                                 engine = engine)
    reps <- Filter(Negate(is.null), reps)
    if (!length(reps))
      stop("All bootstrap replicates failed to fit.", call. = FALSE)

    # One matrix: rows are replicates, columns are grid points. Built from the
    # single set of resamples.
    boot_mat <- vapply(reps, function(p) {
      .plm_estimate_from(p, grid$k, grid$imperfection)
    }, numeric(nrow(grid)))
    if (is.null(dim(boot_mat))) boot_mat <- matrix(boot_mat, nrow = nrow(grid))

    grid$std_error <- apply(boot_mat, 1, stats::sd, na.rm = TRUE)
    if (ci_type == "percentile") {
      qs <- apply(boot_mat, 1, stats::quantile,
                  probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
      grid$ci_lower <- qs[1, ]
      grid$ci_upper <- qs[2, ]
    } else {
      z <- stats::qnorm(1 - alpha / 2)
      grid$ci_lower <- grid$adjusted_coefficient - z * grid$std_error
      grid$ci_upper <- grid$adjusted_coefficient + z * grid$std_error
    }
  }

  rownames(grid) <- NULL
  grid
}
