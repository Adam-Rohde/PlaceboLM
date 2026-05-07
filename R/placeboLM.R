
# ---- helpers -----------------------------------------------------------------

# Build an lm formula from response, mandatory predictors, and optional covariates.
.make_formula <- function(response, predictors, covariates) {
  rhs <- c(predictors, covariates)
  if (length(rhs) == 0) rhs <- "1"
  reformulate(rhs, response = response)
}


# ---- placeboLM ---------------------------------------------------------------

#' Set up a PlaceboLM analysis
#'
#' @description
#' Configures a partial-identification causal analysis using a single imperfect
#' placebo variable. The function determines appropriate regression
#' specifications from the assumed causal structure between the placebo,
#' treatment, and outcome, following Rohde and Hazlett (2025).
#'
#' Exactly one of `placebo_outcome` or `placebo_treatment` must be supplied.
#'
#' @param data A data frame containing all analysis variables.
#' @param outcome Character. Name of the outcome variable (`Y`).
#' @param treatment Character. Name of the treatment variable (`D`).
#' @param placebo_outcome Character or `NULL`. Name of a placebo outcome
#'   variable (`N`), e.g. a pre-treatment measure of the outcome.
#' @param placebo_treatment Character or `NULL`. Name of a placebo treatment
#'   variable (`P`), e.g. an alternative exposure that shares confounders with
#'   the real treatment.
#' @param DP Character. Assumed direct causal relationship between the placebo
#'   treatment (`P`) and the treatment (`D`). One of `""` (no direct path),
#'   `"->"` (treatment causes placebo, i.e. D→P), or `"<-"` (placebo causes
#'   treatment, i.e. P→D, meaning P is an observed confounder). Only relevant
#'   when `placebo_treatment` is supplied.
#' @param PY Character. Assumed direct causal relationship between the placebo
#'   outcome (`P`) and the outcome (`Y`). One of `""` (no direct path),
#'   `"->"` (placebo causes outcome, i.e. P→Y), or `"<-"` (outcome causes
#'   placebo, i.e. Y→P). Only relevant when `placebo_outcome` is supplied.
#' @param observed_covariates Character vector of covariate names to include in
#'   all regressions. Defaults to `NULL` (intercept only).
#' @param partialIDparam_minmax Named list. Each element is a length-2 numeric
#'   vector `c(min, max)` giving the assumed range of one partial-identification
#'   parameter. Typical parameters are `k` (confounding-ratio relative to the
#'   placebo relationship) and a coefficient capturing the residual
#'   placebo-variable association after conditioning on observed covariates.
#'   See the vignettes for parameter names by placebo type.
#'
#' @return An object of class `"placeboLM"`: a list with components
#'   `data`, `outcome`, `treatment`, `placebo`, `placebo_outcome`,
#'   `placebo_treatment`, `DP`, `PY`, `observed_covariates`,
#'   `partialIDparam_minmax`, `type`, and `regressions` (a named list of
#'   `formula` objects).
#'
#' @references
#' Rohde, A. and Hazlett, C. (2025). Causal progress with imperfect placebo
#' treatments and outcomes. *Journal of the Royal Statistical Society: Series A*.
#'
#' @examples
#' set.seed(1)
#' n <- 400
#' dat <- data.frame(
#'   Y = rnorm(n), D = rbinom(n, 1, 0.5),
#'   P = rnorm(n), X = rnorm(n)
#' )
#' plm <- placeboLM(
#'   data = dat,
#'   outcome = "Y", treatment = "D",
#'   placebo_outcome = "P",
#'   observed_covariates = "X",
#'   partialIDparam_minmax = list(k = c(-2, 2), coef_P_D_given_XZ = c(-1, 1))
#' )
#'
#' @export
placeboLM <- function(data,
                      outcome,
                      treatment,
                      placebo_outcome       = NULL,
                      placebo_treatment     = NULL,
                      DP                    = c("", "->", "<-"),
                      PY                    = c("", "->", "<-"),
                      observed_covariates   = NULL,
                      partialIDparam_minmax = list(k = c(-2, 2),
                                                   coef_P_D_given_XZ = c(-2, 2))) {

  DP <- match.arg(DP)
  PY <- match.arg(PY)

  # ---- input validation ------------------------------------------------------
  if (!is.data.frame(data))
    stop("`data` must be a data frame.")
  if (!outcome %in% names(data))
    stop("`outcome` variable '", outcome, "' not found in `data`.")
  if (!treatment %in% names(data))
    stop("`treatment` variable '", treatment, "' not found in `data`.")
  if (!is.null(placebo_outcome) && !placebo_outcome %in% names(data))
    stop("`placebo_outcome` variable '", placebo_outcome, "' not found in `data`.")
  if (!is.null(placebo_treatment) && !placebo_treatment %in% names(data))
    stop("`placebo_treatment` variable '", placebo_treatment, "' not found in `data`.")
  if (!is.null(observed_covariates)) {
    missing_covs <- setdiff(observed_covariates, names(data))
    if (length(missing_covs))
      stop("Covariate(s) not found in `data`: ",
           paste(missing_covs, collapse = ", "))
  }
  if (is.null(placebo_outcome) && is.null(placebo_treatment))
    stop("Provide exactly one of `placebo_outcome` or `placebo_treatment`.")
  if (!is.null(placebo_outcome) && !is.null(placebo_treatment))
    stop("Provide exactly one of `placebo_outcome` or `placebo_treatment`, not both.")

  # ---- setup -----------------------------------------------------------------
  placebo <- if (!is.null(placebo_outcome)) placebo_outcome else placebo_treatment

  mf <- function(response, predictors) {
    .make_formula(response, predictors, observed_covariates)
  }

  # ---- determine placebo type and regression formulas ------------------------

  if (PY == "<-" && DP == "<-")
    stop("The combination PY='<-' and DP='<-' implies a cycle. Check your causal assumptions.")

  type <- NULL
  regressions <- list()

  # Single Placebo, No Direct Relationships
  if (PY == "" && DP == "") {
    message("Placebo assumed to have no direct relationship with either treatment or outcome.")
    if (!is.null(placebo_outcome)) {
      type <- "Single Placebo, No Direct Relationships, Placebo Outcome"
      regressions <- list(
        reg_Y_on_D   = mf(outcome,  treatment),
        reg_P_on_D   = mf(placebo,  treatment)
      )
    } else {
      type <- "Single Placebo, No Direct Relationships, Placebo Treatment"
      regressions <- list(
        reg_Y_on_D_plus_P = mf(outcome, c(treatment, placebo))
      )
    }

  # Single Placebo, Treatment causes Placebo (D→P)
  } else if (PY == "" && DP == "->") {
    message("Placebo assumed to be directly caused by treatment (D→P).")
    type <- "Single Placebo, Treatment causes Placebo"
    regressions <- list(
      reg_Y_on_D = mf(outcome, treatment),
      reg_P_on_D = mf(placebo, treatment)
    )

  # Single Placebo, Placebo causes Outcome (P→Y)
  } else if (PY == "->" && DP == "") {
    message("Placebo assumed to directly cause outcome (P→Y).")
    if (!is.null(placebo_treatment)) {
      type <- "Single Placebo, Placebo causes Outcome, Placebo Treatment"
      regressions <- list(
        reg_Y_on_D_plus_P = mf(outcome, c(treatment, placebo))
      )
    } else {
      type <- "Single Placebo, Placebo causes Outcome, Placebo Outcome"
      regressions <- list(
        reg_Y_on_D_plus_P = mf(outcome, c(treatment, placebo)),
        reg_P_on_D        = mf(placebo, treatment)
      )
    }

  # Single Placebo, Placebo is Mediator (D→P→Y)
  } else if (PY == "->" && DP == "->") {
    message("Placebo assumed to be a mediator (D→P→Y). ",
            "PlaceboLM targets the total effect of D on Y.")
    if (!is.null(placebo_outcome)) {
      type <- "Single Placebo, Placebo is Mediator, Placebo Outcome"
      regressions <- list(
        reg_Y_on_D = mf(outcome, treatment),
        reg_P_on_D = mf(placebo, treatment)
      )
    } else {
      type <- "Single Placebo, Placebo is Mediator, Placebo Treatment"
      regressions <- list(
        reg_Y_on_D       = mf(outcome, treatment),
        reg_Y_on_D_plus_P = mf(outcome, c(treatment, placebo))
      )
    }

  # Single Placebo, Placebo is Observed Confounder (P→D)
  } else if (DP == "<-") {
    message("Placebo assumed to be an observed confounder (P→D).")
    type <- "Single Placebo, Placebo is Observed Confounder"
    regressions <- list(
      reg_Y_on_D_plus_P = mf(outcome,    c(treatment, placebo)),
      reg_D_on_P        = mf(treatment,  placebo)
    )

  # Single Placebo, Outcome causes Placebo (Y→P)
  } else if (PY == "<-") {
    message("Placebo assumed to be a descendant of outcome (Y→P).")
    type <- "Single Placebo, Outcome causes Placebo"
    regressions <- list(
      reg_Y_on_D       = mf(outcome, treatment),
      reg_P_on_Y_plus_D = mf(placebo, c(outcome, treatment))
    )
  }

  message("Placebo type: ", type)
  for (nm in names(regressions)) {
    message("  ", nm, ": ", deparse(regressions[[nm]]))
  }

  structure(
    list(
      data                  = data,
      outcome               = outcome,
      treatment             = treatment,
      placebo               = placebo,
      placebo_outcome       = placebo_outcome,
      placebo_treatment     = placebo_treatment,
      DP                    = DP,
      PY                    = PY,
      observed_covariates   = observed_covariates,
      partialIDparam_minmax = partialIDparam_minmax,
      type                  = type,
      regressions           = regressions
    ),
    class = "placeboLM"
  )
}


# ---- estimate_regs -----------------------------------------------------------

#' Fit the regressions stored in a placeboLM object
#'
#' @description
#' Fits each regression formula in a `placeboLM` object via `lm()` and returns
#' the coefficient estimates, standard errors, and residual degrees of freedom.
#' Normally called internally; exposed for advanced use and bootstrap.
#'
#' @param plm A `placeboLM` object from [placeboLM()].
#' @param data Optional data frame. When supplied (e.g. a bootstrap resample),
#'   this data frame is used instead of `plm$data`.
#'
#' @return A named list parallel to `plm$regressions`. Each element is itself a
#'   list with components `betas` (named numeric vector of coefficients),
#'   `ses` (named numeric vector of standard errors), and `df` (integer
#'   residual degrees of freedom).
#'
#' @export
estimate_regs <- function(plm, data = NULL) {
  dset <- if (!is.null(data)) data else plm$data
  lapply(plm$regressions, function(fml) {
    m          <- lm(fml, data = dset)
    coef_table <- stats::coef(summary(m))
    list(
      betas = coef_table[, "Estimate"],
      ses   = coef_table[, "Std. Error"],
      df    = m$df.residual
    )
  })
}


# ---- estimate_PLM ------------------------------------------------------------

#' Compute a PlaceboLM point estimate
#'
#' @description
#' Given fitted regression results and values for the partial-identification
#' parameters, computes the adjusted causal estimate (or the scale factor used
#' to translate *k* into difference-in-differences units).
#'
#' @param plm A `placeboLM` object.
#' @param partialIDparam Named list of partial-identification parameter values.
#'   Names and interpretation depend on the placebo type; see the vignettes.
#'   Setting all values to zero recovers the selection-on-observables (SOO)
#'   estimate.
#' @param estimated_regs Output of [estimate_regs()].
#' @param returned One of `"estimate"` (the adjusted causal estimate) or
#'   `"SF"` (the scale factor linking *k* to the omitted-variable bias
#'   formula).
#'
#' @return A single numeric value.
#'
#' @export
estimate_PLM <- function(plm, partialIDparam, estimated_regs,
                         returned = c("estimate", "SF")) {
  returned <- match.arg(returned)

  type <- plm$type
  er   <- estimated_regs

  if (type %in% c("Single Placebo, No Direct Relationships, Placebo Outcome",
                  "Single Placebo, Treatment causes Placebo",
                  "Single Placebo, Placebo is Mediator, Placebo Outcome")) {

    beta_yd.x  <- er$reg_Y_on_D$betas[plm$treatment]
    beta_pd.x  <- er$reg_P_on_D$betas[plm$treatment]
    se_yd.x    <- er$reg_Y_on_D$ses[plm$treatment]
    se_pd.x    <- er$reg_P_on_D$ses[plm$treatment]
    df_y       <- er$reg_Y_on_D$df
    df_p       <- er$reg_P_on_D$df

    k          <- partialIDparam$k
    beta_pd.xz <- partialIDparam$coef_P_D_given_XZ

    SF         <- (se_yd.x * sqrt(df_y)) / (se_pd.x * sqrt(df_p))
    estimate   <- beta_yd.x - k * (beta_pd.x - beta_pd.xz) * SF

  } else if (type %in% c("Single Placebo, No Direct Relationships, Placebo Treatment",
                         "Single Placebo, Placebo causes Outcome, Placebo Treatment")) {

    beta_yd.px  <- er$reg_Y_on_D_plus_P$betas[plm$treatment]
    beta_yp.dx  <- er$reg_Y_on_D_plus_P$betas[plm$placebo_treatment]
    se_yd.px    <- er$reg_Y_on_D_plus_P$ses[plm$treatment]
    se_yp.dx    <- er$reg_Y_on_D_plus_P$ses[plm$placebo_treatment]
    df_y        <- er$reg_Y_on_D_plus_P$df

    k           <- partialIDparam$k
    beta_yp.dxz <- partialIDparam$coef_Y_P_given_DXZ

    SF          <- (se_yd.px * sqrt(df_y)) / (se_yp.dx * sqrt(df_y))
    estimate    <- beta_yd.px - k * (beta_yp.dx - beta_yp.dxz) * SF

  } else if (type == "Single Placebo, Placebo causes Outcome, Placebo Outcome") {

    beta_yd.px  <- er$reg_Y_on_D_plus_P$betas[plm$treatment]
    beta_pd.x   <- er$reg_P_on_D$betas[plm$treatment]
    se_yd.px    <- er$reg_Y_on_D_plus_P$ses[plm$treatment]
    se_pd.x     <- er$reg_P_on_D$ses[plm$treatment]
    df_y        <- er$reg_Y_on_D_plus_P$df
    df_p        <- er$reg_P_on_D$df

    k           <- partialIDparam$k
    beta_pd.xz  <- partialIDparam$coef_P_D_given_XZ

    SF          <- (se_yd.px * sqrt(df_y)) / (se_pd.x * sqrt(df_p))
    estimate    <- beta_yd.px - k * (beta_pd.x - beta_pd.xz) * SF

  } else if (type == "Single Placebo, Placebo is Mediator, Placebo Treatment") {

    beta_yd.x   <- er$reg_Y_on_D$betas[plm$treatment]
    beta_yp.dx  <- er$reg_Y_on_D_plus_P$betas[plm$placebo]
    se_yd.x     <- er$reg_Y_on_D$ses[plm$treatment]
    se_yp.dx    <- er$reg_Y_on_D_plus_P$ses[plm$placebo]
    df_yd       <- er$reg_Y_on_D$df
    df_yp       <- er$reg_Y_on_D_plus_P$df

    k           <- partialIDparam$k
    beta_yp.dxz <- partialIDparam$coef_Y_P_given_DXZ

    SF          <- (se_yd.x * sqrt(df_yd)) / (se_yp.dx * sqrt(df_yp))
    estimate    <- beta_yd.x - k * (beta_yp.dx - beta_yp.dxz) * SF

  } else if (type == "Single Placebo, Placebo is Observed Confounder") {

    beta_yd.px  <- er$reg_Y_on_D_plus_P$betas[plm$treatment]
    beta_dp.x   <- er$reg_D_on_P$betas[plm$placebo]
    se_yd.px    <- er$reg_Y_on_D_plus_P$ses[plm$treatment]
    se_dp.x     <- er$reg_D_on_P$ses[plm$placebo]
    df_yd       <- er$reg_Y_on_D_plus_P$df
    df_dp       <- er$reg_D_on_P$df

    k           <- partialIDparam$k
    beta_dp.xz  <- partialIDparam$coef_D_P_given_XZ

    SF          <- (se_yd.px * sqrt(df_yd)) / (se_dp.x * sqrt(df_dp))
    estimate    <- beta_yd.px - k * (beta_dp.x - beta_dp.xz) * SF

  } else if (type == "Single Placebo, Outcome causes Placebo") {

    beta_yd.x   <- er$reg_Y_on_D$betas[plm$treatment]
    beta_py.dx  <- er$reg_P_on_Y_plus_D$betas[plm$outcome]
    se_yd.x     <- er$reg_Y_on_D$ses[plm$treatment]
    se_py.dx    <- er$reg_P_on_Y_plus_D$ses[plm$outcome]
    df_yd       <- er$reg_Y_on_D$df
    df_py       <- er$reg_P_on_Y_plus_D$df

    k           <- partialIDparam$k
    beta_py.dxz <- partialIDparam$coef_P_Y_given_DXZ

    SF          <- (se_yd.x * sqrt(df_yd)) / (se_py.dx * sqrt(df_py))
    estimate    <- beta_yd.x - k * (beta_py.dx - beta_py.dxz) * SF

  } else {
    stop("Unknown placebo type: ", type)
  }

  if (returned == "estimate") estimate else SF
}


# ---- boot_funk ---------------------------------------------------------------

#' Bootstrap statistic function for use with boot::boot
#'
#' @description
#' A thin wrapper around [estimate_regs()] and [estimate_PLM()] that conforms
#' to the interface expected by [boot::boot()]. Normally called internally by
#' [bootstrap_regs()].
#'
#' @param boot_data The full data frame passed to [boot::boot()].
#' @param indys Integer index vector selected by the bootstrap.
#' @param plm A `placeboLM` object.
#' @param partialIDparam Named list of partial-identification parameter values.
#'
#' @return A single numeric estimate.
#'
#' @export
boot_funk <- function(boot_data, indys, plm, partialIDparam) {
  regs <- estimate_regs(plm, data = boot_data[indys, , drop = FALSE])
  estimate_PLM(plm, partialIDparam, regs, "estimate")
}


# ---- bootstrap_regs ----------------------------------------------------------

#' Nonparametric bootstrap for a PlaceboLM estimate
#'
#' @description
#' Draws `n_boot` nonparametric bootstrap resamples of the rows of `plm$data`
#' and returns the vector of bootstrap estimates. Parallelisation uses multiple
#' cores on Unix/macOS and falls back to `"snow"` on Windows.
#'
#' @param plm A `placeboLM` object.
#' @param partialIDparam Named list of partial-identification parameter values.
#' @param n_boot Positive integer. Number of bootstrap replicates.
#'
#' @return A numeric vector of length `n_boot`.
#'
#' @export
bootstrap_regs <- function(plm, partialIDparam, n_boot) {
  parallel_type <- if (.Platform$OS.type == "windows") "snow" else "multicore"
  boot::boot(
    data       = plm$data,
    statistic  = boot_funk,
    R          = n_boot,
    parallel   = parallel_type,
    ncpus      = parallel::detectCores(logical = TRUE),
    plm        = plm,
    partialIDparam = partialIDparam
  )$t
}


# ---- placeboLM_point_estimate ------------------------------------------------

#' Compute a single point estimate with bootstrap confidence interval
#'
#' @description
#' Returns the PlaceboLM estimate and a nonparametric bootstrap confidence
#' interval for a specific choice of the partial-identification parameters.
#'
#' @param plm A `placeboLM` object.
#' @param partialIDparam Named list of partial-identification parameter values.
#' @param bootstrap Logical. Whether to compute a bootstrap SE and CI.
#'   Defaults to `TRUE`.
#' @param n_boot Positive integer. Number of bootstrap replicates. Required
#'   when `bootstrap = TRUE`.
#' @param alpha Numeric in (0, 1). Significance level for the CI. Defaults to
#'   `0.05` (95 % CI).
#'
#' @return A one-row numeric matrix with columns `Estimate`, and (when
#'   `bootstrap = TRUE`) `Std. Error`, `CI Low`, and `CI High`.
#'
#' @export
placeboLM_point_estimate <- function(plm, partialIDparam,
                                     bootstrap = TRUE, n_boot, alpha = 0.05) {
  reg_estimates  <- estimate_regs(plm)
  point_estimate <- estimate_PLM(plm, partialIDparam, reg_estimates, "estimate")

  if (bootstrap) {
    if (missing(n_boot))
      stop("`n_boot` must be supplied when `bootstrap = TRUE`.")
    boot_results <- bootstrap_regs(plm, partialIDparam, n_boot)
    se <- stats::sd(boot_results)
    ci <- stats::quantile(boot_results, probs = c(alpha / 2, 1 - alpha / 2))
    out <- matrix(c(point_estimate, se, ci), nrow = 1)
    colnames(out) <- c("Estimate", "Std. Error", "CI Low", "CI High")
  } else {
    out <- matrix(point_estimate, nrow = 1)
    colnames(out) <- "Estimate"
  }
  out
}


# ---- placeboLM_table ---------------------------------------------------------

#' Tabulate PlaceboLM estimates across a parameter grid
#'
#' @description
#' Prints a results table combining three benchmark estimates (no unobserved
#' confounding / SOO, the difference-in-differences equivalent, and the perfect
#' placebo at *k* = 1) with estimates at a grid of user-specified parameter
#' percentiles.
#'
#' @param plm A `placeboLM` object.
#' @param n_boot Positive integer. Number of bootstrap replicates for each
#'   estimate.
#' @param ptiles Numeric vector of percentile values in \[0, 1\] used to select
#'   grid values from each parameter range. Pass `NA` to show only the three
#'   benchmark estimates. Defaults to `c(0, 0.5, 1)`.
#' @param alpha Numeric. Significance level for bootstrap CIs. Defaults to
#'   `0.05`.
#' @param decimals Integer. Number of decimal places in the printed table.
#'   Defaults to `3`.
#'
#' @return Invisibly returns the printed matrix. Called for its side-effect of
#'   printing the table.
#'
#' @export
placeboLM_table <- function(plm, n_boot, ptiles = c(0, 0.5, 1),
                             alpha = 0.05, decimals = 3) {
  # ---- benchmark estimates (SOO, DID, perfect placebo k=1) -------------------
  reg_estimates <- estimate_regs(plm)

  zero_param <- lapply(plm$partialIDparam_minmax, function(x) 0)

  scale_factor <- estimate_PLM(plm, zero_param, reg_estimates, "SF")
  kDID         <- 1 / scale_factor

  soo_param    <- zero_param
  did_param    <- zero_param; did_param$k <- kDID
  k1_param     <- zero_param; k1_param$k  <- 1

  soo_est <- placeboLM_point_estimate(plm, soo_param, bootstrap = TRUE,
                                      n_boot = n_boot, alpha = alpha)
  did_est <- placeboLM_point_estimate(plm, did_param, bootstrap = TRUE,
                                      n_boot = n_boot, alpha = alpha)
  k1_est  <- placeboLM_point_estimate(plm, k1_param,  bootstrap = TRUE,
                                      n_boot = n_boot, alpha = alpha)

  bench_params  <- do.call(rbind, lapply(list(soo_param, did_param, k1_param),
                                         function(p) unlist(p)))
  bench_results <- rbind(soo_est, did_est, k1_est)
  bench_combined <- round(cbind(bench_params, bench_results), decimals)
  rownames(bench_combined) <- c("No Unobserved Confounding",
                                 paste0("DID (k=", round(kDID, decimals), ")"),
                                 "Perfect Placebo (k=1)")

  # ---- parameter grid --------------------------------------------------------
  if (is.na(ptiles[1])) {
    out <- bench_combined
  } else {
    param_ranges <- plm$partialIDparam_minmax
    num_param    <- length(param_ranges)

    # Build grid of parameter values at the requested percentiles.
    # expand.grid uses list element names as column names.
    qvals      <- lapply(param_ranges,
                         function(r) unname(stats::quantile(r, probs = ptiles)))
    param_vals <- as.matrix(do.call(expand.grid, qvals))
    colnames(param_vals) <- names(param_ranges)

    n_combos     <- nrow(param_vals)
    grid_results <- matrix(0, nrow = n_combos, ncol = 4,
                           dimnames = list(NULL, c("Estimate", "Std. Error",
                                                    "CI Low", "CI High")))
    for (i in seq_len(n_combos)) {
      grid_results[i, ] <- placeboLM_point_estimate(
        plm,
        partialIDparam = as.list(param_vals[i, ]),
        bootstrap = TRUE, n_boot = n_boot, alpha = alpha
      )
    }

    grid_combined <- round(cbind(param_vals, grid_results), decimals)
    rownames(grid_combined) <- rep("Grid", n_combos)

    out <- rbind(bench_combined, grid_combined)
  }

  print(out)
  invisible(out)
}


# ---- beta_expression_convert -------------------------------------------------

#' Convert a coefficient name string to a plotmath expression
#'
#' @description
#' Parses a coefficient name of the form `coef_A_B_given_C` into the
#' mathematical notation \eqn{\beta_{A \sim B | C}} as a plotmath expression
#' for axis labels and titles. Names that do not match this pattern are
#' returned unchanged.
#'
#' @param t Character string, typically a name from `partialIDparam_minmax`.
#'
#' @return A plotmath `expression` (for matching names) or the original string.
#'
#' @export
beta_expression_convert <- function(t) {
  m <- regmatches(t, regexec("^coef_([^_]+)_([^_]+)_given_(.+)$", t))[[1]]
  if (length(m) == 4) {
    dep <- m[2]; ind <- m[3]; giv <- m[4]
    bquote(beta[.(dep) * "~" * .(ind) * "|" * .(giv)])
  } else {
    t
  }
}


# ---- placeboLM_contour_plot --------------------------------------------------

#' Contour plot of PlaceboLM estimates over a two-parameter space
#'
#' @description
#' Creates a contour plot of the PlaceboLM estimate as a function of two
#' partial-identification parameters, with benchmark reference points overlaid.
#' Requires exactly two parameters in `partialIDparam_minmax`; use
#' [placeboLM_table()] for one or three or more parameters.
#'
#' @param plm A `placeboLM` object with exactly two partial-identification
#'   parameters.
#' @param gran Positive integer. Number of grid points per dimension.
#'   Defaults to `100`.
#' @param decimals Integer. Decimal places used in legend labels. Defaults to
#'   `3`.
#'
#' @return Invisibly returns `NULL`. Called for its side-effect of drawing a
#'   plot.
#'
#' @export
placeboLM_contour_plot <- function(plm, gran = 100, decimals = 3) {
  param_ranges <- plm$partialIDparam_minmax
  num_param    <- length(param_ranges)

  if (num_param != 2) {
    warning("placeboLM_contour_plot() requires exactly 2 partial-identification ",
            "parameters; this object has ", num_param, ". ",
            "Use placeboLM_table() instead.")
    return(invisible(NULL))
  }

  reg_estimates <- estimate_regs(plm)
  zero_param    <- lapply(param_ranges, function(x) 0)
  scale_factor  <- estimate_PLM(plm, zero_param, reg_estimates, "SF")
  kDID          <- 1 / scale_factor

  did_param <- zero_param; did_param$k <- kDID
  DID_estimate    <- estimate_PLM(plm, did_param,  reg_estimates, "estimate")
  k1_param  <- zero_param; k1_param$k  <- 1
  DID_k1_estimate <- estimate_PLM(plm, k1_param,   reg_estimates, "estimate")
  SOO_estimate    <- estimate_PLM(plm, zero_param, reg_estimates, "estimate")

  # Grid of parameter values. z[i,j] = estimate at (xvals[i], yvals[j]),
  # matching the indexing convention expected by graphics::contour().
  xvals <- seq(min(param_ranges[[1]]), max(param_ranges[[1]]), length.out = gran)
  yvals <- seq(min(param_ranges[[2]]), max(param_ranges[[2]]), length.out = gran)
  z     <- matrix(0, nrow = gran, ncol = gran)
  for (i in seq_len(gran)) {
    for (j in seq_len(gran)) {
      p      <- stats::setNames(list(xvals[i], yvals[j]), names(param_ranges))
      z[i,j] <- estimate_PLM(plm, p, reg_estimates, "estimate")
    }
  }

  graphics::contour(xvals, yvals, z, method = "edge",
                    xlab = beta_expression_convert(names(param_ranges)[1]),
                    ylab = beta_expression_convert(names(param_ranges)[2]),
                    col = "black", nlevels = 20)
  graphics::contour(xvals, yvals, z, add = TRUE, levels = 0,
                    col = "red", lty = 1, lwd = 2, labels = "0", method = "edge")

  graphics::points(kDID, 0, col = "darkgreen", pch = 15, cex = 1.5)
  graphics::points(1,    0, col = "blue",      pch = 17, cex = 1.5)
  graphics::points(0,    0, col = "navy",      pch = 18, cex = 1.5)

  graphics::legend(
    x = max(xvals), y = max(yvals),
    legend = c(
      paste0("■ DID (k=", round(kDID, decimals),
             ") Estimate = ", round(DID_estimate, decimals)),
      paste0("▲ Perfect Placebo (k=1) Estimate = ",
             round(DID_k1_estimate, decimals)),
      paste0("◆ No Unobserved Confounding Estimate = ",
             round(SOO_estimate, decimals))
    ),
    text.col = c("darkgreen", "blue", "navy"),
    xjust = 1, bg = "white"
  )
  invisible(NULL)
}


# ---- placeboLM_line_plot -----------------------------------------------------

#' Line plot of PlaceboLM estimates along one parameter axis
#'
#' @description
#' Creates one line plot per percentile of `ptile_param`, showing the
#' PlaceboLM estimate (with optional bootstrap CI) as a function of
#' `focus_param`. Benchmark reference points are overlaid on the panel where
#' the conditioning parameter equals its minimum value and `focus_param` is
#' `"k"`.
#'
#' @param plm A `placeboLM` object.
#' @param bootstrap Logical. Whether to draw bootstrap CI bands. Defaults to
#'   `TRUE`.
#' @param n_boot Positive integer. Bootstrap replicates per grid point.
#'   Required when `bootstrap = TRUE`.
#' @param ptiles Numeric vector. Percentiles of `ptile_param` at which to
#'   condition. Defaults to `c(0, 0.5, 1)`.
#' @param focus_param Character. Name of the parameter to vary on the x-axis.
#'   Defaults to `"k"`.
#' @param ptile_param Character. Name of the parameter to condition on
#'   (held fixed at each percentile). Defaults to `"coef_P_D_given_XZ"`.
#' @param gran Positive integer. Number of points on the x-axis per panel.
#'   Defaults to `10`.
#' @param alpha Numeric. CI significance level. Defaults to `0.05`.
#' @param decimals Integer. Decimal places in legend labels. Defaults to `3`.
#'
#' @return Invisibly returns `NULL`. Called for its side-effect of drawing
#'   plots.
#'
#' @export
placeboLM_line_plot <- function(plm, bootstrap = TRUE, n_boot = 10,
                                 ptiles = c(0, 0.5, 1),
                                 focus_param = "k",
                                 ptile_param = "coef_P_D_given_XZ",
                                 gran = 10, alpha = 0.05, decimals = 3) {
  param_ranges <- plm$partialIDparam_minmax
  num_param    <- length(param_ranges)

  if (num_param > 2) {
    warning("placeboLM_line_plot() supports at most 2 partial-identification ",
            "parameters; this object has ", num_param, ". ",
            "Use placeboLM_table() instead.")
    return(invisible(NULL))
  }

  reg_estimates <- estimate_regs(plm)
  zero_param    <- lapply(param_ranges, function(x) 0)
  scale_factor  <- estimate_PLM(plm, zero_param, reg_estimates, "SF")
  kDID          <- 1 / scale_factor

  did_param <- zero_param; did_param$k <- kDID
  DID_estimate    <- estimate_PLM(plm, did_param,  reg_estimates, "estimate")
  k1_param  <- zero_param; k1_param$k  <- 1
  DID_k1_estimate <- estimate_PLM(plm, k1_param,   reg_estimates, "estimate")
  SOO_estimate    <- estimate_PLM(plm, zero_param, reg_estimates, "estimate")

  focus_seq  <- seq(min(param_ranges[[focus_param]]),
                    max(param_ranges[[focus_param]]),
                    length.out = gran)
  ptile_vals <- unname(stats::quantile(param_ranges[[ptile_param]], probs = ptiles))

  # Build a data frame of all (focus, ptile) combinations.
  # Use a named list so expand.grid assigns correct column names.
  combos <- expand.grid(
    stats::setNames(list(focus_seq, ptile_vals), c(focus_param, ptile_param))
  )

  n_combos     <- nrow(combos)
  n_out_cols   <- if (bootstrap) 4 else 1
  out_colnames <- if (bootstrap) c("Estimate", "Std. Error", "CI Low", "CI High") else "Estimate"
  grid_results <- matrix(0, nrow = n_combos, ncol = n_out_cols,
                         dimnames = list(NULL, out_colnames))

  for (i in seq_len(n_combos)) {
    grid_results[i, ] <- placeboLM_point_estimate(
      plm,
      partialIDparam = as.list(combos[i, , drop = TRUE]),
      bootstrap = bootstrap, n_boot = n_boot, alpha = alpha
    )
  }

  all_results <- cbind(combos, grid_results)
  y_range     <- if (bootstrap)
    range(all_results[, "CI Low"], all_results[, "CI High"])
  else
    range(all_results[, "Estimate"])

  for (g in seq_along(ptiles)) {
    cond_val <- ptile_vals[g]
    gr1      <- all_results[abs(all_results[[ptile_param]] - cond_val) < 1e-10, ]

    main_title <- if (length(ptiles) > 1) {
      parse(text = paste0(deparse(beta_expression_convert(ptile_param)),
                          ' * " = " * ', cond_val))
    } else {
      NULL
    }

    plot(gr1[[focus_param]], gr1[["Estimate"]], type = "l", lwd = 2,
         ylab = "Estimate", xlab = focus_param,
         main = main_title, ylim = y_range)

    if (bootstrap) {
      graphics::polygon(
        c(gr1[[focus_param]], rev(gr1[[focus_param]])),
        c(gr1[["CI Low"]],    rev(gr1[["CI High"]])),
        col = "lightsteelblue1", border = NA
      )
      graphics::lines(gr1[[focus_param]], gr1[["CI Low"]],  col = "blue", lty = 2)
      graphics::lines(gr1[[focus_param]], gr1[["CI High"]], col = "blue", lty = 2)
    }

    graphics::abline(h = 0, col = "red",  lwd = 2)
    graphics::abline(v = 0, col = "gray", lwd = 1)
    graphics::lines(gr1[[focus_param]], gr1[["Estimate"]], lwd = 2)

    # Benchmark reference points only on the panel where the conditioning
    # variable is zero and the focus axis is k, to avoid clutter.
    if (cond_val == 0 && focus_param == "k") {
      graphics::points(kDID, DID_estimate,    col = "darkgreen", pch = 15, cex = 1.5)
      graphics::points(1,    DID_k1_estimate, col = "blue",      pch = 17, cex = 1.5)
      graphics::points(0,    SOO_estimate,    col = "navy",      pch = 18, cex = 1.5)

      x_max  <- max(param_ranges[["k"]])
      y_mid  <- mean(y_range)
      s      <- if (gr1[gr1[[focus_param]] == x_max, "Estimate"] <= 0) -1 else 1
      y_leg  <- if (s < 0) max(y_range) else min(y_range)

      graphics::legend(
        x = x_max, y = y_leg + s * 0.15 * diff(y_range),
        legend = c(
          paste0("■ DID (k=", round(kDID, decimals),
                 ") Estimate = ", round(DID_estimate, decimals)),
          paste0("▲ Perfect Placebo (k=1) Estimate = ",
                 round(DID_k1_estimate, decimals)),
          paste0("◆ No Unobserved Confounding Estimate = ",
                 round(SOO_estimate, decimals))
        ),
        text.col = c("darkgreen", "blue", "navy"),
        xjust = 1, bg = "white"
      )
    }
  }
  invisible(NULL)
}
