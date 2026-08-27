# Constructor ------------------------------------------------------------------

#' Fit a placebo partial-identification model
#'
#' @description
#' Sets up a partial-identification analysis of the effect of `treatment` on
#' `outcome`, leveraging an imperfect `placebo` variable, following Rohde and
#' Hazlett.
#'
#' The regressions implied by the assumed causal `structure` are fitted once,
#' here, and every downstream calculation ([plm_estimate()], [plm_bounds()],
#' [plm_solve()], the plots) reads from the returned object. The scale factor
#' `SF` does not depend on the sensitivity parameters and so is also computed
#' once.
#'
#' @param data A data frame containing all analysis variables.
#' @param outcome Character. Name of the outcome variable (`Y` in the paper).
#' @param treatment Character. Name of the treatment variable (`D`).
#' @param placebo Character. Name of the placebo variable (`P`). Depending on
#'   `structure` this is read as a placebo outcome, a placebo treatment, an
#'   observed confounder, or a post-outcome variable.
#' @param covariates Character vector of observed covariate names (`X`) to
#'   include in every regression. Defaults to `NULL` (intercept only).
#' @param structure Character. The assumed causal structure. One of
#'   `"placebo_outcome"`, `"placebo_treatment"`, `"observed_confounder_1"`,
#'   `"observed_confounder_2"`, or `"post_outcome"`. See
#'   [plm_structure_table()] for a description of each, and the
#'   `structures` vignette for worked examples.
#'
#' @return An object of class `"placebo_lm"`: a list with components
#'   \describe{
#'     \item{`data`}{the analysis data frame}
#'     \item{`vars`}{named list of variable roles (`Y`, `D`, `P`, `X`)}
#'     \item{`structure`}{the structure name}
#'     \item{`spec`}{the corresponding entry of the structure registry}
#'     \item{`formulas`}{the fitted regression formulas}
#'     \item{`regressions`}{the fitted `lm` objects}
#'     \item{`coefs`}{list with `target` and `sens`, each
#'       `list(estimate=, se=, df=)`}
#'     \item{`SF`}{the scale factor}
#'   }
#'
#' @references
#' Rohde, A. and Hazlett, C. Causal progress with imperfect placebo treatments
#' and outcomes.
#'
#' @examples
#' set.seed(1)
#' n <- 500
#' U <- rnorm(n); X <- rnorm(n)
#' D <- X + U + rnorm(n)
#' P <- X + U + rnorm(n)
#' Y <- D + X + U + rnorm(n)
#' dat <- data.frame(Y = Y, D = D, P = P, X = X)
#'
#' fit <- placebo_lm(dat, outcome = "Y", treatment = "D", placebo = "P",
#'                   covariates = "X", structure = "placebo_outcome")
#' fit
#'
#' @export
placebo_lm <- function(data,
                       outcome,
                       treatment,
                       placebo,
                       covariates = NULL,
                       structure  = c("placebo_outcome",
                                      "placebo_treatment",
                                      "observed_confounder_1",
                                      "observed_confounder_2",
                                      "post_outcome")) {

  # Refused structures get their own message before match.arg's generic one.
  if (length(structure) == 1L && structure %in% names(.plm_refused))
    stop(.plm_refused[[structure]], call. = FALSE)

  structure <- match.arg(structure)

  # --- validation -----------------------------------------------------------
  if (!is.data.frame(data))
    stop("`data` must be a data frame.", call. = FALSE)
  if (nrow(data) == 0L)
    stop("`data` has no rows.", call. = FALSE)

  roles <- list(outcome = outcome, treatment = treatment, placebo = placebo)
  for (nm in names(roles)) {
    val <- roles[[nm]]
    if (!is.character(val) || length(val) != 1L)
      stop("`", nm, "` must be a single variable name.", call. = FALSE)
    if (!val %in% names(data))
      stop("`", nm, "` variable '", val, "' not found in `data`.", call. = FALSE)
  }
  if (!is.null(covariates)) {
    if (!is.character(covariates))
      stop("`covariates` must be a character vector of variable names.",
           call. = FALSE)
    missing_covs <- setdiff(covariates, names(data))
    if (length(missing_covs))
      stop("Covariate(s) not found in `data`: ",
           paste(missing_covs, collapse = ", "), call. = FALSE)
  }
  if (anyDuplicated(c(outcome, treatment, placebo, covariates)))
    stop("`outcome`, `treatment`, `placebo`, and `covariates` must all name ",
         "distinct variables.", call. = FALSE)

  vars <- list(Y = outcome, D = treatment, P = placebo, X = covariates)
  spec <- plm_structures[[structure]]

  # --- fit each regression exactly once -------------------------------------
  formulas <- spec$regressions(vars)
  regressions <- lapply(formulas, function(f) stats::lm(f, data = data))

  target_loc <- spec$target_coef(vars)
  sens_loc   <- spec$sens_coef(vars)

  coefs <- list(
    target = .plm_extract(regressions, target_loc, "target"),
    sens   = .plm_extract(regressions, sens_loc,   "sensitivity")
  )

  # --- scale factor ---------------------------------------------------------
  # One generic rule reproduces every structure-specific SF in Tables 1-2 of
  # the paper. In OLS se(beta_j) = sigma_resid / sqrt(SSR_j), so
  # se(beta) * sqrt(df) = sqrt(RSS) / sqrt(SSR_j); forming the ratio makes the
  # residual-scale and residualised-regressor terms telescope into exactly the
  # products of sd(.  |  .) ratios the paper writes out row by row. Footnotes 6
  # and 9 of the paper state the two-regression cases of this identity.
  SF <- (coefs$target$se * sqrt(coefs$target$df)) /
        (coefs$sens$se   * sqrt(coefs$sens$df))

  # A degenerate *regression* is the failure to catch here, not a large scale
  # factor. A very large SF is entirely legitimate: the paper's NSW analysis
  # uses 1975 unemployment as a placebo for 1978 earnings, where SF exceeds
  # 40,000 and the analysis remains meaningful (it is precisely why one reasons
  # about k rather than m). What is not meaningful is a regression whose
  # response is fully explained by its predictors, leaving no residual
  # variation for the omitted-variable-bias argument to work with.
  for (nm in names(regressions)) {
    m <- regressions[[nm]]
    resp <- stats::model.response(stats::model.frame(m))
    denom <- stats::sd(resp)
    if (is.finite(denom) && denom > 0 &&
        stats::sigma(m) / denom < 1e-8) {
      stop("The ", nm, " regression (",
           paste(deparse(formulas[[nm]]), collapse = " "),
           ") is a near-perfect fit:\n",
           "its response has essentially no residual variation once the ",
           "predictors are included.\n",
           "Check that the placebo is not a deterministic function of the ",
           "treatment or covariates.", call. = FALSE)
    }
  }

  if (!is.finite(SF))
    stop("The scale factor is not finite (SF = ", SF, ").\n",
         "This usually means the placebo variable has (near-)zero residual ",
         "variance after\nconditioning on the treatment and covariates, so the ",
         "placebo carries no information.", call. = FALSE)

  base <- structure(
    list(
      data        = data,
      vars        = vars,
      structure   = structure,
      spec        = spec,
      formulas    = formulas,
      regressions = regressions,
      coefs       = coefs,
      SF          = SF
    ),
    class = "placebo_lm"
  )
  base
}


# Locate one coefficient (estimate, se, df) within the fitted regressions.
.plm_extract <- function(regressions, loc, what) {
  m <- regressions[[loc$reg]]
  ct <- stats::coef(summary(m))
  if (!loc$coef %in% rownames(ct))
    stop("Could not find the ", what, " coefficient '", loc$coef,
         "' in the fitted regression.\n",
         "This can happen when the variable is collinear with the covariates ",
         "and was dropped.", call. = FALSE)
  list(
    estimate = unname(ct[loc$coef, "Estimate"]),
    se       = unname(ct[loc$coef, "Std. Error"]),
    df       = m$df.residual,
    name     = loc$coef
  )
}


# Refit on a new data frame (used by the bootstrap). Reuses the already-resolved
# spec and formulas rather than re-validating.
.plm_refit <- function(fit, data) {
  regressions <- lapply(fit$formulas, function(f) stats::lm(f, data = data))
  target <- .plm_extract(regressions, fit$spec$target_coef(fit$vars), "target")
  sens   <- .plm_extract(regressions, fit$spec$sens_coef(fit$vars), "sensitivity")
  list(
    target = target,
    sens   = sens,
    SF     = (target$se * sqrt(target$df)) / (sens$se * sqrt(sens$df))
  )
}
