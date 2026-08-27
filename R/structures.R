# Structure registry -----------------------------------------------------------
#
# One entry per causal structure supported by the package, transcribing Tables 2
# and 3 of Rohde and Hazlett. Those tables have columns
#
#   Placebo Type | Short Regression(s) | Parameters | Target Coefficient Expression
#
# and this list is a direct encoding of them. Nothing else in the package
# hard-codes a structure: the estimator, the plots, and the printing methods all
# read from here.
#
# Each entry supplies:
#   label        human-readable name, as used in the paper
#   paper_ref    which row(s) of Tables 2-3 it encodes
#   describe     one-line statement of the assumed causal structure
#   regressions  function(v) -> named list of formulas ("target" and, when the
#                sensitivity coefficient comes from a second regression, "sens")
#   target_coef  function(v) -> list(reg=, coef=) locating the coefficient being
#                partially identified
#   sens_coef    function(v) -> list(reg=, coef=) locating the coefficient whose
#                omitted-variable bias is being compared against
#   sens_param   name of the sensitivity parameter (the "placebo imperfection"),
#                in the paper's beta_A_B_given_C notation
#   sens_expr    function(v) -> plotmath expression for axis labels
#
# `v` is the variable map: list(Y=, D=, P=, X=).


#' Supported placebo causal structures
#'
#' @description
#' The registry of causal structures supported by [placebo_lm()], transcribing
#' Tables 2 and 3 of Rohde and Hazlett. Use [plm_structure_table()] for a
#' human-readable summary.
#'
#' @format A named list. See the package source for the fields of each entry.
#' @keywords internal
plm_structures <- list(

  # --- Table 2[a], [b] -------------------------------------------------------
  placebo_outcome = list(
    label     = "Placebo Outcome",
    paper_ref = "Table 2[a], [b]",
    describe  = paste(
      "P is a placebo outcome: it shares confounders with Y but is not caused",
      "by D (perfect placebo) or is caused by D only weakly (imperfect)."
    ),
    regressions = function(v) list(
      target = .plm_formula(v$Y, v$D, v$X),
      sens   = .plm_formula(v$P, v$D, v$X)
    ),
    target_coef = function(v) list(reg = "target", coef = v$D),
    sens_coef   = function(v) list(reg = "sens",   coef = v$D),
    sens_param  = "beta_P_D_given_ZX",
    sens_expr   = function(v) bquote(beta[.(v$P) * "~" * .(v$D) * "|Z,X"])
  ),

  # --- Table 2[a], [c] -------------------------------------------------------
  placebo_treatment = list(
    label     = "Placebo Treatment",
    paper_ref = "Table 2[a], [c]",
    describe  = paste(
      "P is a placebo treatment: it shares confounders with D but does not",
      "cause Y (perfect placebo) or causes Y only weakly (imperfect).",
      "Requires that P is not a descendant of D and D is not a descendant of P."
    ),
    regressions = function(v) list(
      target = .plm_formula(v$Y, c(v$D, v$P), v$X)
    ),
    target_coef = function(v) list(reg = "target", coef = v$D),
    sens_coef   = function(v) list(reg = "target", coef = v$P),
    sens_param  = "beta_Y_P_given_DZX",
    sens_expr   = function(v) bquote(beta[.(v$Y) * "~" * .(v$P) * "|" * .(v$D) * ",Z,X"])
  ),

  # --- Table 2[c] ------------------------------------------------------------
  observed_confounder_1 = list(
    label     = "Observed Confounder 1",
    paper_ref = "Table 2[c]",
    describe  = paste(
      "P causes Y and shares confounders with D. Read as a placebo outcome,",
      "with P included in the outcome regression so that it blocks the",
      "non-causal path between D and Y."
    ),
    regressions = function(v) list(
      target = .plm_formula(v$Y, c(v$D, v$P), v$X),
      sens   = .plm_formula(v$P, v$D, v$X)
    ),
    target_coef = function(v) list(reg = "target", coef = v$D),
    sens_coef   = function(v) list(reg = "sens",   coef = v$D),
    sens_param  = "beta_P_D_given_ZX",
    sens_expr   = function(v) bquote(beta[.(v$P) * "~" * .(v$D) * "|Z,X"])
  ),

  # --- Table 3[e], [f] -------------------------------------------------------
  observed_confounder_2 = list(
    label     = "Observed Confounder 2",
    paper_ref = "Table 3[e], [f]",
    describe  = paste(
      "P causes D (P is an observed confounder of the D-Y relationship).",
      "The sensitivity parameter is the residual P-D association."
    ),
    regressions = function(v) list(
      target = .plm_formula(v$Y, c(v$D, v$P), v$X),
      sens   = .plm_formula(v$D, v$P, v$X)
    ),
    target_coef = function(v) list(reg = "target", coef = v$D),
    sens_coef   = function(v) list(reg = "sens",   coef = v$P),
    sens_param  = "beta_D_P_given_ZX",
    sens_expr   = function(v) bquote(beta[.(v$D) * "~" * .(v$P) * "|Z,X"])
  ),

  # --- Table 3[g], [h] -------------------------------------------------------
  post_outcome = list(
    label     = "Post-Outcome",
    paper_ref = "Table 3[g], [h]",
    describe  = paste(
      "P is a descendant of Y (reverse causation). The sensitivity parameter",
      "is the residual Y-P association."
    ),
    regressions = function(v) list(
      target = .plm_formula(v$Y, v$D, v$X),
      sens   = .plm_formula(v$P, c(v$Y, v$D), v$X)
    ),
    target_coef = function(v) list(reg = "target", coef = v$D),
    sens_coef   = function(v) list(reg = "sens",   coef = v$Y),
    sens_param  = "beta_P_Y_given_DZX",
    sens_expr   = function(v) bquote(beta[.(v$P) * "~" * .(v$Y) * "|" * .(v$D) * ",Z,X"])
  )
)


# Structures the paper explicitly declines to recommend. Naming one of these
# produces an informative error rather than a silently-computed number.
.plm_refused <- list(
  mediator = paste0(
    "The mediator structure (D -> P -> Y) is not supported.\n",
    "Rohde and Hazlett state that when 'the causal structure is that of a ",
    "mediator\n(i.e., there exists a path D -> P -> Y in addition to D -> Y), ",
    "the placebo\napproach becomes very complicated, and we do not recommend ",
    "our approach.'\n",
    "For partial identification of direct and indirect effects see Zhang and ",
    "Ding (2022);\nfor partial identification of the total effect see Cinelli ",
    "and Hazlett (2020)."
  )
)


# Build a regression formula from a response, mandatory predictors, and
# optional covariates.
#
# The formula's environment is set to baseenv() rather than the calling frame.
# Every variable is resolved from the `data` argument of lm(), so the calling
# frame is never needed -- and keeping it would both clutter the printed
# formula and retain a reference to whatever happened to be in scope at
# construction time.
.plm_formula <- function(response, predictors, covariates = NULL) {
  rhs <- c(predictors, covariates)
  if (length(rhs) == 0L) rhs <- "1"
  f <- stats::reformulate(rhs, response = response)
  environment(f) <- baseenv()
  f
}


#' Summarise the supported causal structures
#'
#' @description
#' Returns a data frame describing each causal structure that [placebo_lm()]
#' supports, with the corresponding row of Tables 2 and 3 of Rohde and Hazlett
#' and the name of the structure's sensitivity parameter.
#'
#' @return A data frame with columns `structure`, `label`, `paper_ref`,
#'   `sens_param`, and `description`.
#'
#' @examples
#' plm_structure_table()
#'
#' @export
plm_structure_table <- function() {
  data.frame(
    structure   = names(plm_structures),
    label       = vapply(plm_structures, `[[`, character(1), "label"),
    paper_ref   = vapply(plm_structures, `[[`, character(1), "paper_ref"),
    sens_param  = vapply(plm_structures, `[[`, character(1), "sens_param"),
    description = vapply(plm_structures, `[[`, character(1), "describe"),
    row.names   = NULL,
    stringsAsFactors = FALSE
  )
}
