# S3 methods -------------------------------------------------------------------

#' @export
print.placebo_lm <- function(x, ...) {
  cat("Placebo partial identification (placebo_lm)\n\n")
  cat("Structure:  ", x$spec$label, "  [", x$spec$paper_ref, "]\n", sep = "")
  cat("Outcome:    ", x$vars$Y, "\n", sep = "")
  cat("Treatment:  ", x$vars$D, "\n", sep = "")
  cat("Placebo:    ", x$vars$P, "\n", sep = "")
  cat("Covariates: ",
      if (length(x$vars$X)) paste(x$vars$X, collapse = ", ") else "(none)",
      "\n", sep = "")
  cat("Rows used:  ", nrow(x$data), "\n\n", sep = "")

  cat("Regressions:\n")
  for (nm in names(x$formulas)) {
    cat("  ", format(nm, width = 7), " ",
        paste(deparse(x$formulas[[nm]]), collapse = " "), "\n", sep = "")
  }

  cat("\nEstimated quantities:\n")
  cat("  target coefficient (", x$coefs$target$name, "): ",
      signif(x$coefs$target$estimate, 5), "\n", sep = "")
  cat("  sensitivity coefficient (", x$coefs$sens$name, "): ",
      signif(x$coefs$sens$estimate, 5), "\n", sep = "")
  cat("  scale factor SF: ", signif(x$SF, 5), "   (m = k * SF)\n", sep = "")

  cat("\nSensitivity parameter for this structure: ", x$spec$sens_param,
      "\n  (zero means a perfect placebo)\n", sep = "")

  cat("\nBenchmarks:\n")
  bm <- plm_benchmarks(x)
  print(format(bm, digits = 5), row.names = FALSE)
  invisible(x)
}


#' Summarise a placebo_lm fit
#'
#' @description
#' Reports the fitted quantities, the benchmark estimates, and the tipping
#' point -- the value of relative confounding at which the adjusted estimate
#' changes sign.
#'
#' @param object A `placebo_lm` object.
#' @param ... Ignored.
#'
#' @return An object of class `summary.placebo_lm`, printed for its side effect.
#'
#' @export
summary.placebo_lm <- function(object, ...) {
  out <- list(
    fit        = object,
    benchmarks = plm_benchmarks(object),
    tipping    = tryCatch(plm_solve(object, target = 0),
                          error = function(e) NULL)
  )
  class(out) <- "summary.placebo_lm"
  out
}


#' @export
print.summary.placebo_lm <- function(x, ...) {
  print(x$fit)
  if (!is.null(x$tipping)) {
    cat("\nTipping point (adjusted estimate crosses zero):\n")
    cat("  k = ", signif(x$tipping$k, 5),
        "   m = ", signif(x$tipping$m, 5), "\n", sep = "")
    cat("  Any argument that the effect is non-zero must defend ",
        if (x$tipping$k > 0) "k < " else "k > ",
        signif(x$tipping$k, 4), ".\n", sep = "")
  }
  invisible(x)
}


#' @export
plot.placebo_lm <- function(x, type = c("line", "contour"), ...) {
  type <- match.arg(type)
  switch(type,
         line    = plm_line_plot(x, ...),
         contour = plm_contour_plot(x, ...))
}


#' @export
as.data.frame.placebo_lm <- function(x, ..., k = seq(0, 2, length.out = 21),
                                     imperfection = 0, n_boot = 0) {
  plm_grid(x, k = k, imperfection = imperfection, n_boot = n_boot)
}


#' @export
coef.placebo_lm <- function(object, ...) {
  c(target = object$coefs$target$estimate,
    sens   = object$coefs$sens$estimate,
    SF     = object$SF)
}
