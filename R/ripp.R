#' Partial identification with imperfect placebos
#'
#' @description
#' This function performs partial identification (or sensitivity analysis) of causal
#' effects leveraging information about imperfect placebo outcomes and/or treatments.
#' \code{Y} is the actual outcome.
#' \code{N} is the placebo outcome.
#' \code{D} is the treatment.
#' \code{P} is the placebo treatment.
#' \code{X} contains observed covariates.
#' Users need to supply a \code{\link{lm}} object of the form \code{lm(Y~D+P+X)}.
#' When a placebo outcome is available, users need to supply a \code{\link{lm}} object of the form \code{lm(N~D+P+X)}.
#' \code{P} is a placebo treatment that is included in the regression for \code{Y} when a placebo treatment is available.
#' When both a placebo outcome and a placebo treatment are available, \code{P} is also included in the regression for \code{N}.
#' \code{ripp} returns an object of class \code{ripp} that contains key metrics for partial identification.
#'
#' @param type "placebo outcome", "placebo treatment", or "double placebo"
#' @param ... arguments passed to other methods.
#' \code{lm.y.dpx} is a \code{\link{lm}} with the actual outcome regression.
#' \code{lm.n.dpx} is a \code{\link{lm}} with the placebo outcome regression, when a placebo outcome, \code{N}, is available.
#' \code{treatment} is the name of the actual treatment.
#' \code{placebo_treatment} is the name of the actual treatment.
#' @return \code{ripp}, a list, that includes
#' (1) \code{info} containing (a) the \code{lm} formulas for the actual outcome and placebo outcome regressions and (b) the treatment andplacebo treatment names.
#' (2) \code{stats} containing the following items from actual outcome and placebo outcome regressions: (a) the coefficients \code{beta.yd.px, beta.yp.dx, beta.nd.px, beta.np.dx}
#' (b) the standard errors \code{se.yd.px, se.yp.dx, se.nd.px, se.np.dx} and (c) the degrees of freedom \code{df.yd.px, df.yp.dx, df.nd.px, df.np.dx}.
#' @export
ripp <- function(type = c("placebo outcome","placebo treatment","double placebo"),
                            ...){
  type <- match.arg(type)
  switcher <- switch(type,
                     "placebo outcome" = ripp.p.outcome,
                     "placebo treatment" = ripp.p.treatment,
                     "double placebo" = ripp.double.p)
  switcher(type,...)
}

#' Partial identification with imperfect placebo outcome
#' @param lm.y.dpx \code{lm(Y~D+P+X)} object.
#' @param lm.n.dpx \code{lm(N~D+P+X)} object.
#' @param treatment The name of the actual treatment.
#' @rdname ripp
#' @export
ripp.p.outcome <- function(type,
                           lm.y.dpx,
                           lm.n.dpx,
                           treatment){
  collect <- list()

  collect$info <- list(type = type,
                       formula.y.dpx = stats::formula(lm.y.dpx),
                       formula.n.dpx = stats::formula(lm.n.dpx),
                       treatment = treatment)


  coefs.y.dpx <- stats::coef(summary(lm.y.dpx))
  coefs.n.dpx <- stats::coef(summary(lm.n.dpx))

  collect$stats <- list(beta.yd.px = coefs.y.dpx[treatment,"Estimate"],
                        se.yd.px = coefs.y.dpx[treatment,"Std. Error"],
                        df.yd.px = lm.y.dpx$df.residual,
                        beta.nd.px = coefs.n.dpx[treatment,"Estimate"],
                        se.nd.px = coefs.n.dpx[treatment,"Std. Error"],
                        df.nd.px = lm.n.dpx$df.residual,
                        scale_factor = (coefs.y.dpx[treatment,"Std. Error"]/coefs.n.dpx[treatment,"Std. Error"])*sqrt(lm.y.dpx$df.residual/lm.n.dpx$df.residual))

  class(collect) <- "ripp"

  return(collect)
}

#' Partial identification with imperfect placebo treatment
#' @param lm.y.dpx \code{lm(Y~D+P+X)} object.
#' @param treatment The name of the actual treatment.
#' @param placebo_treatment The name of the placebo treatment.
#' @rdname ripp
#' @export
ripp.p.treatment <- function(type,
                             lm.y.dpx,
                             treatment,
                             placebo_treatment){
  collect <- list()

  collect$info <- list(type = type,
                       formula.y.dpx = stats::formula(lm.y.dpx),
                       treatment = treatment,
                       placebo_treatment = placebo_treatment)


  coefs.y.dpx <- stats::coef(summary(lm.y.dpx))

  collect$stats <- list(beta.yd.px = coefs.y.dpx[treatment,"Estimate"],
                        se.yd.px = coefs.y.dpx[treatment,"Std. Error"],
                        df.yd.px = lm.y.dpx$df.residual,
                        beta.yp.dx = coefs.y.dpx[placebo_treatment,"Estimate"],
                        se.yp.dx = coefs.y.dpx[placebo_treatment,"Std. Error"],
                        df.yp.dx = lm.y.dpx$df.residual,
                        scale_factor = (coefs.y.dpx[treatment,"Std. Error"]/coefs.y.dpx[placebo_treatment,"Std. Error"])*sqrt(lm.y.dpx$df.residual/lm.y.dpx$df.residual))

  class(collect) <- "ripp"

  return(collect)
}


#' Partial identification with imperfect placebo outcome and placebo treatment
#' @param lm.y.dpx \code{lm(Y~D+P+X)} object.
#' @param lm.n.dpx \code{lm(N~D+P+X)} object.
#' @param treatment The name of the actual treatment.
#' @param placebo_treatment The name of the placebo treatment.
#' @rdname ripp
#' @export
ripp.double.p <- function(type,
                          lm.y.dpx,
                          lm.n.dpx,
                          treatment,
                          placebo_treatment){
  collect <- list()

  collect$info <- list(type = type,
                       formula.y.dpx = stats::formula(lm.y.dpx),
                       formula.n.dpx = stats::formula(lm.n.dpx),
                       treatment = treatment,
                       placebo_treatment = placebo_treatment)


  coefs.y.dpx <- stats::coef(summary(lm.y.dpx))
  coefs.n.dpx <- stats::coef(summary(lm.n.dpx))

  collect$stats <- list(beta.yd.px = coefs.y.dpx[treatment,"Estimate"],
                        se.yd.px = coefs.y.dpx[treatment,"Std. Error"],
                        df.yd.px = lm.y.dpx$df.residual,

                        beta.yp.dx = coefs.y.dpx[placebo_treatment,"Estimate"],
                        se.yp.dx = coefs.y.dpx[placebo_treatment,"Std. Error"],
                        df.yp.dx = lm.y.dpx$df.residual,

                        beta.nd.px = coefs.n.dpx[treatment,"Estimate"],
                        se.nd.px = coefs.n.dpx[treatment,"Std. Error"],
                        df.nd.px = lm.n.dpx$df.residual,

                        beta.np.dx = coefs.n.dpx[placebo_treatment,"Estimate"],
                        se.np.dx = coefs.n.dpx[placebo_treatment,"Std. Error"],
                        df.np.dx = lm.n.dpx$df.residual
                        )

  class(collect) <- "ripp"

  return(collect)
}

