#' Partial identification with imperfect placebos
#'
#' @description
#' This function performs partial identification (or sensitivity analysis) of causal
#' effects leveraging information about imperfect placebo outcomes and/or treatments.
#' It returns an object of class \code{ripp} that contains key metrics for partial identification.
#' @param type "outcome", "treatment", or "double"
#' @param lm.y.dpx \code{lm(Y~D+P+X)} object. \code{Y} is actual outcome.
#' @param lm.n.dpx \code{lm(N~D+P+X)} object. \code{N} is placebo outcome.
#' @param treatment \code{D} is treatment treatment.
#' @param placebo_treatment \code{P} is placebo treatment.
#' @return \code{ripp} object that includes:
#' \describe{
#' \item{xxxx}
#' }
#' @export
ripp = function(type = c("placebo outcome","placebo treatment","double placebo"),
                            ...){
  type <- match.arg(type)
  switcher <- switch(type,
                     "placebo outcome" = ripp.p.outcome,
                     "placebo treatment" = ripp.p.treatment,
                     "double placebo" = ripp.double.p)
  switcher(...)
}


ripp.p.outcome = function(lm.y.dpx,
                        lm.n.dpx,
                        treatment){
  collect = list()

  collect$info <- list(formula.y.dpx = formula(lm.y.dpx),
                       formula.n.dpx = formula(lm.n.dpx),
                       treatment = treatment)


  coefs.y.dpx = coef(summary(lm.y.dpx))
  coefs.n.dpx = coef(summary(lm.n.dpx))

  collect$stats <- list(beta.yd.px = coefs.y.dpx[treatment,"Estimate"],
                        se.yd.px = coefs.y.dpx[treatment,"Std. Error"],
                        df.yd.px = lm.y.dpx$df.residual,
                        beta.nd.px = coefs.n.dpx[treatment,"Estimate"],
                        se.nd.px = coefs.n.dpx[treatment,"Std. Error"],
                        df.nd.px = lm.n.dpx$df.residual)

  class(collect) = "ripp"

  return(collect)
}


ripp.p.treatment = function(lm.y.dpx,
                          treatment,
                          placebo_treatment){
  collect = list()

  collect$info <- list(formula.y.dpx = formula(lm.y.dpx),
                       treatment = treatment,
                       placebo_treatment = placebo_treatment)


  coefs.y.dpx = coef(summary(lm.y.dpx))

  collect$stats <- list(beta.yd.px = coefs.y.dpx[treatment,"Estimate"],
                        se.yd.px = coefs.y.dpx[treatment,"Std. Error"],
                        df.yd.px = lm.y.dpx$df.residual,
                        beta.yp.dx = coefs.y.dpx[placebo_treatment,"Estimate"],
                        se.yp.dx = coefs.y.dpx[placebo_treatment,"Std. Error"],
                        df.yp.dx = lm.y.dpx$df.residual)

  class(collect) = "ripp"

  return(collect)
}


ripp.double.p = function(lm.y.dpx,
                        lm.n.dpx,
                        treatment,
                        placebo_treatment){
  collect = list()

  collect$info <- list(formula.y.dpx = formula(lm.y.dpx),
                       formula.n.dpx = formula(lm.n.dpx),
                       treatment = treatment,
                       placebo_treatment = placebo_treatment)


  coefs.y.dpx = coef(summary(lm.y.dpx))
  coefs.n.dpx = coef(summary(lm.n.dpx))

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

  class(collect) = "ripp"

  return(collect)
}
