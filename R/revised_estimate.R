#' @export
revised_estimate <- function(type = c("placebo outcome","placebo treatment","double placebo"),
                 ...){
  type <- match.arg(type)
  switcher <- switch(type,
                     "placebo outcome" = revised_estimate.p.outcome,
                     "placebo treatment" = revised_estimate.p.treatment,
                     "double placebo" = revised_estimate.double.p)
  switcher(type,...)
}


#' @export
revised_estimate.p.outcome <- function(type,ripped,
                           gamma,
                           lambda,
                           beta.nd.pxz) {
  beta.yd.px = ripped$stats$beta.yd.px
  revised_estimate = beta.yd.px - bias(type,ripped,gamma,lambda,beta.nd.pxz)
  return(revised_estimate)
}

#' @export
revised_estimate.p.treatment <- function(type,ripped,
                             gamma,
                             lambda,
                             beta.yp.dxz) {
  beta.yd.px = ripped$stats$beta.yd.px
  revised_estimate = beta.yd.px - bias(type,ripped,gamma,lambda,beta.yp.dxz)
  return(revised_estimate)
}


#' @export
revised_estimate.double.p <- function(type,ripped,
                          beta.yp.dxz,
                          beta.nd.pxz,
                          beta.np.dxz) {
  beta.yd.px = ripped$stats$beta.yd.px
  revised_estimate = beta.yd.px - bias(type,ripped,beta.yp.dxz,beta.nd.pxz,beta.np.dxz)
  return(revised_estimate)
}
