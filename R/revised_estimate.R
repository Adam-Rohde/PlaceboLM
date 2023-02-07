#' @export
revised_estimate <- function(ripper,
                 ...){
  #type <- match.arg(type)
  switcher <- switch(ripper$info$type,
                     "placebo outcome" = revised_estimate.p.outcome,
                     "placebo treatment" = revised_estimate.p.treatment,
                     "double placebo" = revised_estimate.double.p)
  switcher(ripper,...)
}

#' @export
revised_estimate.p.outcome <- function(ripper,
                           gamma,
                           lambda,
                           beta.nd.pxz) {
  beta.yd.px = ripper$stats$beta.yd.px
  revised_estimate = beta.yd.px - bias(ripper,gamma,lambda,beta.nd.pxz)
  return(revised_estimate)
}

#' @export
revised_estimate.p.treatment <- function(ripper,
                             gamma,
                             lambda,
                             beta.yp.dxz) {
  beta.yd.px = ripper$stats$beta.yd.px
  revised_estimate = beta.yd.px - bias(ripper,gamma,lambda,beta.yp.dxz)
  return(revised_estimate)
}

#' @export
revised_estimate.double.p <- function(ripper,
                          beta.yp.dxz,
                          beta.nd.pxz,
                          beta.np.dxz) {
  beta.yd.px = ripper$stats$beta.yd.px
  revised_estimate = beta.yd.px - bias(ripper,beta.yp.dxz,beta.nd.pxz,beta.np.dxz)
  return(revised_estimate)
}
