#' @export
revised_estimate <- function(ripper,
                 ...){
  #type <- match.arg(type)
  switcher <- switch(ripper$info$type,
                     "placebo outcome" = revised_estimate.p.outcome,
                     "placebo treatment" = revised_estimate.p.treatment,
                     "double placebo" = revised_estimate.double.p)
  switcher(ripper$stats,...)
}

#' @export
revised_estimate.p.outcome <- function(ripper_stats,
                           gamma,
                           lambda,
                           beta.nd.pxz) {
  beta.yd.px = ripper_stats$beta.yd.px
  revised_estimate = beta.yd.px - bias.p.outcome(ripper_stats,gamma,lambda,beta.nd.pxz)
  return(revised_estimate)
}

#' @export
revised_estimate.p.treatment <- function(ripper_stats,
                             gamma,
                             lambda,
                             beta.yp.dxz) {
  beta.yd.px = ripper_stats$beta.yd.px
  revised_estimate = beta.yd.px - bias.p.treatment(ripper_stats,gamma,lambda,beta.yp.dxz)
  return(revised_estimate)
}

#' @export
revised_estimate.double.p <- function(ripper_stats,
                          beta.yp.dxz,
                          beta.nd.pxz,
                          beta.np.dxz) {
  beta.yd.px = ripper_stats$beta.yd.px
  revised_estimate = beta.yd.px - bias.double.p(ripper_stats,beta.yp.dxz,beta.nd.pxz,beta.np.dxz)
  return(revised_estimate)
}
