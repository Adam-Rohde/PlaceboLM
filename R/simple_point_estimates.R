#' @export
simple_point_estimates <- function(type = c("placebo outcome","placebo treatment","double placebo"),
                             ...){
  type <- match.arg(type)
  switcher <- switch(type,
                     "placebo outcome" = simple_point_estimates.p.outcome,
                     "placebo treatment" = simple_point_estimates.p.treatment,
                     "double placebo" = simple_point_estimates.double.p)
  switcher(type,...)
}

#' @export
simple_point_estimates.p.outcome <- function(type,ripped) {
  simple_point_estimates = list(SOO = ripped$stats$beta.yd.px,
                                perfect_placebo_DID = revised_estimate(type,ripped = ripped,gamma = 1,lambda = (1/ripped$partialID_stats$scale_factor)^2,beta.nd.pxz=0),
                                perfect_placebo_DID_lambda1 = revised_estimate(type,ripped = ripped,gamma = 1,lambda = 1,beta.nd.pxz=0))
  return(simple_point_estimates)
}

#' @export
simple_point_estimates.p.treatment <- function(type,ripped) {
  simple_point_estimates = list(SOO = ripped$stats$beta.yd.px,
                                perfect_placebo_DID = revised_estimate(type,ripped = ripped,gamma = 1,lambda = (1/ripped$partialID_stats$scale_factor)^2,beta.yp.dxz=0),
                                perfect_placebo_DID_lambda1 = revised_estimate(type,ripped = ripped,gamma = 1,lambda = 1,beta.yp.dxz=0))
  return(simple_point_estimates)
}

#' @export
simple_point_estimates.double.p <- function(type,ripped) {
  simple_point_estimates = list(SOO = ripped$stats$beta.yd.px,
                                perfect_placebo = revised_estimate(type,ripped = ripped,beta.yp.dxz=0,beta.nd.pxz=0,beta.np.dxz=0))
  return(simple_point_estimates)
}
