#' @export
simple_point_estimates <- function(ripper, #type = c("placebo outcome","placebo treatment","double placebo"),
                             ...){
  #type <- match.arg(type)
  switcher <- switch(ripper$info$type,
                     "placebo outcome" = simple_point_estimates.p.outcome,
                     "placebo treatment" = simple_point_estimates.p.treatment,
                     "double placebo" = simple_point_estimates.double.p)
  switcher(ripper,...)
}

#' @export
simple_point_estimates.p.outcome <- function(ripper) {
  simple_point_estimates = list(SOO = ripper$stats$beta.yd.px,
                                perfect_placebo_DID = revised_estimate(ripper = ripper,gamma = 1,lambda = (1/ripper$partialID_stats$scale_factor)^2,beta.nd.pxz=0),
                                perfect_placebo_DID_lambda1 = revised_estimate(ripper = ripper,gamma = 1,lambda = 1,beta.nd.pxz=0))
  return(simple_point_estimates)
}

#' @export
simple_point_estimates.p.treatment <- function(ripper) {
  simple_point_estimates = list(SOO = ripper$stats$beta.yd.px,
                                perfect_placebo_DID = revised_estimate(ripper = ripper,gamma = 1,lambda = (1/ripper$partialID_stats$scale_factor)^2,beta.yp.dxz=0),
                                perfect_placebo_DID_lambda1 = revised_estimate(ripper = ripper,gamma = 1,lambda = 1,beta.yp.dxz=0))
  return(simple_point_estimates)
}

#' @export
simple_point_estimates.double.p <- function(ripper) {
  simple_point_estimates = list(SOO = ripper$stats$beta.yd.px,
                                perfect_placebo = revised_estimate(ripper = ripper,beta.yp.dxz=0,beta.nd.pxz=0,beta.np.dxz=0))
  return(simple_point_estimates)
}
