#' @export
simple_point_estimates <- function(ripper, #type = c("placebo outcome","placebo treatment","double placebo"),
                             ...){
  #type <- match.arg(type)
  switcher <- switch(ripper$info$type,
                     "placebo outcome" = simple_point_estimates.p.outcome,
                     "placebo treatment" = simple_point_estimates.p.treatment,
                     "double placebo" = simple_point_estimates.double.p)
  switcher(ripper$stats,...)
}

#' @export
simple_point_estimates.p.outcome <- function(ripper_stats) {
  simple_point_estimates = list(SOO = ripper_stats$beta.yd.px,
                                perfect_placebo_DID = revised_estimate.p.outcome(ripper_stats = ripper_stats,gamma = 1,lambda = (1/ripper_stats$scale_factor)^2,beta.nd.pxz=0),
                                perfect_placebo_DID_lambda1 = revised_estimate.p.outcome(ripper_stats = ripper_stats,gamma = 1,lambda = 1,beta.nd.pxz=0))
  return(simple_point_estimates)
}

#' @export
simple_point_estimates.p.treatment <- function(ripper_stats) {
  simple_point_estimates = list(SOO = ripper_stats$beta.yd.px,
                                perfect_placebo_DID = revised_estimate.p.treatment(ripper_stats = ripper_stats,gamma = 1,lambda = (1/ripper_stats$scale_factor)^2,beta.yp.dxz=0),
                                perfect_placebo_DID_lambda1 = revised_estimate.p.treatment(ripper_stats = ripper_stats,gamma = 1,lambda = 1,beta.yp.dxz=0))
  return(simple_point_estimates)
}

#' @export
simple_point_estimates.double.p <- function(ripper_stats) {
  simple_point_estimates = list(SOO = ripper_stats$beta.yd.px,
                                perfect_placebo = revised_estimate.double.p(ripper_stats = ripper_stats,beta.yp.dxz=0,beta.nd.pxz=0,beta.np.dxz=0))
  return(simple_point_estimates)
}
