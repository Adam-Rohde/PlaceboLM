#' @export
robustness_values <- function(ripper,
                 ...){
  switcher <- switch(ripper$info$type,
                     "placebo outcome" = robustness_values.p.outcome,
                     "placebo treatment" = robustness_values.p.treatment,
                     "double placebo" = robustness_values.double.p)
  switcher(ripper$stats,...)
}

#' @export
robustness_values.p.outcome <- function(ripper_stats,
                           gamma,
                           lambda,
                           beta.nd.pxz,
                           q) {
  beta.yd.px = ripper_stats$beta.yd.px
  beta.nd.px = ripper_stats$beta.nd.px
  scale_factor = ripper_stats$scale_factor
  effect_estimate = revised_estimate.p.outcome(ripper_stats = ripper_stats,gamma = gamma,lambda = lambda,beta.nd.pxz=beta.nd.pxz)

  RV_lambda_q = (((beta.yd.px - (1-q)*effect_estimate)/(beta.nd.px - beta.nd.pxz))*(1/scale_factor))^2
  RV_beta_q = beta.nd.px - ((beta.yd.px - (1-q)*effect_estimate)/(gamma*sqrt(lambda)))*(1/scale_factor)

  robustness_values = list(RV_lambda_q = RV_lambda_q,
                           RV_beta_q = RV_beta_q)
  return(robustness_values)
}

#' @export
robustness_values.p.treatment <- function(ripper_stats,
                             gamma,
                             lambda,
                             beta.yp.dxz,
                             q) {
  beta.yd.px = ripper_stats$beta.yd.px
  beta.yp.dx = ripper_stats$beta.yp.dx
  scale_factor = ripper_stats$scale_factor
  effect_estimate = revised_estimate.p.treatment(ripper_stats = ripper_stats,gamma = gamma,lambda = lambda,beta.yp.dxz=beta.yp.dxz)

  RV_lambda_q = (((beta.yd.px - (1-q)*effect_estimate)/(beta.yp.dx - beta.yp.dxz))*(1/scale_factor))^2
  RV_beta_q = beta.yp.dx - ((beta.yd.px - (1-q)*effect_estimate)/(gamma*sqrt(lambda)))*(1/scale_factor)

  robustness_values = list(RV_lambda_q = RV_lambda_q,
                           RV_beta_q = RV_beta_q)
  return(robustness_values)
}

#' @export
robustness_values.double.p <- function(ripper_stats,
                                      beta.yp.dxz,
                                      beta.nd.pxz,
                                      beta.np.dxz,
                                      q) {
  print("RVs for double placebo not yet implemented.")
}
