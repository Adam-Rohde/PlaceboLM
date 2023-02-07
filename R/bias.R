#' @export
bias <- function(type = c("placebo outcome","placebo treatment","double placebo"),
                 ...){
  type <- match.arg(type)
  switcher <- switch(type,
                     "placebo outcome" = bias.p.outcome,
                     "placebo treatment" = bias.p.treatment,
                     "double placebo" = bias.double.p)
  switcher(...)
}

#' @export
bias.p.outcome <- function(ripped,
                          gamma,
                          lambda,
                          beta.nd.pxz) {
  beta.nd.px = ripped$stats$beta.nd.px
  scale_factor = ripped$partialID_stats$scale_factor
  bias = gamma*sqrt(lambda)*(beta.nd.px - beta.nd.pxz)*scale_factor
  return(bias)
}

#' @export
bias.p.treatment <- function(ripped,
                           gamma,
                           lambda,
                           beta.yp.dxz) {
  beta.yp.dx = ripped$stats$beta.yp.dx
  scale_factor = ripped$partialID_stats$scale_factor
  bias = gamma*sqrt(lambda)*(beta.yp.dx - beta.yp.dxz)*scale_factor
  return(bias)
}

#' @export
bias.double.p <- function(ripped,
                          beta.yp.dxz,
                          beta.nd.pxz,
                          beta.np.dxz) {
  beta.yp.dx = ripped$stats$beta.yp.dx
  beta.nd.px = ripped$stats$beta.nd.px
  beta.np.dx = ripped$stats$beta.np.dx
  bias = ((beta.yp.dx - beta.yp.dxz)*(beta.nd.px - beta.nd.pxz))/(beta.np.dx - beta.np.dxz)
  return(bias)
}
