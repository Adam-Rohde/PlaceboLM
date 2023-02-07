#' External facing bias function
#' @export
bias <- function(ripper,
                 ...){
  switcher <- switch(ripper$info$type,
                     "placebo outcome" = bias.p.outcome,
                     "placebo treatment" = bias.p.treatment,
                     "double placebo" = bias.double.p)
  switcher(ripper,...)
}

#' @export
bias.p.outcome <- function(ripper,
                          gamma,
                          lambda,
                          beta.nd.pxz) {
  beta.nd.px = ripper$stats$beta.nd.px
  scale_factor = ripper$stats$scale_factor
  bias = gamma*sqrt(lambda)*(beta.nd.px - beta.nd.pxz)*scale_factor
  return(bias)
}

#' @export
bias.p.treatment <- function(ripper,
                           gamma,
                           lambda,
                           beta.yp.dxz) {
  beta.yp.dx = ripper$stats$beta.yp.dx
  scale_factor = ripper$stats$scale_factor
  bias = gamma*sqrt(lambda)*(beta.yp.dx - beta.yp.dxz)*scale_factor
  return(bias)
}

#' @export
bias.double.p <- function(ripper,
                          beta.yp.dxz,
                          beta.nd.pxz,
                          beta.np.dxz) {
  beta.yp.dx = ripper$stats$beta.yp.dx
  beta.nd.px = ripper$stats$beta.nd.px
  beta.np.dx = ripper$stats$beta.np.dx
  bias = ((beta.yp.dx - beta.yp.dxz)*(beta.nd.px - beta.nd.pxz))/(beta.np.dx - beta.np.dxz)
  return(bias)
}
