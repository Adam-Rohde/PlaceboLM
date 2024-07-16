#' @export
ripp_summary <- function(ripper,
                 ...){
  switcher <- switch(ripper$info$type,
                     "placebo outcome" = ripp_summary.p.outcome,
                     "placebo treatment" = ripp_summary.p.treatment,
                     "double placebo" = ripp_summary.double.p)
  switcher(ripper,...)
}

#' @export
ripp_summary.p.outcome <- function(ripper) {
  summ = matrix(0,nrow = 1,ncol = 5)
  summ[1,1] = round(ripper$simple_point_estimates$SOO,3)
  summ[1,2] = round(ripper$simple_point_estimates$perfect_placebo_DID,3)
  summ[1,3] = round(ripper$simple_point_estimates$perfect_placebo_DID_lambda1,3)
  summ[1,4] = round(ripper$robustness_values$RV_lambda_q,3)
  summ[1,5] = round(ripper$robustness_values$RV_beta_q,3)
  row.names(summ) = "beta.yd.pxz"
  colnames(summ) = c("SOO","Perfect Placebo DID","Perfect Placebo DID lambda=1","RV lambda, q=1","RV beta, q=1")
  knitr::kable(summ)
}

#' @export
ripp_summary.p.treatment <- function(ripper) {
  summ = matrix(0,nrow = 1,ncol = 5)
  summ[1,1] = round(ripper$simple_point_estimates$SOO,3)
  summ[1,2] = round(ripper$simple_point_estimates$perfect_placebo_DID,3)
  summ[1,3] = round(ripper$simple_point_estimates$perfect_placebo_DID_lambda1,3)
  summ[1,4] = round(ripper$robustness_values$RV_lambda_q,3)
  summ[1,5] = round(ripper$robustness_values$RV_beta_q,3)
  row.names(summ) = "beta.yd.pxz"
  colnames(summ) = c("SOO","Perfect Placebo DID","Perfect Placebo DID lambda=1","RV lambda, q=1","RV beta, q=1")
  knitr::kable(summ)
}

#' @export
ripp_summary.double.p <- function(ripper) {
  summ = matrix(0,nrow = 1,ncol = 2)
  summ[1,1] = round(ripper$simple_point_estimates$SOO,3)
  summ[1,2] = round(ripper$simple_point_estimates$perfect_placebo,3)
  row.names(summ) = "beta.yd.pxz"
  colnames(summ) = c("SOO","Perfect Placebos")
  knitr::kable(summ)
}
