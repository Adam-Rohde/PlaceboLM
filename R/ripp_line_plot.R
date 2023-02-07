#' @export
ripp_line_plot <- function(ripper,
                              ...){
  switcher <- switch(ripper$info$type,
                     "placebo outcome" = ripp_line_plot.p.outcome,
                     "placebo treatment" = ripp_line_plot.p.treatment,
                     "double placebo" = ripp_line_plot.double.p)
  switcher(ripper,...)
}

ripp_line_plot.p.outcome = function(ripper,beta.nd.pxz=0){

  beta.yd.px = ripper$stats$beta.yd.px
  beta.nd.px = ripper$stats$beta.nd.px
  scale_factor = ripper$stats$scale_factor

  kDID = (1/scale_factor)^2
  mDID = sqrt(1)*scale_factor

  revised_estimate_k1 = revised_estimate.p.outcome(ripper_stats = ripper$stats,gamma = 1,lambda = 1,beta.nd.pxz=0)
  revised_estimate_did = revised_estimate.p.outcome(ripper_stats = ripper$stats,gamma = 1,lambda = (1/scale_factor)^2,beta.nd.pxz=0)

  lambda_vals = seq(from=0,to=2,by=0.01)

  l_lambda_vals = length(lambda_vals)

  revised_estimates_p = matrix(0,nrow = l_lambda_vals, ncol = 1)
  revised_estimates_m = matrix(0,nrow = l_lambda_vals, ncol = 1)

  for(i in 1:l_lambda_vals){
      revised_estimates_p[i,1] = revised_estimate.p.outcome(ripper_stats = ripper$stats,gamma =  1,lambda = lambda_vals[i],beta.nd.pxz=beta.nd.pxz)
      revised_estimates_m[i,1] = revised_estimate.p.outcome(ripper_stats = ripper$stats,gamma = -1,lambda = lambda_vals[i],beta.nd.pxz=beta.nd.pxz)
  }

  revised_estimates = c(revised_estimates_m[2:l_lambda_vals,1],revised_estimates_p[,1])
  lambda_val_double = c(-1*lambda_vals[2:l_lambda_vals],lambda_vals)

  revised_estimates = revised_estimates[order(lambda_val_double)]
  lambda_val_double = lambda_val_double[order(lambda_val_double)]


  plot(x = lambda_val_double, y = revised_estimates, type = "l",lwd=2,
       ylab = expression("Revised Estimate of "~beta[Y %~% D %.% list(P,X,Z)]),
       xlab = expression(gamma~" x "~ sqrt(lambda) == ~"\U00B1"~sqrt(R[Y %~% Z %.% list(D,P,X)]^2 %/% R[N %~% Z %.% list(D,P,X)]^2)),
       ylim = c(
         min(revised_estimates),
         max(revised_estimates)
       ))

  graphics::abline(h=0,col="red",lwd=2)
  graphics::abline(v=0,col="gray",lwd=1)
  # cs = terrain.colors(l_R_vals)
  # for(j in 1:l_R_vals){
  #   graphics::lines(x = k_val_double, y = revised_CIs[,j],col=cs[j],lty = j)
  #   graphics::lines(x = k_val_double, y = revised_CIs[,j+l_R_vals],col=cs[j],lty = j)
  # }
  graphics::points(x=kDID,y=revised_estimate_did,col="darkgreen",pch=18,cex=1.5)
  graphics::points(x=1,y=revised_estimate_k1,col="blue",pch=18,cex=1.5)
  graphics::points(x=0,y=beta.yd.px,col="navy",pch=18,cex=1.5)

  max_lambda = max(lambda_val_double)
  max_b = max(revised_estimates)

  graphics::text(paste0("DID Estimate = ",round(revised_estimate_did,3),", k = ",round(kDID,3)),
       x=max_lambda,
       y=1.0*max_b,col="darkgreen",adj=1)
  graphics::text(paste0("DID (k=1) Estimate = ",round(revised_estimate_k1,3),", m = ",round(mDID,3)),
       x=max_lambda,
       y=0.9*max_b,col="blue",adj=1)
  graphics::text(paste0("SOO Estimate = ",round(beta.yd.px,3)),
       x=max_lambda,
       y=0.8*max_b,col="navy",adj=1)

}


ripp_line_plot.p.treatment = function(ripper,beta.yp.dxz=0){

  beta.yd.px = ripper$stats$beta.yd.px
  beta.yp.dx = ripper$stats$beta.yp.dx
  scale_factor = ripper$stats$scale_factor

  kDID = (1/scale_factor)^2
  mDID = sqrt(1)*scale_factor

  revised_estimate_k1 = revised_estimate.p.treatment(ripper_stats = ripper$stats,gamma = 1,lambda = 1,beta.yp.dxz=0)
  revised_estimate_did = revised_estimate.p.treatment(ripper_stats = ripper$stats,gamma = 1,lambda = (1/scale_factor)^2,beta.yp.dxz=0)

  lambda_vals = seq(from=0,to=2,by=0.01)

  l_lambda_vals = length(lambda_vals)

  revised_estimates_p = matrix(0,nrow = l_lambda_vals, ncol = 1)
  revised_estimates_m = matrix(0,nrow = l_lambda_vals, ncol = 1)

  for(i in 1:l_lambda_vals){
      revised_estimates_p[i,1] = revised_estimate.p.treatment(ripper_stats = ripper$stats,gamma =  1,lambda = lambda_vals[i],beta.yp.dxz=beta.yp.dxz)
      revised_estimates_m[i,1] = revised_estimate.p.treatment(ripper_stats = ripper$stats,gamma = -1,lambda = lambda_vals[i],beta.yp.dxz=beta.yp.dxz)
  }

  revised_estimates = c(revised_estimates_m[2:l_lambda_vals,1],revised_estimates_p[,1])
  lambda_val_double = c(-1*lambda_vals[2:l_lambda_vals],lambda_vals)

  revised_estimates = revised_estimates[order(lambda_val_double)]
  lambda_val_double = lambda_val_double[order(lambda_val_double)]


  plot(x = lambda_val_double, y = revised_estimates, type = "l",lwd=2,
       ylab = expression("Revised Estimate of "~beta[Y %~% D %.% list(P,X,Z)]),
       xlab = expression(gamma~" x "~ sqrt(lambda) == ~"\U00B1"~sqrt(R[D %~% Z %.% list(P,X)]^2 %/% R[P %~% Z %.% list(D,X)]^2)),
       ylim = c(
         min(revised_estimates),
         max(revised_estimates)
       ))

  graphics::abline(h=0,col="red",lwd=2)
  graphics::abline(v=0,col="gray",lwd=1)
  # cs = terrain.colors(l_R_vals)
  # for(j in 1:l_R_vals){
  #   lines(x = k_val_double, y = revised_CIs[,j],col=cs[j],lty = j)
  #   lines(x = k_val_double, y = revised_CIs[,j+l_R_vals],col=cs[j],lty = j)
  # }
  graphics::points(x=kDID,y=revised_estimate_did,col="darkgreen",pch=18,cex=1.5)
  graphics::points(x=1,y=revised_estimate_k1,col="blue",pch=18,cex=1.5)
  graphics::points(x=0,y=beta.yd.px,col="navy",pch=18,cex=1.5)

  max_lambda = max(lambda_val_double)
  max_b = max(revised_estimates)

  graphics::text(paste0("DID Estimate = ",round(revised_estimate_did,3),", k = ",round(kDID,3)),
       x=max_lambda,
       y=1.0*max_b,col="darkgreen",adj=1)
  graphics::text(paste0("DID (k=1) Estimate = ",round(revised_estimate_k1,3),", m = ",round(mDID,3)),
       x=max_lambda,
       y=0.9*max_b,col="blue",adj=1)
  graphics::text(paste0("SOO Estimate = ",round(beta.yd.px,3)),
       x=max_lambda,
       y=0.8*max_b,col="navy",adj=1)

}

ripp_line_plot.double.p = function(ripper){
  print("Plots for double placebos not implemneted yet.")
}
