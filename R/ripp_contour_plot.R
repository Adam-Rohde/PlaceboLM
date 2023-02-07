#' @export
ripp_contour_plot <- function(ripper,
                         ...){
  switcher <- switch(ripper$info$type,
                     "placebo outcome" = ripp_contour_plot.p.outcome,
                     "placebo treatment" = ripp_contour_plot.p.treatment,
                     "double placebo" = ripp_contour_plot.double.p)
  switcher(ripper,...)
}

ripp_contour_plot.p.outcome = function(ripper){

  beta.yd.px = ripper$stats$beta.yd.px
  beta.nd.px = ripper$stats$beta.nd.px
  scale_factor = ripper$stats$scale_factor

  kDID = (1/scale_factor)^2
  mDID = sqrt(1)*scale_factor

  revised_estimate_k1 = revised_estimate.p.outcome(ripper_stats = ripper$stats,gamma = 1,lambda = 1,beta.nd.pxz=0)
  revised_estimate_did = revised_estimate.p.outcome(ripper_stats = ripper$stats,gamma = 1,lambda = (1/scale_factor)^2,beta.nd.pxz=0)

  lambda_vals = seq(from=0,to=2,by=0.01)
  beta_vals = seq(from=-2*abs(beta.nd.px),to=2*abs(beta.nd.px),by=abs(beta.nd.px)/200)

  l_lambda_vals = length(lambda_vals)
  l_beta_vals = length(beta_vals)

  revised_estimates_p = matrix(0,nrow = l_lambda_vals, ncol = l_beta_vals)
  revised_estimates_m = matrix(0,nrow = l_lambda_vals, ncol = l_beta_vals)

  for(i in 1:l_lambda_vals){
    for(j in 1:l_beta_vals){
      revised_estimates_p[i,j] = revised_estimate.p.outcome(ripper_stats = ripper$stats,gamma =  1,lambda = lambda_vals[i],beta.nd.pxz=beta_vals[j])
      revised_estimates_m[i,j] = revised_estimate.p.outcome(ripper_stats = ripper$stats,gamma = -1,lambda = lambda_vals[i],beta.nd.pxz=beta_vals[j])
    }
  }

  revised_estimates = rbind(revised_estimates_m[2:l_lambda_vals,],revised_estimates_p)
  lambda_val_double = c(-1*lambda_vals[2:l_lambda_vals],lambda_vals)

  revised_estimates = revised_estimates[order(lambda_val_double),]
  lambda_val_double = lambda_val_double[order(lambda_val_double)]


  graphics::contour(x=lambda_val_double,
          y=beta_vals,
          z=revised_estimates,method="edge",
          xlab=expression(gamma~" x "~ sqrt(lambda) == ~"\U00B1"~sqrt(R[Y %~% Z %.% list(D,P,X)]^2 %/% R[N %~% Z %.% list(D,P,X)]^2)),
          ylab=expression(beta[N %~% D %.% list(P,X,Z)]),col="black",nlevels=20)
  graphics::contour(x=lambda_val_double,
          y=beta_vals,
          z=revised_estimates,
          add=T,levels = 0,col = "red",lty=1,lwd = 2,labels = "0",method="edge")

  graphics::points(x=kDID,y=0,col="darkgreen",pch=18,cex=1.5)
  graphics::points(x=1,y=0,col="blue",pch=18,cex=1.5)
  graphics::points(x=0,y=0,col="navy",pch=18,cex=1.5)

  max_lambda = max(lambda_val_double)
  min_b = min(beta_vals)

  graphics::text(bquote("Revised Estimates of "~beta[Y %~% D %.% list(P,X,Z)]),
       x=0,y=max(beta_vals)*1.02,col="black")
  graphics::text(paste0("DID Estimate = ",round(revised_estimate_did,3),", k = ",round(kDID,3)),
       x=max_lambda,
       y=1.0*min_b,col="darkgreen",adj=1)
  graphics::text(paste0("DID (k=1) Estimate = ",round(revised_estimate_k1,3),", m = ",round(mDID,3)),
       x=max_lambda,
       y=0.9*min_b,col="blue",adj=1)
  graphics::text(paste0("SOO Estimate = ",round(beta.yd.px,3)),
       x=max_lambda,
       y=0.8*min_b,col="navy",adj=1)

}


ripp_contour_plot.p.treatment = function(ripper){

  beta.yd.px = ripper$stats$beta.yd.px
  beta.yp.dx = ripper$stats$beta.yp.dx
  scale_factor = ripper$stats$scale_factor

  kDID = (1/scale_factor)^2
  mDID = sqrt(1)*scale_factor

  revised_estimate_k1 = revised_estimate.p.treatment(ripper_stats = ripper$stats,gamma = 1,lambda = 1,beta.yp.dxz=0)
  revised_estimate_did = revised_estimate.p.treatment(ripper_stats = ripper$stats,gamma = 1,lambda = (1/scale_factor)^2,beta.yp.dxz=0)

  lambda_vals = seq(from=0,to=2,by=0.01)
  beta_vals = seq(from=-2*abs(beta.yp.dx),to=2*abs(beta.yp.dx),by=abs(beta.yp.dx)/200)

  l_lambda_vals = length(lambda_vals)
  l_beta_vals = length(beta_vals)

  revised_estimates_p = matrix(0,nrow = l_lambda_vals, ncol = l_beta_vals)
  revised_estimates_m = matrix(0,nrow = l_lambda_vals, ncol = l_beta_vals)

  for(i in 1:l_lambda_vals){
    for(j in 1:l_beta_vals){
      revised_estimates_p[i,j] = revised_estimate.p.treatment(ripper_stats = ripper$stats,gamma =  1,lambda = lambda_vals[i],beta.yp.dxz=beta_vals[j])
      revised_estimates_m[i,j] = revised_estimate.p.treatment(ripper_stats = ripper$stats,gamma = -1,lambda = lambda_vals[i],beta.yp.dxz=beta_vals[j])
    }
  }

  revised_estimates = rbind(revised_estimates_m[2:l_lambda_vals,],revised_estimates_p)
  lambda_val_double = c(-1*lambda_vals[2:l_lambda_vals],lambda_vals)

  revised_estimates = revised_estimates[order(lambda_val_double),]
  lambda_val_double = lambda_val_double[order(lambda_val_double)]


  graphics::contour(x=lambda_val_double,
          y=beta_vals,
          z=revised_estimates,method="edge",
          xlab=expression(gamma~" x "~ sqrt(lambda) == ~"\U00B1"~sqrt(R[D %~% Z %.% list(P,X)]^2 %/% R[P %~% Z %.% list(D,X)]^2)),
          ylab=expression(beta[Y %~% P %.% list(D,X,Z)]),col="black",nlevels=20)
  graphics::contour(x=lambda_val_double,
          y=beta_vals,
          z=revised_estimates,
          add=T,levels = 0,col = "red",lty=1,lwd = 2,labels = "0",method="edge")

  graphics::points(x=kDID,y=0,col="darkgreen",pch=18,cex=1.5)
  graphics::points(x=1,y=0,col="blue",pch=18,cex=1.5)
  graphics::points(x=0,y=0,col="navy",pch=18,cex=1.5)

  max_lambda = max(lambda_val_double)
  min_b = min(beta_vals)

  graphics::text(bquote("Revised Estimates of "~beta[Y %~% D %.% list(P,X,Z)]),
       x=0,y=max(beta_vals)*1.02,col="black")
  graphics::text(paste0("DID Estimate = ",round(revised_estimate_did,3),", k = ",round(kDID,3)),
       x=max_lambda,
       y=1.0*min_b,col="darkgreen",adj=1)
  graphics::text(paste0("DID (k=1) Estimate = ",round(revised_estimate_k1,3),", m = ",round(mDID,3)),
       x=max_lambda,
       y=0.9*min_b,col="blue",adj=1)
  graphics::text(paste0("SOO Estimate = ",round(beta.yd.px,3)),
       x=max_lambda,
       y=0.8*min_b,col="navy",adj=1)

}

ripp_contour_plot.double.p = function(ripper){
  print("Plots for double placebos not implemneted yet.")
}
