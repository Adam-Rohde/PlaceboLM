
#' @export
placeboLM <- function(data = "",
                      placebo_data = NULL,
                      outcome,
                      treatment,
                      placebo_outcome = "",
                      placebo_treatment = "",
                      DP = c("->","<-",""),
                      PY = c("->","<-",""),
                      observed_covariates = "",
                      partialIDparam_minmax = c(list(k = c(-2,2),coef_P_D_given_XZ = c(-2,2)))
                      ){

  # double placebo parameters: k_yd_yp k_np_nd coef_Y_D_given_PXZ coef_Y_P_given_DXZ coef_N_D_given_PXZ coef_N_P_given_DXZ
  # single placebo parameters: k coef_P_D_given_XZ coef_Y_P_given_DXZ coef_D_P_given_XZ coef_P_Y_given_DXZ

  # create a list to collect parameters for placeboLM
  collect <- list(data = data,
                  placebo_data = placebo_data,
                  dta = eval(parse(text=data)),
                  outcome = outcome,
                  treatment = treatment,
                  placebo_outcome = placebo_outcome,
                  placebo_treatment = placebo_treatment,
                  DP = DP,
                  PY = PY,
                  observed_covariates = observed_covariates,
                  partialIDparam_minmax = partialIDparam_minmax)

  # depending on inputs for placeboLM, categorize placebo type and create relevant regression formulas

  # Double Placebo: when placebo_outcome != "" & placebo_treatment != ""
  if(placebo_outcome != "" & placebo_treatment != ""){
    message(cat("Both a placebo treatment and a placebo outcome have been indicated.", "\n",
        "PlaceboLM assumes a 'double placebo' setting is desired.", "\n",
        "PlaceboLM assumes the following causal relations:", "\n",
        "    D->N", "\n",
        "    P->D", "\n",
        "    P->N", "\n",
        "    P->Y", "\n",
        "    N->Y", "\n",
        "If you wish to assume that one of these causal relations does not exist", "\n",
        "set the relavant partial identification parameter to zero."))
    collect$type = "Double Placebo"
    collect$regressions <- list(
      reg_Y_on_D_plus_P = paste0("lm(",outcome,"~",paste0(c(treatment,placebo_treatment,observed_covariates),collapse = " + ")," , data = plm$dta)"),
      reg_N_on_D_plus_P = paste0("lm(",placebo_outcome,"~",paste0(c(treatment,placebo_treatment,observed_covariates),collapse = " + ")," , data = plm$dta)")
      )

  # if no placebo is indicated, return a warning
  } else if(placebo_outcome == "" & placebo_treatment == ""){
    warning("No placebo indicated.")
  }
  else {

    # create a 'placebo' variable
    if(placebo_outcome != ""){collect$placebo = placebo_outcome} else {collect$placebo = placebo_treatment}
    if(placebo_outcome != ""){placebo = placebo_outcome} else {placebo = placebo_treatment}

    # return a warning when a cycle is specified
    if(PY=="<-" & DP=="<-"){
      warning("Values for PY and DP create a cycle.")
    }

    # Single Placebo, No Direct Relationships: when 'PY' and 'DP' are both missing
    else if(PY=="" & DP==""){
      message(cat("Placebo assumed to have no direct relationship with either treatment or outcome."))
      if(placebo_outcome != ""){
        collect$type = "Single Placebo, No Direct Relationships, Placebo Outcome"
        collect$regressions <- list(
          reg_Y_on_D = paste0("lm(",outcome,"~",paste0(c(treatment,observed_covariates),collapse = " + ")," , data = plm$dta)"),
          reg_P_on_D = paste0("lm(",placebo,"~",paste0(c(treatment,observed_covariates),collapse = " + ")," , data = plm$dta)"))
      } else if(placebo_treatment != ""){
        collect$type = "Single Placebo, No Direct Relationships, Placebo Treatment"
        collect$regressions <- list(
          reg_Y_on_D_plus_P = paste0("lm(",outcome,"~",paste0(c(treatment,placebo,observed_covariates),collapse = " + ")," , data = plm$dta)"))
      }
    }

    # Single Placebo, Treatment causes Placebo: when DP=="->" but PY==""
    else if(PY=="" & DP=="->"){
      message(cat("Placebo assumed to be directly caused by treatment."))
      collect$type = "Single Placebo, Treatment causes Placebo"
      collect$regressions <- list(
        reg_Y_on_D = paste0("lm(",outcome,"~",paste0(c(treatment,observed_covariates),collapse = " + ")," , data = plm$dta)"),
        reg_P_on_D = paste0("lm(",placebo,"~",paste0(c(treatment,observed_covariates),collapse = " + ")," , data = plm$dta)"))
    }

    # Single Placebo, Placebo causes Outcome: when PY=="->" & DP==""
    else if(PY=="->" & DP==""){
      message(cat("Placebo assumed to directly cause outcome."))
      if(placebo_treatment != ""){
        collect$type = "Single Placebo, Placebo causes Outcome, Placebo Treatment"
        collect$regressions <- list(
          reg_Y_on_D_plus_P = paste0("lm(",outcome,"~",paste0(c(treatment,placebo,observed_covariates),collapse = " + ")," , data = plm$dta)"))
      } else if(placebo_outcome != ""){
        collect$type = "Single Placebo, Placebo causes Outcome, Placebo Outcome"
        collect$regressions <- list(
          reg_Y_on_D_plus_P = paste0("lm(",outcome,"~",paste0(c(treatment,placebo,observed_covariates),collapse = " + ")," , data = plm$dta)"),
          reg_P_on_D = paste0("lm(",placebo,"~",paste0(c(treatment,observed_covariates),collapse = " + ")," , data = plm$dta)"))
      }
    }

    # Single Placebo, Placebo is Mediator: when PY=="->" & DP=="->"
    else if(PY=="->" & DP=="->"){
      message(cat("Placebo assumed to be a mediator between treatment and outcome.", "\n",
          "For partial identification of the driect or indirect effect", "\n",
          "use approaches from Zhang and Ding (2022).", "\n",
          "PlaceboLM will assume total effect is target causal contrast."))
      if(placebo_outcome != ""){
        collect$type = "Single Placebo, Placebo is Mediator, Placebo Outcome"
        collect$regressions <- list(
          reg_Y_on_D = paste0("lm(",outcome,"~",paste0(c(treatment,observed_covariates),collapse = " + ")," , data = plm$dta)"),
          reg_P_on_D = paste0("lm(",placebo,"~",paste0(c(treatment,observed_covariates),collapse = " + ")," , data = plm$dta)"))
      } else if(placebo_treatment != ""){
        collect$type = "Single Placebo, Placebo is Mediator, Placebo Treatment"
        collect$regressions <- list(
          reg_Y_on_D = paste0("lm(",outcome,"~",paste0(c(treatment,observed_covariates),collapse = " + ")," , data = plm$dta)"),
          reg_Y_on_D_plus_P = paste0("lm(",outcome,"~",paste0(c(treatment,placebo,observed_covariates),collapse = " + ")," , data = plm$dta)"))
      }
    }

    # Single Placebo, Placebo is Observed Confounder: when DP=="<-"
    else if(DP=="<-"){
      message(cat("Placebo assumed to be an observed confounder."))
      collect$type = "Single Placebo, Placebo is Observed Confounder"
      if(placebo_outcome != ""){placebo = placebo_outcome} else {placebo = placebo_treatment}
      collect$regressions <- list(
        reg_Y_on_D_plus_P = paste0("lm(",outcome,"~",paste0(c(treatment,placebo,observed_covariates),collapse = " + ")," , data = plm$dta)"),
        reg_D_on_P = paste0("lm(",treatment,"~",paste0(c(placebo,observed_covariates),collapse = " + ")," , data = plm$dta)"))
    }

    # Single Placebo, Outcome causes Placebo: when PY=="<-"
    else if(PY=="<-"){
      message(cat("Placebo assumed to be a descendant of outcome."))
      collect$type = "Single Placebo, Outcome causes Placebo"
      collect$regressions <- list(
        reg_Y_on_D = paste0("lm(",outcome,"~",paste0(c(treatment,observed_covariates),collapse = " + ")," , data = plm$dta)"),
        reg_P_on_Y_plus_D = paste0("lm(",placebo,"~",paste0(c(outcome,treatment,observed_covariates),collapse = " + ")," , data = plm$dta)"))
    }


  }
  message(cat("Placebo Type:",collect$type))
  for(i in 1:length(collect$regressions)){
    message(cat("Regression",i,":",collect$regressions[[i]]))
  }


  # return collect
  class(collect) <- "placeboLM"
  return(collect)
}






#' @export
placeboLM_table <- function(plm,n_boot,ptiles = c(0,0.5,1)){
  # this will provide a table of point estimates that cover the range of partial ID parameters given

  param_ranges = plm$partialIDparam_minmax
  num_param = length(param_ranges)

  val_matrix = matrix(0,ncol = length(ptiles),nrow = num_param)
  row.names(val_matrix) = names(param_ranges)
  colnames(val_matrix) = ptiles
  for(i in 1:num_param){
    val_matrix[i,] = stats::quantile(x = param_ranges[[i]], probs = ptiles)
    if(i==1){
      param_vals = val_matrix[i,]
    } else{
      param_vals = tidyr::crossing(param_vals,val_matrix[i,],.name_repair = "unique")
    }
  }
  param_vals = as.matrix(param_vals)
  colnames(param_vals) = names(param_ranges)

  n_param_combos = dim(param_vals)[1]
  grid_results = cbind(param_vals,matrix(0,ncol = 4,nrow = n_param_combos))
  colnames(grid_results) = c(names(param_ranges),"Estimate","Std. Error","95% CI Low","95% CI High")
  for(i in 1:n_param_combos){
    grid_results[i,(num_param+1):(num_param+4)] = placeboLM_point_estimate(plm, partialIDparam = as.list(param_vals[i,]),bootstrap = TRUE, n_boot = n_boot)
  }

  return(grid_results)

}













#' @export
placeboLM_point_estimate <- function(plm,
                               partialIDparam,
                               bootstrap = TRUE,
                               n_boot){
  # this will provide a single point estimate, SE, and CI
  # takes in plm object and partialID params

  # get regression estimates
  reg_estimates = estimate_regs(plm = plm)

  # get point estimate
  point_estimate = estimate_PLM(plm = plm, partialIDparam = partialIDparam, estimated_regs = reg_estimates)


  # get NP bootstrap standard errors and CI
  if(bootstrap == TRUE){
    boot_results = bootstrap_regs(plm, partialIDparam = partialIDparam,n_boot = n_boot)
    se = stats::sd(boot_results)
    ci = stats::quantile(boot_results,probs = c(0.025,0.975))

    point_estimate_results = t(matrix(c(point_estimate,se,ci)))
    colnames(point_estimate_results) = c("Estimate","Std. Error","95% CI Low","95% CI High")
  } else {
    point_estimate_results = t(matrix(c(point_estimate)))
    colnames(point_estimate_results) = c("Estimate")
  }

  return(point_estimate_results)

}







#' @export
bootstrap_regs <- function(plm,partialIDparam,n_boot){

  # update this to use a boot strap package or to run it in C++


  boot_results = boot::boot(data = plm$dta, statistic = boot_funk, R = n_boot,
                            parallel="multicore",ncpus = parallel::detectCores(all.tests = FALSE, logical = TRUE),
                            plm = plm,partialIDparam = partialIDparam)$t

  ################

  # n = dim(plm$dta)[1]
  # boot_results = rep(0, n_boot)
  #
  # for(i in 1:n_boot){
  #
  #   boot_indices = sample(x = 1:n,size = n,replace=TRUE)
  #   boot_data = plm$dta[boot_indices,]
  #   temp_reg_est = estimate_regs(plm,dset_name="boot_data",dset = boot_data)
  #   boot_results[i] = estimate_PLM(plm = plm,partialIDparam = partialIDparam, estimated_regs = temp_reg_est)
  #
  # }

  ####################

  return(boot_results)

}

#' @export
boot_funk <- function(boot_data,indys,plm,partialIDparam){

  temp_reg_est = estimate_regs(plm,dset_name="boot_data",dset = boot_data[indys,])
  out = estimate_PLM(plm = plm,partialIDparam = partialIDparam, estimated_regs = temp_reg_est)
  return(out)

}





#' @export
estimate_regs <- function(plm,dset_name="",dset = NULL){

  results <- plm$regressions

  # estimate regressions in plm$regressions and save relevant results

  for(i in 1:length(plm$regressions)){

    if(dset_name != ""){
      formula = stringr::str_replace(plm$regressions[[i]], "data = plm\\$dta", "data = dset")
      m = eval(parse(text=formula))
    } else {
      formula = plm$regressions[[i]]
      m = eval(parse(text=formula))
    }

    coef_table = stats::coef(summary(m))
    df = m$df.residual
    betas = coef_table[,"Estimate"]
    ses = coef_table[,"Std. Error"]

    results[[i]] = list(
      betas = betas,
      ses = ses,
      df = df
    )

  }

  return(results)

}




#' @export
estimate_PLM <- function(plm,
                         partialIDparam,
                         estimated_regs){

  # this function provides the PLM estimate, given estimated quantities and assumed quantities


  if(plm$type == "Double Placebo"){

    beta_yd.px = estimated_regs$reg_Y_on_D_plus_P$betas[plm$treatment]
    beta_yp.dx = estimated_regs$reg_Y_on_D_plus_P$betas[plm$placebo_treatment]
    beta_nd.px = estimated_regs$reg_N_on_D_plus_P$betas[plm$treatment]
    beta_np.dx = estimated_regs$reg_N_on_D_plus_P$betas[plm$placebo_treatment]

    k_yd_yp = partialIDparam$k_yd_yp
    k_np_nd = partialIDparam$k_np_nd
    beta_yd.pxz = partialIDparam$coef_Y_D_given_PXZ
    beta_yp.dxz = partialIDparam$coef_Y_P_given_DXZ
    beta_nd.pxz = partialIDparam$coef_N_D_given_PXZ
    beta_np.dxz = partialIDparam$coef_N_P_given_DXZ

    #add functionality to reason about beta_yp.ndxz, beta_yn.pdxz, beta_np.dxz
    #beta_yp.dxz = beta_yp.ndxz + beta_yn.pdxz*beta_np.dxz

    beta_yd.pxz = beta_yd.px - k_yd_yp*k_np_nd*((beta_yp.dx - beta_yp.dxz)*(beta_nd.px - beta_nd.pxz)/(beta_np.dx - beta_np.dxz))
    estimate = beta_yd.pxz

  }
  else if(plm$type == "Single Placebo, No Direct Relationships, Placebo Outcome" |
          plm$type == "Single Placebo, Treatment causes Placebo" |
          plm$type == "Single Placebo, Placebo is Mediator, Placebo Outcome"){

    beta_yd.x = estimated_regs$reg_Y_on_D$betas[plm$treatment]
    beta_pd.x = estimated_regs$reg_P_on_D$betas[plm$treatment]
    se_yd.x = estimated_regs$reg_Y_on_D$ses[plm$treatment]
    se_pd.x = estimated_regs$reg_P_on_D$ses[plm$treatment]
    df_y = estimated_regs$reg_Y_on_D$df
    df_p = estimated_regs$reg_P_on_D$df

    k = partialIDparam$k
    beta_pd.xz = partialIDparam$coef_P_D_given_XZ

    beta_yd.xz = beta_yd.x - k*(beta_pd.x - beta_pd.xz)*((se_yd.x*sqrt(df_y))/(se_pd.x*sqrt(df_p)))
    estimate = beta_yd.xz

  }
  else if(plm$type == "Single Placebo, No Direct Relationships, Placebo Treatment" |
          plm$type == "Single Placebo, Placebo causes Outcome, Placebo Treatment"){

    beta_yd.px = estimated_regs$reg_Y_on_D_plus_P$betas[plm$treatment]
    beta_yp.dx = estimated_regs$reg_Y_on_D_plus_P$betas[plm$placebo_treatment]
    se_yd.px = estimated_regs$reg_Y_on_D_plus_P$ses[plm$treatment]
    se_yp.dx = estimated_regs$reg_Y_on_D_plus_P$ses[plm$placebo_treatment]
    df_y = estimated_regs$reg_Y_on_D_plus_P$df
    df_p = estimated_regs$reg_Y_on_D_plus_P$df

    k = partialIDparam$k
    beta_yp.dxz = partialIDparam$coef_Y_P_given_DXZ

    beta_yd.pxz = beta_yd.px - k*(beta_yp.dx - beta_yp.dxz)*((se_yd.px*sqrt(df_y))/(se_yp.dx*sqrt(df_p)))
    estimate = beta_yd.pxz

  }
  else if(plm$type == "Single Placebo, Placebo causes Outcome, Placebo Outcome"){

    beta_yd.px = estimated_regs$reg_Y_on_D_plus_P$betas[plm$treatment]
    beta_pd.x  = estimated_regs$reg_P_on_D$betas[plm$treatment]
    se_yd.px = estimated_regs$reg_Y_on_D_plus_P$ses[plm$treatment]
    se_pd.x = estimated_regs$reg_P_on_D$ses[plm$treatment]
    df_y = estimated_regs$reg_Y_on_D_plus_P$df
    df_p = estimated_regs$reg_P_on_D$df

    k  = partialIDparam$k
    beta_pd.xz  = partialIDparam$coef_P_D_given_XZ

    beta_yd.pxz = beta_yd.px - k*(beta_pd.x - beta_pd.xz)*((se_yd.px*sqrt(df_y))/(se_pd.x*sqrt(df_p)))
    estimate = beta_yd.pxz

  }

  else if(plm$type == "Single Placebo, Placebo is Mediator, Placebo Treatment"){

    beta_yd.x = estimated_regs$reg_Y_on_D$betas[plm$treatment]
    beta_yp.dx = estimated_regs$reg_Y_on_D_plus_P$betas[plm$placebo]
    se_yd.x = estimated_regs$reg_Y_on_D$ses[plm$treatment]
    se_yp.dx = estimated_regs$reg_Y_on_D_plus_P$ses[plm$placebo]
    df_yd = estimated_regs$reg_Y_on_D$df
    df_yp = estimated_regs$reg_Y_on_D_plus_P$df

    k = partialIDparam$k
    beta_yp.dxz = partialIDparam$coef_Y_P_given_DXZ

    beta_yd.xz = beta_yd.x - k*(beta_yp.dx - beta_yp.dxz)*((se_yd.x*sqrt(df_yd))/(se_yp.dx*sqrt(df_yp)))
    estimate = beta_yd.xz

  }
  else if(plm$type == "Single Placebo, Placebo is Observed Confounder"){

    beta_yd.px = estimated_regs$reg_Y_on_D_plus_P$betas[plm$treatment]
    beta_dp.x = estimated_regs$reg_D_on_P$betas[plm$placebo]
    se_yd.px = estimated_regs$reg_Y_on_D_plus_P$ses[plm$treatment]
    se_dp.x = estimated_regs$reg_D_on_P$ses[plm$placebo]
    df_yd = estimated_regs$reg_Y_on_D_plus_P$df
    df_dp = estimated_regs$reg_D_on_P$df

    k = partialIDparam$k
    beta_dp.xz = partialIDparam$coef_D_P_given_XZ

    beta_yd.pxz = beta_yd.px - k*(beta_dp.x - beta_dp.xz)*((se_yd.px*sqrt(df_yd))/(se_dp.x*sqrt(df_dp)))
    estimate = beta_yd.pxz

  }

  else if(plm$type == "Single Placebo, Outcome causes Placebo"){

    beta_yd.x = estimated_regs$reg_Y_on_D$betas[plm$treatment]
    beta_py.dx = estimated_regs$reg_P_on_Y_plus_D$betas[plm$outcome]
    se_yd.x = estimated_regs$reg_Y_on_D$ses[plm$treatment]
    se_py.dx = estimated_regs$reg_P_on_Y_plus_D$ses[plm$outcome]
    df_yd = estimated_regs$reg_Y_on_D$df
    df_py = estimated_regs$reg_P_on_Y_plus_D$df

    k = partialIDparam$k
    beta_py.dxz = partialIDparam$coef_P_Y_given_DXZ

    beta_yd.xz = beta_yd.x - k*(beta_py.dx - beta_py.dxz)*((se_yd.x*sqrt(df_yd))/(se_py.dx*sqrt(df_py)))
    estimate = beta_yd.xz
  }


  return(estimate)
}














#' @export
placeboLM_contour_plot <- function(plm,gran = 100){
  # this will provide a contour plot of point estimates that cover the range of partial ID parameters given

  # update to work for 3 parameter settings, where we pick one param to fix at min, mid, and max values and create 3 contour plots


  param_ranges = plm$partialIDparam_minmax
  num_param = length(param_ranges)
  if(num_param>2){
    error(cat("More than 2 partial identification parameters specified. Contour plot not possible. Use placeboLM_table()."))
  } else if(num_param<=2){

    # get regression estimates
    reg_estimates = estimate_regs(plm = plm)

    # get all parameter settings to run
    iter = gran
    val_matrix = matrix(0,ncol = num_param, nrow = iter)
    colnames(val_matrix) = names(param_ranges)

    for(i in 1:num_param){
      val_matrix[,i] = seq(from=min(param_ranges[[i]]),to=max(param_ranges[[i]]),length.out=iter)
      if(i==1){
        param_vals = val_matrix[,i]
      } else{
        param_vals = tidyr::crossing(param_vals,val_matrix[,i],.name_repair = "unique")
      }
    }
    param_vals = as.matrix(param_vals)
    colnames(param_vals) = names(param_ranges)


    # estimate at all param levels
    l_param_vals = dim(param_vals)[1]
    grid_results = cbind(param_vals,rep(0,l_param_vals))
    for(i in 1:l_param_vals){
      grid_results[i,3] = estimate_PLM(plm = plm, partialIDparam = as.list(param_vals[i,]), estimated_regs = reg_estimates)
    }
    grid_results = as.matrix(reshape(as.data.frame(grid_results), idvar = names(param_ranges)[1], timevar = names(param_ranges)[2], direction = "wide")[,-1])

    graphics::contour(x=val_matrix[,1],
                      y=val_matrix[,2],
                      z=grid_results,method="edge",
                      xlab=names(param_ranges)[1],
                      ylab=names(param_ranges)[2],
                      col="black",nlevels=20)
    graphics::contour(x=val_matrix[,1],
                      y=val_matrix[,2],
                      z=grid_results,
                      add=T,levels = 0,col = "red",lty=1,lwd = 2,labels = "0",method="edge")
  }

}







# this is a work in proggress


#' @export
placeboLM_line_plot <- function(plm,bootstrap=TRUE,n_boot=10,ptiles = c(0,0.5,1),focus_param = "k",ptile_param = "coef_P_D_given_XZ",gran = 100){

  param_ranges = plm$partialIDparam_minmax
  num_param = length(param_ranges)

  if(num_param>2){
    error(cat("More than 2 partial identification parameters specified. Line plot not possible. Use placeboLM_table()."))
  } else if(num_param<=2){

    # get regression estimates
    reg_estimates = estimate_regs(plm = plm)

    # get all parameter settings to run
    iter = gran
    val_matrix = matrix(0,ncol = num_param, nrow = iter*length(ptiles))
    colnames(val_matrix) = names(param_ranges)


    val_matrix[,focus_param] = rep(seq(from=min(param_ranges[[focus_param]]),to=max(param_ranges[[focus_param]]),length.out=iter),length(ptiles))
    ptile_param_ptiles = stats::quantile(x = param_ranges[[ptile_param]], probs = ptiles)
    val_matrix[,ptile_param] = sort(rep(ptile_param_ptiles,iter))

    if(bootstrap == TRUE){
      grid_results = matrix(0,ncol = 4, nrow = iter*length(ptiles))
      colnames(grid_results) = c("Estimate","Std. Error","95% CI Low","95% CI High")
      } else {
      grid_results = matrix(0,ncol = 1, nrow = iter*length(ptiles))
      colnames(grid_results) = c("Estimate")}
    grid_results = cbind(val_matrix,grid_results)


    # estimate at all param levels
    for(i in 1:(iter*length(ptiles))){
      grid_results[i,(3:dim(grid_results)[2])] = placeboLM_point_estimate(plm = plm,partialIDparam = as.list(grid_results[i,1:2]), bootstrap = bootstrap,n_boot = n_boot)
    }

    for(g in 1:length(ptiles)){
      gr1 = grid_results[grid_results[,ptile_param]==ptile_param_ptiles[g],]

      plot(x = gr1[,focus_param], y = gr1[,"Estimate"], type = "l",lwd=2,
           ylab = "Estimate",
           xlab = focus_param,
           main = paste0(ptile_param," = ",ptile_param_ptiles[g]," (",ptiles[g]*100,"th percentile)"),
           ylim = c(min(grid_results[,"Estimate"]),max(grid_results[,"Estimate"])))
      graphics::lines(x = gr1[,focus_param], y = gr1[,"95% CI Low"],col="blue",lty = 2)
      graphics::lines(x = gr1[,focus_param], y = gr1[,"95% CI High"],col="blue",lty = 2)
      graphics::abline(h=0,col="red",lwd=2)
      graphics::abline(v=0,col="gray",lwd=1)
    }

  }
}
