
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
                      partialIDparam_minmax = c(list("coef_P_D_given_XZ" = c(-2,2),"k" = c(-2,2)),
                                                list("coef_Y_P_given_DXZ" = c(-2,2),"k" = c(-2,2)),
                                                list("coef_Y_P_given_DXZ" = c(-2,2),"coef_P_D_given_XZ" = c(-2,2),"R_P_Z_given_DX" = c(-2,2)))
                      ){
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

  if(placebo_outcome != "" & placebo_treatment != ""){
    cat("Both a placebo treatment and a placebo outcome have been indicated.", "\n",
        "PlaceboLM assumes a 'double placebo' setting is desired.", "\n",
        "PlaceboLM assumes the following causal relations:", "\n",
        "    D->N", "\n",
        "    P->D", "\n",
        "    P->N", "\n",
        "    P->Y", "\n",
        "    N->Y", "\n",
        "If you wish to assume that one of these causal relations does not exist", "\n",
        "set the relavant partial identification parameter to zero.")
    collect$type = "Double Placebo"
    collect$regressions <- list(
      reg_Y_on_D_plus_P = paste0("lm(",outcome,"~",paste0(c(treatment,placebo_treatment,observed_covariates),collapse = " + ")," , data = plm$dta )"),
      reg_N_on_D_plus_P = paste0("lm(",placebo_outcome,"~",paste0(c(treatment,placebo_treatment,observed_covariates),collapse = " + ")," , data = plm$dta)")
      )
  } else if(placebo_outcome == "" & placebo_treatment == ""){
    cat("Error: No placebo indicated.")
  }
  else {
    if(PY=="" & DP==""){
      cat("Placebo assumed to have no direct relationship with either treatment or outcome.")
      if(placebo_outcome != ""){
        collect$type = "Single Placebo, No Direct Relationships, Placebo Outcome"
        collect$regressions <- list(
          reg_Y_on_D = paste0("lm(",outcome,"~",paste0(c(treatment,observed_covariates),collapse = " + ")," , data = plm$dta)"),
          reg_P_on_D = paste0("lm(",placebo_outcome,"~",paste0(c(treatment,observed_covariates),collapse = " + ")," , data = plm$dta)"))
      } else if(placebo_treatment != ""){
        collect$type = "Single Placebo, No Direct Relationships, Placebo Treatment"
        collect$regressions <- list(
          reg_Y_on_D_plus_P = paste0("lm(",outcome,"~",paste0(c(treatment,placebo_treatment,observed_covariates),collapse = " + ")," , data = plm$dta)"))
      }
    }
    if(PY=="" & DP=="->"){
      cat("Placebo assumed to be directly caused by treatment.")
      collect$type = "Single Placebo, Treatment causes Placebo"
      if(placebo_outcome != ""){placebo = placebo_outcome} else {placebo = placebo_treatment}
      collect$regressions <- list(
        reg_Y_on_D = paste0("lm(",outcome,"~",paste0(c(treatment,observed_covariates),collapse = " + ")," , data = plm$dta)"),
        reg_P_on_D = paste0("lm(",placebo,"~",paste0(c(treatment,observed_covariates),collapse = " + ")," , data = plm$dta)"))
    }
    if(PY=="->" & DP==""){
      cat("Placebo assumed to directly cause outcome.")
      if(placebo_treatment != ""){
        collect$type = "Single Placebo, Placebo causes Outcome, Placebo Treatment"
        collect$regressions <- list(
          reg_Y_on_D_plus_P = paste0("lm(",outcome,"~",paste0(c(treatment,placebo_treatment,observed_covariates),collapse = " + ")," , data = plm$dta)"))
      } else if(placebo_outcome != ""){
        collect$type = "Single Placebo, Placebo causes Outcome, Placebo Outcome"
        collect$regressions <- list(
          reg_Y_on_D_plus_P = paste0("lm(",outcome,"~",paste0(c(treatment,placebo_outcome,observed_covariates),collapse = " + ")," , data = plm$dta)"),
          reg_P_on_D = paste0("lm(",placebo_outcome,"~",paste0(c(treatment,observed_covariates),collapse = " + ")," , data = plm$dta)"))
      }
    }
    if(PY=="->" & DP=="->"){
      cat("Placebo assumed to be a mediator between treatment and outcome.", "\n",
          "For partial identification of the driect or indirect effect", "\n",
          "use approaches from Zhang and Ding (2022).", "\n",
          "PlaceboLM will assume total effect is target causal contrast.")
      collect$type = "Single Placebo, Placebo is Mediator"
      if(placebo_outcome != ""){placebo = placebo_outcome} else {placebo = placebo_treatment}
      collect$regressions <- list(
        reg_Y_on_D = paste0("lm(",outcome,"~",paste0(c(treatment,observed_covariates),collapse = " + ")," , data = plm$dta)"),
        reg_P_on_D = paste0("lm(",placebo,"~",paste0(c(treatment,observed_covariates),collapse = " + ")," , data = plm$dta)"),
        reg_Y_on_D_plus_P = paste0("lm(",outcome,"~",paste0(c(treatment,placebo,observed_covariates),collapse = " + ")," , data = plm$dta)"))
    }
    if(DP=="<-"){
      cat("Placebo assumed to be an observed confounder.")
      collect$type = "Single Placebo, Placebo is Observed Confounder"
      if(placebo_outcome != ""){placebo = placebo_outcome} else {placebo = placebo_treatment}
      collect$regressions <- list(
        reg_Y_on_D_plus_P = paste0("lm(",outcome,"~",paste0(c(treatment,placebo,observed_covariates),collapse = " + ")," , data = plm$dta)"),
        reg_D_on_P = paste0("lm(",treatment,"~",paste0(c(placebo,observed_covariates),collapse = " + ")," , data = plm$dta)"))
    }
    if(PY=="<-"){
      cat("Placebo assumed to be a descendant of outcome.", "\n",
          "Use approach from Cinelli and Hazlett (2020), without conditioning on P.")
      collect$type = "Single Placebo, Outcome causes Placebo"
      collect$regressions <- list(
        reg_Y_on_D = paste0("lm(",outcome,"~",paste0(c(treatment,observed_covariates),collapse = " + ")," , data = plm$dta)"))
    }
    if(PY=="<-" & DP=="<-"){
      cat("Error: Values for PY and DP create a cycle.")
    }
  }

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
    se = sd(boot_results)
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

  # this only provides the PLM estimate, given estimated quantities and assumed quantities

  # update this to fill in all expressions

  if(plm$type == "Double Placebo"){

    beta_yd.px = estimated_regs$reg_Y_on_D_plus_P$betas[plm$treatment]
    beta_yp.dx = estimated_regs$reg_Y_on_D_plus_P$betas[plm$placebo_treatment]
    beta_nd.px = estimated_regs$reg_N_on_D_plus_P$betas[plm$treatment]
    beta_np.dx = estimated_regs$reg_N_on_D_plus_P$betas[plm$placebo_treatment]

    beta_yp.ndxz = partialIDparam$coef_Y_P_given_NDXZ
    beta_yn.pdxz = partialIDparam$coef_Y_N_given_PDXZ
    beta_nd.pxz  = partialIDparam$coef_N_D_given_PXZ
    beta_np.dxz  = partialIDparam$coef_N_P_given_DXZ

    beta_yp.dxz = beta_yp.ndxz + beta_yn.pdxz*beta_np.dxz

    beta_yd.pxz = beta_yd.px - (((beta_yp.dx - beta_yp.dxz)*(beta_nd.px - beta_nd.pxz))/(beta_np.dx - beta_np.dxz))
    estimate = beta_yd.pxz

  }
  else if(plm$type == "Single Placebo, No Direct Relationships, Placebo Outcome" |
          plm$type == "Single Placebo, Treatment causes Placebo"){

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
    beta_yp.dx = estimated_regs$reg_Y_on_D_plus_P$betas[plm$placebo_outcome]
    beta_pd.x  = estimated_regs$reg_P_on_D$betas[plm$treatment]

    beta_yp.dxz = partialIDparam$coef_Y_P_given_DXZ
    beta_pd.xz  = partialIDparam$coef_P_D_given_XZ
    r_pz.dx     = partialIDparam$R_P_Z_given_DX

    beta_yd.pxz = beta_yd.px - ((beta_yp.dx - beta_yp.dxz)*(beta_pd.x - beta_pd.xz))/(r_pz.dx^2)
    estimate = beta_yd.pxz

  }
  else if(plm$type == "Single Placebo, Placebo is Mediator"){}
  else if(plm$type == "Single Placebo, Placebo is Observed Confounder"){}
  else if(plm$type == "Single Placebo, Outcome causes Placebo"){

    # use approach from Cinelli and Hazlett (2020)

    beta_yd.x = estimated_regs$reg_Y_on_D$betas[plm$treatment]
    se_yd.x = estimated_regs$reg_Y_on_D$ses[plm$treatment]
    df_y = estimated_regs$reg_Y_on_D$df

    R_Y_Z_given_DX = partialIDparam$R_Y_Z_given_DX
    R_Z_D_given_X = partialIDparam$R_Z_D_given_X

    beta_yd.xz = beta_yd.x - ((R_Y_Z_given_DX*R_Z_D_given_X)/sqrt(1-R_Z_D_given_X^2))*(se_yd.x*sqrt(df_y))
    estimate = beta_yd.xz

  }


  return(estimate)
}
