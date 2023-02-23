
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
                      partialIDparam_minmax = c(list("coef_P_Y_given_XZ" = c(-2,2),"k_M" = c(-2,2)),
                                                list("coef_D_P_given_XZ" = c(-2,2),"k_P" = c(-2,2)),
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
    }
    if(PY=="<-" & DP=="<-"){
      cat("Error: Values for PY and DP create a cycle.")
    }
  }

  class(collect) <- "placeboLM"
  return(collect)
}




#' @export
placeboLM_point_estimate <- function(plm,
                               partialIDparam,
                               n_boot = 1000){
  # this will provide a single point estimate, SE, and CI
  # takes in plm object and partialID params

  # get regression estimates
  reg_estimates = estimate_regs(plm = plm)

  # get point estimate
  point_estimate = estimate_PLM(plm = plm, partialIDparam = partialIDparam, estimated_regs = reg_estimates)

  # get NP bootstrap standard errors and CI
  boot_results = bootstrap_regs(plm, partialIDparam = partialIDparam,n_boot = n_boot)
  se = sd(boot_results)
  ci = stats::quantile(boot_results,probs = c(0.025,0.975))

  point_estimate_results = t(matrix(c(point_estimate,se,ci)))
  colnames(point_estimate_results) = c("Estimate","Std. Error","95% CI Low","95% CI High")
  return(point_estimate_results)

}

#' @export
bootstrap_regs <- function(plm,partialIDparam,n_boot = 1000){


  n = dim(plm$dta)[1]
  boot_results = rep(0, n_boot)

  for(i in 1:n_boot){

    boot_indices = sample(x = 1:n,size = n,replace=TRUE)
    boot_data = plm$dta[boot_indices,]
    temp_reg_est = estimate_regs(plm,dset_name="boot_data",dset = boot_data)
    boot_results[i] = estimate_PLM(plm = plm,partialIDparam = partialIDparam, estimated_regs = temp_reg_est)

  }

  return(boot_results)

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

  if(plm$type == "Double Placebo"){}
  else if(plm$type == "Single Placebo, No Direct Relationships, Placebo Outcome"){}
  else if(plm$type == "Single Placebo, No Direct Relationships, Placebo Treatment"){}
  else if(plm$type == "Single Placebo, Treatment causes Placebo"){}
  else if(plm$type == "Single Placebo, Placebo causes Outcome, Placebo Treatment"){}
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

  return(estimate)
}
