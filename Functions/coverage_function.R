###############################################################################
######################### COVERAGE ESTIMATION ######################
###############################################################################

## @ sim <- data or simulation matrix
## @ CI <- confidence intervals

## Preparing different matrix config for estimation 
#  pre/post treatment

coverage <- function(sim, interv){
  
  bands <- ncol(interv[[1]]$upper_bound)
  time_periods <- nrow(interv[[1]]$upper_bound)
  
  out <- list()
  
  ### 1.0 SEPARATED SCM
  if ("SC" %in% names(interv)){
    out$SC <- cover(sim[,1:bands], 
                    interv$SC$lower_bound, interv$SC$upper_bound)
  }
  
  
  ### 2.0 SEPARATED VERTICAL REGRESSION WITH RIDGE PEN
  if ("SR" %in% names(interv)){
    out$SR <- cover(sim[,1:bands], 
                    interv$SR$lower_bound, interv$SR$upper_bound)
  }
  
  ### 3.0 MULTIVARIATE OLS
  if ("OLS" %in% names(interv)){
    out$OLS <- cover(sim[,1:bands], 
                     interv$OLS$lower_bound, interv$OLS$upper_bound)
  }
  
  ### 4.0 BAYESIAN VERTICAL REGRESSION
  if ("BVR" %in% names(interv)){
    out$BVR <- cover(sim[,1:bands], 
                     interv$BVR$lower_bound, interv$BVR$upper_bound)
  } 
  
  
  ### 5.0 BAYESIAN SYNTHETIC CONTROL
  if ("BSC" %in% names(interv)){
    out$BSC <- cover(sim[,1:bands], down = interv$BSC$lower_bound,
                     up = interv$BSC$upper_bound)
  } 
  
  
  ### 6.0 SMAC
  if ("SMAC" %in% names(interv)){
    out$SMAC <- cover(sim[,1:bands], down = interv$SMAC$lower_bound,
                      up = interv$SMAC$upper_bound)
  } 

  return(out)
  
}
