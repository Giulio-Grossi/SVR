###############################################################################
######################### COVERAGE ESTIMATION ######################
###############################################################################

## @ sim <- data or simulation matrix
## @ CI <- confidence intervals

## Preparing different matrix config for estimation 
#  pre/post treatment

coverage_function<-function(sim, ci){
  
  bands=ncol(ci$SC$upper_bound)
  time_periods=nrow(ci$SC$upper_bound)
  out=list()
  ### 1.0 SEPARATED SCM
  if ("SC" %in% names(ci)){
   out$SC <- coverage(sim[,1:bands], 
                      ci$SC$lower_bound, ci$SC$upper_bound)
  }
  
  
  ### 2.0 SEPARATED VERTICAL REGRESSION WITH RIDGE PEN
  if ("SR" %in% names(ci)){
    out$SR <- coverage(sim[,1:bands], 
                       ci$SR$lower_bound, ci$SR$upper_bound)
  }
  
  ### 3.0 MULTIVARIATE OLS
  if ("OLS" %in% names(ci)){
    out$OLS <- coverage(sim[,1:bands], 
                        ci$OLS$lower_bound, ci$OLS$upper_bound)
  }
  
  ### 4.0 BAYESIAN VERTICAL REGRESSION
  if ("BVR" %in% names(ci)){
    out$BVR <- coverage(sim[,1:bands], 
                        ci$BVR$lower_bound, ci$BVR$upper_bound)
  } 
  
  
  ### 5.0 BAYESIAN SYNTHETIC CONTROL
  if ("BSC" %in% names(ci)){
    out$BSC <- coverage(sim[,1:bands], 
                        ci$BSC$lower_bound, ci$BSC$upper_bound)
  } 
  
  
  ### 6.0 SMAC
  if ("SMAC" %in% names(ci)){
    out$SMAC <- coverage(sim[,1:bands], 
                        ci$SMAC$lower_bound, ci$SMAC$upper_bound)
  } 
  
  library(purrr)
  out=compact(out)
  names(out)=names(est)
  
  return(out)
  
}
