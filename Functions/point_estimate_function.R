###############################################################################
################ MSE-BIAS AFTER COEFFICIENT ESTIMATION #####################
###############################################################################
## @ sim <- data or simulation matrix
## @ cal <- results from calculation function


point_estimate <- function(sim, cal) {
  bias=list();mse=list()
  ### 1.0 SEPARATED SCM
  if ("SC" %in% names(cal)){
    bands=ncol(cal$SC)
    bias$SC=sim[,1:bands] - cal$SC
    mse$SC=(sim[,1:bands] - cal$SC)^2
  }
  
  
  ### 2.0 SEPARATED VERTICAL REGRESSION WITH RIDGE PEN
  if ("SR" %in% names(cal)){
    bands=ncol(cal$SR)
    bias$SR=sim[,1:bands] - cal$SR
    mse$SR=(sim[,1:bands] - cal$SR)^2
    }
  
  ### 3.0 MULTIVARIATE OLS
  if ("OLS" %in% names(cal)){
    bands=ncol(cal$OLS)
    bias$OLS=sim[,1:bands] - cal$OLS
    mse$OLS=(sim[,1:bands] - cal$OLS)^2
  }
  
  ### 4.0 BAYESIAN VERTICAL REGRESSION
  if ("BVR" %in% names(cal)){
    bands=ncol(cal$BVR)
    bias$BVR=sim[,1:bands] - cal$BVR
    mse$BVR=(sim[,1:bands] - cal$BVR)^2
    }

  
  ### 5.0 BAYESIAN SYNTHETIC CONTROL
  if ("BSC" %in% names(cal)){
    bands=ncol(cal$BSC)
    bias$BSC=sim[,1:bands] - cal$BSC
    mse$BSC=(sim[,1:bands] - cal$BSC)^2
    }
    
  ### 6.0 SMAC
  if ("SMAC" %in% names(cal)){
    bands=ncol(cal$SMAC)
    bias$SMAC=sim[,1:bands] - cal$SMAC
    mse$SMAC=(sim[,1:bands] - cal$SMAC)^2
  }
  
  library(purrr)
  bias=compact(bias)
  mse=compact(mse)
  out=list(bias, mse)
  
  return(out)
}
