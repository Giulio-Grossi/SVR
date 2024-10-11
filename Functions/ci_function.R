###############################################################################
######################### CONFIDENCE INTERVAL ESTIMATION ######################
###############################################################################

## @ sim <- data or simulation matrix
## @ cal <- results from estimation function
## @ norm <- normalization of outcomes - pre treatment

## Preparing different matrix config for estimation 
#  pre/post treatment

ci <- function(sim, est, cal, t0, norm){
  
  bands = ncol(cal[[1]])
  
  # Setting these in case the data are not standardized, norm = FALSE.
  means <- rep(0, bands)
  sds <- rep(1, bands)
  
  # Standardizing the data using the preset function.
  if (norm == T) {
    sim_std <- preset(sim, t0)
    sim <- sim_std$std
    means <- sim_std$means
    sds <- sim_std$sds
  }
  # -GP- CAREFUL! From now on, sim is the standardized data!
  
  num_controls = ncol(sim) - bands
  time_periods = nrow(sim)
  out <- list()
  
  ### 1.0 SEPARATED SCM
  if ("SC" %in% names(cal)) {
    true <- sim[1:t0, 1 : bands]
    estimate <- cal$SC[1:t0,]
    out$SC <- ci_shen(sim, estimate, bands, t0)
  }
  
  
  ### 2.0 SEPARATED VERTICAL REGRESSION WITH RIDGE PEN
  if ("SR" %in% names(cal)) {
    true <- sim[1:t0, 1 : bands]
    estimate <- cal$SR[1:t0,]
    out$SR <- ci_shen(sim, estimate, bands, t0)
  }
  
  ### 3.0 MULTIVARIATE OLS
  if ("OLS" %in% names(cal)) {
    true <- sim[, 1 : bands]
    estimate <- cal$OLS
    out$OLS <- ci_shen(sim, estimate, bands, t0)
  }
  
  # -GP- The CIs for the Bayesian methods below have been corrected to be on
  # the original scale of the data, NOT standardized.
  
  ### 4.0 BAYESIAN VERTICAL REGRESSION
  if ("BVR" %in% names(est)) {
    bands <- ncol(cal$BVR)
    out$BVR <- ci_bayes(est_method = est[['BVR']], bands = bands,
                        means = means, sds = sds) 
  }
  
  ### 5.0 BAYESIAN SYNTHETIC CONTROL
  if ("BSC" %in% names(est)) {
    bands <- ncol(cal$BSC)
    out$BSC <- ci_bayes(est_method = est[["BSC"]], bands = bands, 
                        means = means, sds = sds) 
  }
  
  
  ### 6.0 SMAC
  if ("SMAC" %in% names(est)) {
    bands <- ncol(cal$SMAC)
    out$SMAC <- ci_bayes(est_method = est[['SMAC']], bands, 
                         means = means, sds = sds)
  }
  
  library(purrr)
  out=compact(out)

  return(out)
  
}
