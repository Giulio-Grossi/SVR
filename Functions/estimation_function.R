###############################################################################
######################### COEFFICIENT ESTIMATION ##############################
###############################################################################

## @ sim <- data or simulation matrix
## @ est <- results from estimation function
## @ norm <- normalization of outcomes - pre treatment


estimation <- function(sim, t0, bands, iter, warm, norm, method){
  
  # Getting some quantities that are used later in the code.
  num_controls <- dim(sim)[2] - bands
  
  if (norm == T) {  # Standardizing using the mean and SD in the pre-intervention period.
    sim <- preset(sim, t0)$std
  }
  
  # Preparing different matrix config for estimation:
  # Treated and control units, pre/post treatment, training
  
  # For the vectorized outcomes, all data for unit 1 are first, then data from
  # unit 2, etc. So as.vector loops over times and then units.
  
  # Getting the treated unit data:
  yv = as.vector(sim[, 1:bands])  # As a vector over all time periods
  yv.pre = as.vector(sim[1 : t0, 1 : bands])  # As a vector in pre-intervention
  ym = sim[, 1 : bands]  # As a matrix over all time periods.
  ym.pre = sim[1:t0, 1:bands] # As a matrix in pre-intervention.
  
  # Getting the control unit data.
  x = sim[, (bands + 1) : (num_controls + bands)]  # Matrix, all time periods
  x.pre = sim[1 : t0, ((bands) + 1) : (num_controls + bands)]  # Matrix, pre-intervention
  
  # Getting the training data, will be used in ridge only.
  train = round(t0 - t0/5)
  y.train = as.vector(sim[1:train, 1:bands])
  x.train = sim[1:train, ((bands) + 1):(num_controls + bands)]
  
  
  ### storing results
  
  out = list()
  
  ### 1.0 SEPARATED SCM
  
  if ("SC" %in% method){
    out$SC = sepSC(ym.pre = ym.pre, x.pre = x.pre)
    print("SC estimates done")
  }
  
  ### 2- Separate ridge estimation
  
  if ("SR" %in% method){
    out$SR = sepSR(ym.pre = ym.pre, x.pre = x.pre)  # -GP- Be careful! It includes intercepts!
    print("SR estimates done")
  }
  
  if ("OLS" %in% method){ # Pooled OLS
    
    # -GP- When using
    #     tx = x_matrix(x.pre)
    # tx ends up being too large with way too many 0s. This is not the matrix
    # we want to use as our design matrix for pooled OLS.
    #
    # We want to have the matrix of the controls repeated #bands times on top
    # of each other. So:
    # tx <- x.pre 
    # for (i in 2 : bands) {
    #   tx <- rbind(tx, x.pre)  # this should be (#bands * T0) x (#controls)
    # }
    
    ## - GG - we can estimate the OLS coefficient also via multivariate
    # regression in a simultaneous estimate without passing through 
    # the x_matrix(x) argument. 
    
    lmod <- lm(ym.pre ~ x.pre)
    out$pooledOLS <- lmod$coef
    print("OLS estimates done")
  }
  
  if ("BVR" %in% method) {
    
  out$BVR <- sepBVR(ym.pre = ym.pre, x.pre = x.pre, x=x)
  print("BVR estimates done")
  }
  
  
  if ("BSC" %in% method){
    out$BSC <- sepBSC(ym.pre = ym.pre, x.pre = x.pre, x=x)
    print("BSC estimates done")
  }
  
  if("SMAC" %in% method) {
    out$SMAC <- SMAC(ym.pre, x.pre, x, treated_radius)
    print("SMAC estimates done")
  }
  
  
  library(purrr)
  out = compact(out)
  
  return(out)
  
}

