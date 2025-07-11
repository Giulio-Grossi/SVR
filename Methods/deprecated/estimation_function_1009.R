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
  
  
  ### 3 - Pooled ridge regression.
  
  if ("PR" %in% method){  # We do not use this currently.
    warning('Pooled Ridge not checked.')
    # THIS FUNCTION IS NOT CURRENTLY USED.
    # If we use it again, we MUST check the following:
    #
    #  - for the choice of penalty:
    # on line 84, you use tcv_ridge on sim, which includes post-intervention data.
    # Shouldn’t we be finding the optimal lambda only in the pre-intervention period?
    # Maybe the argument “train” achieves that. But still, in sim we have both treated
    # and control units. Shouldn’t we be trying to fit the treated units only?
    #  
    #  - for the results:
    #  the results of the pooled ridge (line 88) is a matrix of dimension (# bands) x
    # (# controls + 1). I believe this includes the intercept and the coefficient for
    # each control for EACH of the outcomes. Shouldn’t we have ONE coefficient for
    # each control across all outcomes? I believe the problem might be with the
    # x_matrix() function.
    
    
    l1 = seq(0.01,10,0.01)  # Vector of length 1000
    l = tcv_ridge(sim, x, l1, train)  # Returns 1000 x 2 matrix
    lam = as.numeric(subset(l, l[,2] == min(l[,2])))
    l1 = lam[1]
    ridge.r <- ridge(yv.pre, x.pre, l1, SCM = F)
    c.pool.rid <- ridge.r[[3]]
    out$PR <- c.pool.rid
  }
 
  ## 3- FUSED RIDGE
  
  if ("FR" %in% method){  # We are not using it, but could be interesting for future.
    warning('Fused Ridge not checked.')
    l1=seq(0.01,10,0.01)
    l2=0
    l=tcv_fused(sim,x,l1,l2, SCM=F,train)
    lam=subset(l, l[,3]==min(l[,3]))
    lam=lam[1,1:2]
    l1=lam[1]; l2=lam[2]
    l1=l1;l2=0
    fused.rid <- fused(yv.pre, x.pre, l1,SCM = F)
    c.fus.rid <- fused.rid[[3]]
    out$FR <- c.fus.rid
  }
 
  ### 4 - GAUSSIAN PROCESS REGRESSION
  # -GP- I don't have alpha.stan. So I could not check this part.
  
  if ("GP" %in% method) {
    
    warning('GP not checked.')
    
    ss_data = list(
      x = x_matrix_int(cbind(1, x.pre)),
      x_pre = x.pre,
      h = as.numeric(scale(treated_radius)),
      y = yv.pre,
      N = (t0 * bands),
      K = ((num_controls+1)*bands),
      C = num_controls+1,
      D = bands,
      t0=t0,
      time=seq(1, t0,1),
      X_new=x_matrix_int(cbind(1,x)),
      N_new=nrow(x)
    )
    
    fit <- rstan::stan(
      file = "alpha.stan",
      data = ss_data,
      cores = 3,
      iter = iter,
      chains = 3,
      verbose = F,
      warmup = warm,
      control = list(
        max_treedepth = 15,
        stepsize = 0.05,
        adapt_delta = 0.90
      )
    )
    
    params0 <- fit
    out$GP <- params0
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
  
  
  if ("PVN" %in% method) {
    
    ## Pooled vertical regression, it is a Vertical regression with 
    # pooled estimates and possibly, shrinkage priors
    # WE ARE NOT USING IT ATM 
    
    warning("Pooled Vertical Regression non checked")
    ss_data = list(
      x = x_matrix_int(cbind(1,x.pre)),
      x_pre=x.pre,
      h = as.numeric(scale(treated_radius)),
      y = yv.pre,
      N = (t0 * bands),
      K = ((num_controls+1)*bands),
      C = num_controls+1,
      D = bands,
      t0=t0,
      time=seq(1, t0,1),
      X_new=x_matrix_int(cbind(1,x)),
      N_new=nrow(x)
    )
    
    fit1 <- rstan::stan(
      file = "vanilla_alpha.stan",
      data = ss_data,
      cores = 3,
      iter = iter,
      chains = 3,
      verbose = F,
      warmup = warm,
      control = list(
        max_treedepth = 12,
        stepsize = 0.05,
        adapt_delta = 0.85
      )
    )
    params1 <- rstan::extract(fit1)
    out$PVN <- params1
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

