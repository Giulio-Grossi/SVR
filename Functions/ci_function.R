###############################################################################
######################### CONFIDENCE INTERVAL ESTIMATION ######################
###############################################################################

## @ sim <- data or simulation matrix
## @ cal <- results from estimation function
## @ norm <- normalization of outcomes - pre treatment

## Preparing different matrix config for estimation 
#  pre/post treatment

ci <- function(sim, est, cal, norm){
  
  # Standardizing the data using the preset function.
  # if (norm == T) {
  #   sim_std <- preset(sim, t0)
  #   sim <- sim_std$std
  #   means <- sim_std$means
  #   sds <- sim_std$sds
  # }
  

out=list()
### 1.0 SEPARATED SCM
if ("SC" %in% names(cal)){
  bands=ncol(cal$SC)
  true=sim[,1:bands]
  estimate=cal$SC
  out$SC=ci_shen(true, estimate)
}


### 2.0 SEPARATED VERTICAL REGRESSION WITH RIDGE PEN
if ("SR" %in% names(cal)){
  bands=ncol(cal$SR)
  true=sim[,1:bands]
  estimate=cal$SR
  out$SR=ci_shen(true, estimate)
}

### 3.0 MULTIVARIATE OLS
if ("OLS" %in% names(cal)){
  bands=ncol(cal$OLS)
  true=sim[,1:bands]
  estimate=cal$OLS
  out$OLS=ci_shen(true, estimate)
}

### 4.0 BAYESIAN VERTICAL REGRESSION
if ("BVR" %in% names(est)){
  time_periods<-nrow(sim)
  bands<-ncol(cal$BVR)
  out$BVR <- ci_bayes(sim, est, "BSC", norm) 
}

### 5.0 BAYESIAN SYNTHETIC CONTROL
if ("BSC" %in% names(est)){
  time_periods<-nrow(sim)
  bands<-ncol(cal$BSC)
  out$BSC <- ci_bayes(sim, est, "BSC", norm) 
}
  

### 6.0 SMAC
if ("SMAC" %in% names(est)){
  posterior=rstan::extract(est[["SMAC"]])
  time_periods<-nrow(sim)
  bands<-ncol(cal$BSC)
  lower=matrix(nrow=time_periods, ncol=bands)
  upper=matrix(nrow=time_periods, ncol=bands)
  for(i in 1:bands){
    lower[,i] <- apply(posterior$ynn[,,i],2,0.025)
    upper[,i] <- apply(posterior$ynn[,,i],2,0.975)
    store=cbind(store, loc)
  }
  
  loc=list(lower, upper)
  names(loc)=c("lower_bound","upper_bound")
  out$SMAC=loc
}

library(purrr)
out=compact(out)
names(out)=names(est)

return(out)

}
