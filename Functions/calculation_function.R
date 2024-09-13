###############################################################################
################ CALCULATION AFTER COEFFICIENT ESTIMATION #####################
###############################################################################
## @ sim <- data or simulation matrix
## @ est <- results from estimation function
## @ norm <- normalization of outcomes - pre treatment


calculation <- function(sim, est, norm) {

  # Standardizing the data using the preset function.
  if (norm == T) {
    sim_std <- preset(sim, t0)
    sim <- sim_std$std
    means <- sim_std$means
    sds <- sim_std$sds
  }
  
  ## Preparing different matrix config for estimation 
  #  pre/post treatment
  
  # Getting the treated unit data:
  ym = sim[, 1 : bands]  # As a matrix over all time periods.
  ym.pre = sim[1:t0, 1:bands] # As a matrix in pre-intervention.
  
  # Getting the control unit data.
  x = sim[, (bands + 1) : (num_controls + bands)]  # Matrix, all time periods
  x.pre = sim[1 : t0, ((bands) + 1) : (num_controls + bands)]  # Matrix, pre-intervention
  

  out=list()
  ### 1.0 SEPARATED SCM
  if ("SC" %in% names(est)){
  out$SC=cal_sepSC(est,x)  
  }
  
  
  ### 2.0 SEPARATED VERTICAL REGRESSION WITH RIDGE PEN
  if ("SR" %in% names(est)){
    out$SR=cal_sepSR(est,x)  
  }
  
  ### 3.0 MULTIVARIATE OLS
  if ("OLS" %in% names(est)){
   c.pool.ols = as.matrix(est[["OLS"]])
   out$OLS = x %*% c.pool.ols
  }

  ### 4.0 BAYESIAN VERTICAL REGRESSION
  if ("BVR" %in% names(est)){
    mat=matrix(nrow=nrow(x), ncol=bands)
    for(i in 1:bands){
      mat[,i]=apply(est[["BVR"]][[i]]$y_new,2,median)
    }
    
    out$BVR=mat
  }

  ### 5.0 BAYESIAN SYNTHETIC CONTROL
  if ("BSC" %in% names(est)){
    mat=matrix(nrow=nrow(x), ncol=bands)
    for(i in 1:bands){
      mat[,i]=apply(est[["BSC"]][[i]]$y_new,2,median)
    }

    out$BSC=mat
  }
  
  ### 6.0 SMAC
  if ("SMAC" %in% names(est)){
    posterior=est[["SMAC"]]
    store=vector()
    for(i in 1:bands){
      loc=apply(posterior$ynn[,,i],2,median)
      store=cbind(store, loc)
    }
    out$SMAC=store
  }
  
  library(purrr)
  out=compact(out)
  names(out)=names(est)

  ### rescale outcomes
  
   if (norm==T){
     for (k in 1:length(out)){
       for (i in 1:bands){
         out[[k]][,i]=(out[[k]][,i]*sds[i])+means[i]
       }
     }
   }

  return(out)
}
