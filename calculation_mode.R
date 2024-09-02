###############################################################################
################ CALCULATION AFTER COEFFICIENT ESTIMATION #####################
###############################################################################
## @ sim <- data or simulation matrix
## @ est <- results from estimation function
## @ norm <- normalization of outcomes - pre treatment


calculations_mode <- function(sim, est, norm) {
  library(augsynth)
  
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
    c.sep.scm=est[["SC"]]
    pino = matrix(nrow = time_periods, ncol = bands)
    pred = vector()
    tot_bias = vector()
    for (i in 1:bands) {
      # v = ym.pre[,i]
      # gg = SCM(v, x.pre)
      # c.sep.scm=cbind(c.sep.scm, gg)
      p1 = x %*% c.sep.scm[,i]
      pino[, i] = p1
    }
    out[[1]] = pino
  }
  
  if ("SR" %in% names(est)){
    c.sep.rid=est[["SR"]]
    pino = matrix(nrow = time_periods, ncol = bands)
    pred = vector()
    tot_bias = vector()
    for (i in 1:bands) {
      p1 = cbind(1,x) %*% c.sep.rid[,i]
      pino[, i] = p1
    }
    out[[2]] = pino
  }

  
  ## 2- POOLED RIDGE
  if ( "PR" %in% names(est)){
  c.pool.rid <- est[["PR"]]
  int=matrix(data=0, nrow=time_periods, ncol=bands)
  int[,1]=1
  loc=list()
  for (i in 1:bands){
    loc[[i]]=rep(1, time_periods)
  }
  int=bdiag(loc)
  int=as.matrix(int)
  tx=cbind(1, x)
  out[[3]]= tx%*%t(c.pool.rid)
  #c.fus.rid = fused.rid[[3]]
  #b=as.vector(ym[(t0+1):time_periods,]-p.pool.rid[(t0+1):time_periods,])
  #r.pool.rid = sqrt(sum(b^2))
  }
  ## 3- FUSED RIDGE
  if ( "FR" %in% names(est)){
  c.fus.rid <- est[["FR"]]
  int=matrix(data=0, nrow=time_periods, ncol=bands)
  int[,1]=1
  loc=list()
  for (i in 1:bands){
    loc[[i]]=rep(1, time_periods)
  }
  int=bdiag(loc)
  int=as.matrix(int)
  tx=cbind(1, x)
  out[[4]] = tx%*%t(c.fus.rid)
  #c.fus.rid = fused.rid[[3]]
  # b=as.vector(ym[(t0+1):time_periods,]-p.fus.rid[(t0+1):time_periods,])
  # r.fus.rid = sqrt(sum(b^2))
  # r.fus.rid
  }
  ### GAUSSIAN PROCESS
  # if ("GP" %in% names(est)){
  # params=est[["GP"]]
  # mean.gp = vector()
  # gp.beta=vector()
  # for (i in 1:(iter)) {
  #   loc = x_matrix_int(cbind(1,x)) %*% params$gp1[i,  ]
  #   yhat=matrix(NA, nrow=nrow(loc), ncol=1)
  #   for (i in 1:nrow(loc)){
  #     yhat[i]=rnorm(1,loc[i], sqrt(params$sigma[i]))
  #   }
  #   mean.gp=cbind(mean.gp, yhat)
  #   gp.beta=cbind(gp.beta, params$gp1[i,])
  # }
  # ##
  # out[[5]] = matrix(rowMeans(mean.gp), ncol = bands)
  # }
  if ("GP" %in% names(est)){
    params=rstan::extract(est[["GP"]])
    out[[5]]=matrix(apply(params$y_new, 2, median),ncol=bands)
  }
  
  
  ### ols
  if ("OLS" %in% names(est)){
   c.pool.ols = as.matrix(est[[ "OLS"]])
   out[[6]] = matrix(x_matrix(x) %*% c.pool.ols, ncol=bands)
  }
  
  if ("PVN" %in% names(est)){
    out[[7]]=matrix(apply(est[["PVN"]]$y_new, 2, median),ncol=bands)
  }
  
  if ("BVR" %in% names(est)){
    mat=matrix(nrow=nrow(x), ncol=bands)
    for(i in 1:bands){
      mat[,i]=apply(est[["BVR"]][[i]]$y_new,2,median)
    }
    
    out[[8]]=mat
  }
  
  if ("BSC" %in% names(est)){
    mat=matrix(nrow=nrow(x), ncol=bands)
    for(i in 1:bands){
      mat[,i]=apply(est[["BSC"]][[i]]$y_new,2,median)
    }

    out[[9]]=mat
  }

  if ("MGP" %in% names(est)){
    ff=rstan::extract(est[["MGP"]])
    store=vector()
    for(i in 1:bands){
      loc=apply(ff$ynn[,,i],2,median)
      store=cbind(store, loc)
    }
    out[[10]]=store
  }
  
  library(purrr)
  out=compact(out)
  #out = out[-which(sapply(out, is.null))]
  names(out)=names(est)
  
  
  # with posterior predictive probability
  


  
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
