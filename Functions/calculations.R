###############################################################################
################ CALCULATION AFTER COEFFICIENT ESTIMATION #####################
###############################################################################
## @ sim <- data or simulation matrix
## @ est <- results from estimation function
## @ norm <- normalization of outcomes - pre treatment


calculations<-function(sim, est, norm){
  ## Preparing different matrix config for estimation 
  #  pre/post treatment, training
  sim1=sim
  if (norm==T){
    center= apply(sim[1:t0,],2, mean)
    scale = apply(sim[1:t0,], 2, sd)
    for(i in 1:ncol(sim)){
      loc=(sim[,i]-center[i])/scale[i]
      sim[,i]=loc
    }
  }
  yv = as.vector(sim[, 1:bands])
  yv.pre = as.vector(sim[1:t0, 1:bands])
  ym =sim[, 1:bands]
  ym.pre =sim[1:t0, 1:bands]
  x = sim[, ((bands) + 1):(num_controls + bands)]
  x.pre = sim[1:t0, ((bands) + 1):(num_controls + bands)]
  train=round(t0-t0/3)
  y.train = as.vector(sim[1:train, 1:bands])
  x.train = sim[1:train, ((bands) + 1):(num_controls + bands)]
  y.post = (sim[(t0+1):time_periods, 1:bands])
  x.post = sim[(t0+1):time_periods, ((bands) + 1):(num_controls + bands)]
  
  ### 1.0 SEPARATED SCM
  c.sep.scm=as.matrix(est[[1]])
  pino = matrix(nrow = time_periods, ncol = bands)
  pred = vector()
  tot_bias = vector()
  for (i in 1:bands) {
    # v = ym.pre[,i]
    # gg = SCM(v, x.pre)
    # c.sep.scm=cbind(c.sep.scm, gg)
    p1 = x %*% c.sep.scm[,i]
    bias = ym[(t0+1):time_periods,i] - p1[(t0+1):time_periods]
    tot_bias = c(tot_bias, bias)
    pino[, i] = p1
  }
  p.sep.scm = pino
  r.sep.scm = sqrt(sum(tot_bias^ 2))
  
  ## 2- POOLED RIDGE
  c.pool.rid <- est[[2]]
  int=matrix(data=0, nrow=time_periods, ncol=bands)
  int[,1]=1
  loc=list()
  for (i in 1:bands){
    loc[[i]]=rep(1, time_periods)
  }
  int=bdiag(loc)
  int=as.matrix(int)
  tx=cbind(1, x)
  p.pool.rid = tx%*%t(c.pool.rid)
  #c.fus.rid = fused.rid[[3]]
  b=as.vector(ym[(t0+1):time_periods,]-p.pool.rid[(t0+1):time_periods,])
  r.pool.rid = sqrt(sum(b^2))
  ## 3- FUSED RIDGE
  c.fus.rid <- est[[3]]
  int=matrix(data=0, nrow=time_periods, ncol=bands)
  int[,1]=1
  loc=list()
  for (i in 1:bands){
    loc[[i]]=rep(1, time_periods)
  }
  int=bdiag(loc)
  int=as.matrix(int)
  tx=cbind(1, x)
  p.fus.rid = tx%*%t(c.fus.rid)
  #c.fus.rid = fused.rid[[3]]
  b=as.vector(ym[(t0+1):time_periods,]-p.fus.rid[(t0+1):time_periods,])
  r.fus.rid = sqrt(sum(b^2))
  r.fus.rid
### GAUSSIAN PROCESS
  params=est[[4]]
  mean.gp = vector()
  gp.beta=vector()
  for (i in 1:(iter - warm)) {
    loc = x_matrix(x) %*% params$gp1[i, ] + params$delta[i]
    mean.gp=cbind(mean.gp, loc)
    gp.beta=cbind(gp.beta, params$gp1[i,])
  }
  ##
  p.gp = matrix(rowMeans(mean.gp), ncol = bands)
  b=as.vector(ym[(t0+1):time_periods,]-p.gp[(t0+1):time_periods,])
  r.gp = sqrt(sum(b ^ 2))
  
  ### ols
 # c.pool.ols = as.matrix(est[[6]])
 # p.pool.ols = matrix(x_matrix(x) %*% c.pool.ols, ncol=bands)
  
  
  out=list()
  p.naive.pre=matrix(NA, ncol=bands, nrow=time_periods)
  p.naive.con=matrix(NA, ncol=bands, nrow=time_periods)
  p.naive.imp=matrix(NA, ncol=bands, nrow=time_periods)
  if (norm==T){
    for (i in 1:bands){
      p.sep.scm[,i]=(p.sep.scm[,i]*scale[i])+center[i]
      p.pool.rid[,i]=(p.pool.rid[,i]*scale[i])+center[i]
      p.fus.rid[,i]=(p.fus.rid[,i]*scale[i])+center[i]
      p.gp[,i]=(p.gp[,i]*scale[i])+center[i]
  #    p.pool.ols[,i]=(p.pool.ols[,i]*scale[i])+center[i]
      p.naive.pre[,i]=mean(sim1[1:t0,i])
      p.naive.con[,i]=mean(sim1[(t0+1):time_periods, (bands+1):num_controls])
      p.naive.imp[,i]=mean(sim1[(t0+1):time_periods, -c(((num_controls/2)+1):num_controls)])
      }
    
  }
    out[[1]]=p.sep.scm
    out[[2]]=p.pool.rid
    out[[3]]=p.fus.rid
    out[[4]]=p.gp
    out[[5]]=p.naive.pre
    out[[6]]=p.naive.con
   # out[[7]]=p.pool.ols
    out[[8]]=p.naive.imp

  return(out)
  }
