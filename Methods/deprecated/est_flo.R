###############################################################################
######################### COEFFICIENT ESTIMATION ##############################
###############################################################################

## @ sim <- data or simulation matrix
## @ est <- results from estimation function
## @ norm <- normalization of outcomes - pre treatment


estimation_flo<-function(sim,  iter, warm, norm){
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
  pino = matrix(nrow = time_periods, ncol = bands)
  pred = vector()
  tot_bias = vector()
  c.sep.scm = vector()
  
  for (i in 1:bands) {
    v = ym.pre[,i]
    gg = SCM(v, x.pre)
    c.sep.scm=cbind(c.sep.scm, gg)
  }
  
  ## pooled ridge 
  l1=seq(0.01,10,0.001)
  l=tcv_ridge(sim, x , l1, train)
  lam=subset(l, l[,2]==min(l[,2]))
  lam=lam[1,1:2]
  l1=lam[1]; l2=lam[2]
  ridge.r <- ridge(yv.pre, x.pre, l1,SCM = F)
  c.pool.rid <- ridge.r[[3]]
  
  ## 3- FUSED RIDGE
 l1=seq(0.01,10,0.001)
  l2=0
  l=tcv_fused(sim,x,l1,l2, SCM=F,train)
  lam=subset(l, l[,3]==min(l[,3]))
  lam=lam[1,1:2]
  l1=lam[1]; l2=lam[2]
  l1=l1;l2=0
  fused.rid <- fused(yv.pre, x.pre, l1,SCM = F)
  c.fus.rid <- fused.rid[[3]]
  ### 4 - GAUSSIAN PROCESS REGRESSION

  ss_data = list(
    x = x_matrix(x.pre),
    h =as.numeric(scale( treated_radius)),
    y = yv.pre,
    N = (t0 * bands),
    K = (num_controls*bands),
    C = num_controls,
    D = bands
  )
  
  fit <- rstan::stan(
    file = "alpha.stan",
    data = ss_data,
    cores = 1,
    iter = iter,
    chains = 3,
    verbose = F,
    warmup = warm,
    control = list(
      max_treedepth = 10,
      stepsize = 0.05,
      adapt_delta = 0.75
    )
  )
  params <- rstan::extract(fit)
  
  # 6.1 pooled OLS
 # tx=x_matrix(x.pre)
  
 # c.pool.ols = solve(t(tx) %*% tx) %*% t(tx) %*% yv.pre

  
    out=list()
    out[[1]]=c.sep.scm
    out[[2]]=c.pool.rid
    out[[3]]=c.fus.rid
    out[[4]]=params
    out[[5]]=fit
  #  out[[6]]=c.pool.ols
  return(out)

}

