
estimation<-function(sim,  iter, warm){
  library(rstan)
  ## Preparing different matrix config for estimation 
  #  pre/post treatment, training
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
  scm.coef = vector()
  for (i in 1:bands) {
    v = ym.pre[,i]
    gg = SCM(v, x.pre)
    p1 = x %*% gg
    bias = ym[(t0+1):time_periods,i] - p1[(t0+1):time_periods]
    tot_bias = c(tot_bias, bias)
    pino[, i] = p1
  }
  p.sep.scm = pino
  r.sep.scm = sqrt(sum(tot_bias^ 2))
  
  if (t0>num_controls){
    # 1.1 pooled OLS
    tx=x_matrix(x.pre)
    
    c.pool.ols = solve(t(tx) %*% tx) %*% t(tx) %*% yv.pre
    p.pool.ols = matrix(x_matrix(x) %*% c.pool.ols, ncol=bands)
    
    b=as.vector(ym[(t0+1):time_periods,]-p.pool.ols[(t0+1):time_periods,])
    r.pool.ols = sqrt(sum(b^2))
    # 1.2 separate OLS
    p = matrix(nrow = time_periods, ncol = bands)
    pred = vector()
    tot_bias = vector()
    ols.coef = vector()
    for (i in 1:bands) {
      v = ym.pre[,i]
      lmfit = lm(v ~ -1 + x.pre)
      p1 = x %*% lmfit$coefficients
      bias = ym[(t0+1):time_periods,i] - p1[(t0+1):time_periods]
      pred = cbind(pred,p1)
      tot_bias = c(tot_bias, bias)
      ols.coef = cbind(ols.coef, lmfit$coefficients)
    }
    
    c.sep.ols = matrix(ols.coef, ncol = num_controls)
    p.sep.ols = pred
    r.sep.ols = sqrt(sum(tot_bias ^ 2))
    
    tx=x_matrix(x.pre)    
  }
  ## 3- FUSED RIDGE
  l1=seq(0.0001,10000,100)
  l2=0
  l=tcv_fused(sim,x,l1,l2, SCM=F,train)
  lam=subset(l, l[,3]==min(l[,3]))
  lam=lam[1,1:2]
  l1=lam[1]; l2=lam[2]
  l1=l1;l2=0
  fused.rid <- fused(yv.pre, x.pre, l1, l2,SCM = F)
  
  
  int=matrix(data=0, nrow=time_periods, ncol=bands)
  int[,1]=1
  loc=list()
  for (i in 1:bands){
    loc[[i]]=rep(1, time_periods)
  }
  int=bdiag(loc)
  int=as.matrix(int)
  tx=cbind(1, x)
  p.fus.rid = tx%*%t(fused.rid[[3]])
  #c.fus.rid = fused.rid[[3]]
  b=as.vector(ym[(t0+1):time_periods,]-p.fus.rid[(t0+1):time_periods,])
  r.fus.rid = sqrt(sum(b^2))
  r.fus.rid
  ### 4 - GAUSSIAN PROCESS REGRESSION
  h=vector(length = bands)
  h[1]=0.05
  for ( i in 2:bands){
    h[i]=h[(i-1)]+0.05 
  }
  ss_data = list(
    x = x_matrix(x.pre),
    h = h,
    y = yv.pre,
    N = (t0 * bands),
    K = (num_controls*bands),
    C = num_controls,
    D = bands
  )
  fit <- rstan::stan(
    file = "gp_model.stan",
    data = ss_data,
    cores = 3,
    iter = iter,
    chains = 3
    ,
    verbose = T,
    warmup = warm
    ,
    control = list(
      max_treedepth = 15,
      stepsize = 0.05,
      adapt_delta = 0.90
    )
  )
  params <- rstan::extract(fit)
  mean.gp = vector()
  gp.beta=vector()
  for (i in 1:(iter - warm)) {
    loc = x_matrix(x) %*% params$gp1[i, ] + params$delta[i]
    mean.gp=cbind(mean.gp, loc)
    gp.beta=cbind(gp.beta, params$gp1[i,])
  }
  ##
  mm.gp = matrix(rowMeans(mean.gp), ncol = bands)
  b=as.vector(ym[(t0+1):time_periods,]-mm.gp[(t0+1):time_periods,])
  r.gp = sqrt(sum(b ^ 2))
  res = vector()
  if(t0>num_controls){
    for (i in 1:bands) {
      loc = c(
        mean(ym[1:t0, i] - p.sep.scm[1:t0, i]),
        mean(ym[1:t0, i] -p.fus.rid[1:t0, i] ),
        mean(ym[1:t0, i] -p.pool.ols[1:t0, i]),
        mean(ym[1:t0, i] -p.sep.ols[1:t0, i]),
        mean(ym[1:t0, i] -mm.gp[1:t0, i])
      )
      res = cbind(res, loc)
    }
    post_res = vector()
    for (i in 1:bands) {
      loc = c(
        mean(ym[(t0+1):time_periods, i] - p.sep.scm[(t0+1):time_periods, i]),
        mean(ym[(t0+1):time_periods, i] -p.fus.rid[(t0+1):time_periods, i] ),
        mean(ym[(t0+1):time_periods, i] -p.pool.ols[(t0+1):time_periods, i]),
        mean(ym[(t0+1):time_periods, i] -p.sep.ols[(t0+1):time_periods, i]),
        mean(ym[(t0+1):time_periods, i] -mm.gp[(t0+1):time_periods, i])
      )
      post_res = cbind(post_res, loc)
    }
    rownames(res) = c(
      "Separate SCM",
      "Fused Ridge",
      "Pooled OLS",
      "Separate OLS",
      "GP")
    colnames(res)=c(1:bands)
    rownames(post_res) = c(
      "Separate SCM",
      "Fused Ridge",
      "Pooled OLS",
      "Separate OLS",
      "GP")
    colnames(post_res)=c(1:bands)
    r = c(
      r.sep.scm,
      r.fus.rid, r.pool.ols, r.sep.ols, r.gp)
    names(r) = rownames(res)
    
    out=cbind(res,post_res, r)
  }else{
    for (i in 1:bands) {
      loc = c(
        mean(ym[1:t0, i] - p.sep.scm[1:t0, i]),
        mean(ym[1:t0, i] -p.fus.rid[1:t0, i] ),
        mean(ym[1:t0, i] -mm.gp[1:t0, i])
      )
      res = cbind(res, loc)
    }
    post_res = vector()
    for (i in 1:bands) {
      loc = c(
        mean(ym[(t0+1):time_periods, i] - p.sep.scm[(t0+1):time_periods, i]),
        mean(ym[(t0+1):time_periods, i] -p.fus.rid[(t0+1):time_periods, i] ),
        mean(ym[(t0+1):time_periods, i] -mm.gp[(t0+1):time_periods, i])
      )
      post_res = cbind(post_res, loc)
    }
    rownames(res) = c(
      "Separate SCM",
      "Fused Ridge",
      "GP")
    colnames(res)=c(1:bands)
    rownames(post_res) = c(
      "Separate SCM",
      "Fused Ridge",
      "GP")
    colnames(post_res)=c(1:bands)
    r = c(
      r.sep.scm,
      r.fus.rid, r.gp)
    names(r) = rownames(res)
    
    out=cbind(res,post_res, r)
  }
  pred=list()
  pred[[1]]=p.sep.scm
  pred[[2]]=p.fus.rid
  pred[[3]]=mm.gp
  res=list()
 res[[1]]=pred
 res[[2]]=out
  return(res)
}

