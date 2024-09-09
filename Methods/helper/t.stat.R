


t.stat<-function(sim, cal){
  
  ## data prep
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
  p.sep.scm <- cal[[1]]
  p.pool.rid <- cal[[2]]
  p.fus.rid <- cal[[3]]
  p.gp <- cal[[4]]
  p.naive.pre <-cal[[5]]
  p.naive.con <- cal[[6]]
#  p.pool.ols <-cal[[7]]
  p.naive.imp<-cal[[8]]
  
  res=vector()
    for (i in 1:bands) {
      loc = c(
        mean(ym[1:t0, i] -p.sep.scm[1:t0, i]),
        mean(ym[1:t0, i] -p.pool.rid[1:t0, i] ),
        mean(ym[1:t0, i] -p.fus.rid[1:t0, i] ),
        mean(ym[1:t0, i] -p.gp[1:t0, i]),
        mean(ym[1:t0, i] -p.naive.pre[1:t0, i]),
        mean(ym[1:t0, i] -p.naive.con[1:t0, i]),
 #       mean(ym[1:t0, i] -p.pool.ols[1:t0, i]),
        mean(ym[1:t0, i] -p.naive.imp[1:t0, i])
      )
      res = cbind(res, loc)
    }
    post_res = vector()
    for (i in 1:bands) {
      loc = c(
        mean(ym[(t0+1):time_periods, i] - p.sep.scm[(t0+1):time_periods, i]),
        mean(ym[(t0+1):time_periods, i] - p.pool.rid[(t0+1):time_periods, i]),
        mean(ym[(t0+1):time_periods, i] -p.fus.rid[(t0+1):time_periods, i] ),
        mean(ym[(t0+1):time_periods, i] -p.gp[(t0+1):time_periods, i]),
        mean(ym[(t0+1):time_periods, i] -p.naive.pre[(t0+1):time_periods, i]),
        mean(ym[(t0+1):time_periods, i] -p.naive.con[(t0+1):time_periods, i]),
  #      mean(ym[(t0+1):time_periods, i] -p.pool.ols[(t0+1):time_periods, i]),
        mean(ym[(t0+1):time_periods, i] -p.naive.imp[(t0+1):time_periods, i])
        )
      post_res = cbind(post_res, loc)
    }
    rownames(res) = c(
      "Separate SCM",
      "Pooled Ridge",
      "Fused Ridge",
      "GP",
      "Naive (pre T)", 
      "Naive (Controls)", 
   #   "OLS",
      "Naive (Important)"
      )
    colnames(res)=c(1:bands)
    rownames(post_res) = c(
      "Separate SCM",
      "Pooled Ridge",      
      "Fused Ridge",
      "GP",
      "Naive (pre T)", 
      "Naive (Controls)",
    #  "OLS",
      "Naive (Important)"
      )
    colnames(post_res)=c(1:bands)
    ## root mean square prediction error - pre 
      r1 = c(
        sum(abs(ym[1:t0, ] -p.sep.scm[1:t0, ])),
        sum(abs(ym[1:t0, ] -p.pool.rid[1:t0, ])),
        sum(abs(ym[1:t0, ] -p.fus.rid[1:t0, ])),
        sum(abs(ym[1:t0, ] -p.gp[1:t0, ])),
        sum(abs(ym[1:t0, ] -p.naive.pre[1:t0, ])),
        sum(abs(ym[1:t0, ] -p.naive.con[1:t0, ])),
     #   sum(abs(ym[1:t0, ] -p.pool.ols[1:t0, ])),
        sum(abs(ym[1:t0, ] -p.naive.imp[1:t0, ]))
        )/t0
      r2 = c(
        sum(abs(ym[(t0+1):time_periods,] -p.sep.scm[(t0+1):time_periods,])),
        sum(abs(ym[(t0+1):time_periods,] -p.pool.rid[(t0+1):time_periods,])),
        sum(abs(ym[(t0+1):time_periods,] -p.fus.rid[(t0+1):time_periods,])),
        sum(abs(ym[(t0+1):time_periods,] -p.gp[(t0+1):time_periods,])),
        sum(abs(ym[(t0+1):time_periods,] -p.naive.pre[(t0+1):time_periods,])),
        sum(abs(ym[(t0+1):time_periods,] -p.naive.con[(t0+1):time_periods,])),
      #  sum(abs(ym[(t0+1):time_periods,] -p.pool.ols[(t0+1):time_periods,])),
        sum(abs(ym[(t0+1):time_periods,] -p.naive.imp[(t0+1):time_periods,]))
      )/(time_periods-t0)

    names(r1) = rownames(res)
    names(r2) = rownames(res)
    r3=vector();r4=vector()
    for(i in 1:bands){
      loc1 = c(
        sum(abs(ym[1:t0,i ] -p.sep.scm[1:t0,i ])),
        sum(abs(ym[1:t0,i ] -p.pool.rid[1:t0,i ])),
        sum(abs(ym[1:t0,i ] -p.fus.rid[1:t0,i ])),
        sum(abs(ym[1:t0,i ] -p.gp[1:t0,i ])),
        sum(abs(ym[1:t0,i ] -p.naive.pre[1:t0,i ])),
        sum(abs(ym[1:t0,i ] -p.naive.con[1:t0,i ])),
       # sum(abs(ym[1:t0,i ] -p.pool.ols[1:t0,i ])),
        sum(abs(ym[1:t0,i ] -p.naive.imp[1:t0,i ]))
              )/t0
      loc2 = c(
        sqrt(sum((ym[(t0+1):time_periods,i] -p.sep.scm[(t0+1):time_periods,i])^2)),
        sqrt(sum((ym[(t0+1):time_periods,i] -p.pool.rid[(t0+1):time_periods,i])^2)),
        sqrt(sum((ym[(t0+1):time_periods,i] -p.fus.rid[(t0+1):time_periods,i])^2)),
        sqrt(sum((ym[(t0+1):time_periods,i] -p.gp[(t0+1):time_periods,i])^2)),
        sqrt(sum((ym[(t0+1):time_periods,i] -p.naive.pre[(t0+1):time_periods,i])^2)),
        sqrt(sum((ym[(t0+1):time_periods,i] -p.naive.con[(t0+1):time_periods,i])^2)),
       # sqrt(sum((ym[(t0+1):time_periods,i] -p.pool.ols[(t0+1):time_periods,i])^2)),
        sqrt(sum((ym[(t0+1):time_periods,i] -p.naive.imp[(t0+1):time_periods,i])^2))
      )/(time_periods-t0)
      r3=cbind(r3, loc1)
      r4=cbind(r4, loc2)
    }

    rownames(r3) = rownames(res)
    rownames(r4) = rownames(res)
    colnames(r3) = c(1:bands)
    colnames(r4) = c(1:bands)

    bias=round(cbind(res,post_res),3)
    error=round(cbind(r1,r2,r3,r4),3)
    
    ####### bias calculation per each 
    v_bias=vector()
    for (i in c(1:6,8)){
      loc=cal[[i]]
      loc2=t(t(ym-loc)/ym[1,])
      v_bias=cbind(v_bias,rowSums(sqrt(loc2^2)))
    }
    
    ## graphs
    # dev.new()
    # par(mfrow=c(round(bands/4), 4))
    # 
    # 
    # 
    # pl.res=list()
    #   for(i in 1:bands){
    #     loc=cbind(ym[,i], p.sep.scm[,i], p.pool.rid[,i], p.fus.rid[,i], p.gp[,i], p.naive.pre[,i], p.pool.ols[,i], p.naive.imp[,i])
    #     colnames(loc)=c("True", "SCM", "Ridge", "Fused Ridge", "Gaussian Process", "Naive pre", "OLS",      "Naive (Important)")
    #     pl.res[[i]]=loc
    #   }
    # for (i in 1:bands){
    #   plot(pl.res[[i]][,1], ylim=range(pl.res[[i]], na.rm=T), type="l",
    #        col="black", xlab="Time", ylab="Error")
    #   lines(pl.res[[i]][,2], col="blue")
    #   lines(pl.res[[i]][,3], col="green")
    #   lines(pl.res[[i]][,4], col="red")
    #   lines(pl.res[[i]][,5], col="cyan")
    #   lines(pl.res[[i]][,6], col="brown")
    #   lines(pl.res[[i]][,6], col="coral")
    #   abline(v=t0, col="darkgreen")
    # }
    par(mfrow=c(1,1))
    out=list()
    out[[1]] <- bias
    out[[2]] <- error
    out[[3]] <-v_bias
    return(out)
}

