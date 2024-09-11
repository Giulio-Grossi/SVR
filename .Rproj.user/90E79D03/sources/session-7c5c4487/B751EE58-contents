#' Bayesian confidence interval
#' 
#' @param est the list of posterior distributions from bayes methods
#' We use this function just for bayesian vertical reg, bayesian synthetic 
#' control
#' 
#' @autor Giulio Grossi
#' 


ci_bayes <- function(sim, est, method, norm){
  
  if (norm == T) {
    sim_std <- preset(sim, t0)
    sim <- sim_std$std
    means <- sim_std$means
    sds <- sim_std$sds
  }
  
  lower=matrix(nrow=time_periods, ncol=bands)
  upper=matrix(nrow=time_periods, ncol=bands)
  
  for(i in 1:bands){
    loc <- est[[method]][[i]]$y_new
    for(hh in 1:nrow(loc)){
      loc[hh,]=(loc[hh,]*sds[i])+means[i]
    }	  
    lower[,i]<-apply(loc,2,quantile, prob=0.025)
    upper[,i]<-apply(loc,2,quantile, prob=0.975)
  }
  
  out=list(lower, upper) 
  names(out)=c("lower_bound","upper_bound" )
  return(out)  
}
