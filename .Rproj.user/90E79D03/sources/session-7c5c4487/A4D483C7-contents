#' Coverage with 95% probability 
#' 
#' @param true the matrix of simulated values
#' @param down is the matrix of 2.5% lower bound of the confidence interval
#' @param up is the matrix of 97.5% upper bound of the confidence interval  
#' 
#' @autor Giulio Grossi
#' 

coverage<-function(true, down, up){
  loc <- vector(length = length(true))
  bands <- ncol(down)
  time_periods <- nrow(true)
  loc <- matrix(nrow=time_periods, ncol=bands)
  for(ii in 1:bands){
  for (tt in 1:time_periods){
    loc[tt, ii]=true[tt, ii]<up[tt, ii] && true[tt, ii]>down[tt, ii]
  }
  }
  loc=matrix(as.numeric(loc), nrow=time_periods, ncol=bands)
  return(loc)
  }
