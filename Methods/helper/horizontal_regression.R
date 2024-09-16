################################################################################
#' horizontal regression script 
#' 
#' @param x  the matrix of control units, from 1 to X 
#' @param t0 the end of pretreatment period
#' 
#' @output the vector of horizontal regression coefficient
################################################################################

hz_reg <- function(x, t0){

  x0=x[1:t0,]
  x1=x[(t0+1):nrow(x),]
  
  alpha=matrix(nrow=t0, ncol=ncol(x1))
  for(ii in 1:ncol(x1)){
  gg = cv.glmnet(x = t(x0), y = t(x1)[,ii], alpha = 0)  # alpha = 0 specifies ridge.
  l = gg$lambda.min
  best_model <- glmnet(t(x0), t(x1)[,1], alpha = 0, lambda = l, intercept = F)
  alpha[,ii] = as.matrix(coef(best_model))[-1,]
  }
  
  out <- apply(alpha, 1, mean)
  return(out)
  
}
