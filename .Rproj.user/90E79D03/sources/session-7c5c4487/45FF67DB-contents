#' Frequentist confidence interval following Shen et al. (2023), Econometrica
#' 
#' @param true the matrix of simulated values
#' @param estimate is the estimated matrix obtained in the calculation step 
#' We use this function just for synthetic control, sep vertical regression
#' and OLS
#' 
#' @autor Giulio Grossi
#' 
#'

ci_shen<-function(true, estimate){
  
error = true - estimate
se_treated=apply(error,2, sd)
upper<-matrix(ncol=ncol(estimate), nrow=nrow(estimate))
lower<-matrix(ncol=ncol(estimate), nrow=nrow(estimate))
for(i in 1:ncol(estimate)){
  upper[,i] = estimate[,i] + 1.96*se_treated[i]
  lower[,i] = estimate[,i] - 1.96*se_treated[i]
}

out=list(lower, upper) 
names(out)=c("lower_bound","upper_bound" )
return(out)  
}
