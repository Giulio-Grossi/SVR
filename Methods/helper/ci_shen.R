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

ci_shen<-function(sim, estimate, bands, t0){
x <- sim[,(bands+1):ncol(sim)]
true <- sim[,1:bands]
alpha <- hz_reg(x, t0)
n_control <- ncol(y0)
alpha <- as.matrix(alpha)
error <- true - estimate
r <- min(dim(y0))
sigma_v<- (1/(t0 - r))*(diag(t0)*sum(error^2)) 

upper<-matrix(ncol=ncol(estimate), nrow=nrow(estimate))
lower<-matrix(ncol=ncol(estimate), nrow=nrow(estimate))
for(i in 1:ncol(estimate)){
  v0=t(alpha)%*%sigma_v%*%(alpha)
  se_treated=sqrt(v0)
  upper[,i] = estimate[,i] + 1.96*se_treated[i]
  lower[,i] = estimate[,i] - 1.96*se_treated[i]
}

out=list(lower, upper) 
names(out)=c("lower_bound","upper_bound" )
return(out)  
}
