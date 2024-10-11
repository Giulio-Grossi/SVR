#' Frequentist confidence interval following Shen et al. (2023), Econometrica
#' 
#' @param true the matrix of simulated values
#' @param estimate is the estimated matrix obtained in the calculation step 
#' We use this function just for synthetic control, sep vertical regression
#' and OLS
#' 
#' @autor Giulio Grossi & Alessandra Mattei 
#' 
#'

ci_shen<-function(sim, estimate, bands, t0, means = NULL, sds = NULL){
  data <- t(sim[,(bands+1):ncol(sim)])
  
  if (is.null(means)) means <- rep(0, bands)
  if (is.null(sds)) sds <- rep(1, bands)
  # time indices   
  pre_cols  = data[,1:t0]
  post_cols = data[,(t0+1):ncol(data)]
  N0 <- nrow(data)
  T0 <- t0
  TT <- ncol(data)
  
  upper<-matrix(ncol=ncol(estimate), nrow=nrow(estimate))
  lower<-matrix(ncol=ncol(estimate), nrow=nrow(estimate))
  for(i in 1:ncol(estimate)){
    
    Y0_obs  = as.matrix(t(sim[1:t0, (bands+1):ncol(sim)]))
    y_n_obs = sim[1:t0, i]
    y_t_obs = as.matrix(t(sim[(t0+1):nrow(sim), (bands+1):ncol(sim)]))
    #y_t_obs <- y_t_obs[,1] #only 1990
    
    alpha <- pseudo.inv(Y0_obs)%*%y_t_obs #HR using the pseudo inverse of Y0 = U %*% S^{-1} V
    #lm(y_t_obs~Y0_obs-1)$coef   #HR
    beta <- pseudo.inv(t(Y0_obs))%*%(y_n_obs)
    ## same as  lm(y_n_obs~t(Y0_obs)-1)$coef  because T0>N0
    ### preparing the matrix for the actual estimation of the variance
    
    svd.y0.obs <- svd(Y0_obs)
    r <- rankMatrix(Y0_obs)[1]
    U <- svd.y0.obs$u
    V <- svd.y0.obs$v
    S <- diag(svd.y0.obs$d)
    
    Hu <- U[, 1:r]%*%t(U[, 1:r])
    Hu_perp <- diag(1,N0)-Hu
    Hv <- V[, 1:r]%*% t(V[, 1:r]) 
    Hv_perp <- diag(1,T0)-Hv
    
    # uncertainty estimation via homoskedastic-based confidence intervals
    v.homo <- var_homo(y_n_obs, y_t_obs, Y0_obs, alpha, beta, Hu_perp, Hv_perp)
    var.hz <- v.homo$v.hz
    var.vt <- v.homo$v.vt
    v0.vt <- diag(var.vt)
    se_treated <-c(rep(0, t0),sqrt(v0.vt))
    
    for(tt in (t0+1):ncol(data)){
    upper[,i] = estimate[,i] + 1.96*se_treated[tt] ## Re-standardize
    lower[,i] = estimate[,i] - 1.96*se_treated[tt]
    }
    
    upper[,i] <- upper[,i]*sds[i] + means[i]
    lower[,i] <- lower[,i]*sds[i] + means[i]
    
  }

out=list(lower, upper) 
names(out)=c("lower_bound","upper_bound" )
return(out)  
}
