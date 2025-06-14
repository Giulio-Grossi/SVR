###
# This file contains the code to compute the variance estimators. 
###

#### these files should be used to replicate the results from 
#### Shen et al. (2023) - econometrica



library(Matrix)
###
# Threshold errors (for numerical stability)
###

err_threshold <- function(err, thresh=1e-10){
  err[abs(err)<=1e-10] = 0
  return(err)
}


## pseudo inverse function, the entry matrix should have 
## the number of the units of the rows, and the number of times
## on the columns

pseudo.inv <- function(A){
  r <- rankMatrix(A)[1]
  svd.A <- svd(A)
  
  U <- svd.A$u[,1:r]
  V <- svd.A$v[,1:r]
  if(r>1){
    S <- diag(svd.A$d[1:r])
  }else{
    S<- svd.A$d[1:r]
  }
  
  (V %*% solve(S) %*% t(U))
  
}


###
# Homoskedastic variance estimator (see Section 4.1.3 of the paper for details)
###
var_homo <- function(y_n, y_t, Y0, w_hz, w_vt, Hu_perp, Hv_perp){
  # rank 
  R <- rankMatrix(Y0)[1]
  
  # hz 
  if(R==length(y_t)){
    var_hz = 0
    sigma2_hz = 0 
  }
  else{
    scale = length(y_t) - R
    err_hz = Hu_perp %*% y_t
    err_hz = err_threshold(err_hz)
    sigma2_hz = (1/scale) * sum(err_hz^2) 
    var_hz = sigma2_hz * t(w_vt)%*%w_vt 
  }
  
  # vt 
  if(R==length(y_n)){
    var_vt = 0
    sigma2_vt = 0 
  }
  else{
    scale = length(y_n) - R 
    err_vt = Hv_perp %*% y_n
    err_vt = err_threshold(err_vt)
    sigma2_vt = (1/scale) * sum(err_vt^2)
    var_vt = sigma2_vt * t(w_hz)%*%w_hz
  }
  # interaction  
  
  A = (sigma2_hz * sigma2_vt) * pseudo.inv(Y0)%*%pseudo.inv(t(Y0))
  trA =  sum(diag(A))
  
  list(v.hz =var_hz,v.vt = var_vt, trA=trA)
  
}


###
# Jackknife variance estimator (see online appendix for details)
###

var_jack <- function(y_n, y_t, Y0, w_hz, w_vt, Hu_perp, Hv_perp){
  # rank 
  R <- rankMatrix(Y0)[1]
  
  # hz 
  if( R==length(y_t)){
    var_hz = 0 
    sigma_hz = matrix(0, nrow=length(y_t), ncol=length(y_t))
  }
  else{
    Hu_jack <- Hu_perp*Hu_perp*diag(1,length(y_t))
    H_inv_hz <- pseudo.inv(Hu_jack)
    err_hz = (Hu_perp%*%y_t) * (Hu_perp%*%y_t)
    err_hz = err_threshold(err_hz)
    sigma_hz = diag(as.numeric(H_inv_hz%*%err_hz),nrow=length(y_t))
    var_hz = t(w_vt)%*%sigma_hz%*%w_vt
  }
  
  # vt 
  if(R==length(y_n)){
    var_vt = 0 
    sigma_vt = matrix(0, nrow=length(y_n), ncol=length(y_n))
  }
  else{
    Hv_jack <- Hv_perp*Hv_perp*diag(1,length(y_n))
    H_inv_vt = pseudo.inv(Hv_jack)
    err_vt = (Hv_perp%*%y_n) * (Hv_perp%*%y_n)
    err_vt = err_threshold(err_vt)
    sigma_vt = diag(as.numeric(H_inv_vt%*%err_vt), nrow=length(y_n), ncol=length(y_n) ) 
    var_vt = t(w_hz) %*%sigma_vt %*%w_hz
  }
  
  # interaction 
  
  
  A = pseudo.inv(Y0) %*% sigma_hz %*% pseudo.inv(t(Y0)) %*% sigma_vt
  trA <- sum(diag(A))
  
  list(v.hz =var_hz,v.vt = var_vt, trA=trA)
  
}

###
# HRK variance estimator (see online appendix for details)
###
var_hrk <- function(y_n, y_t, Y0, w_hz, w_vt, Hu_perp, Hv_perp){
  
  # hz 
  H_inv_hz = pseudo.inv((Hu_perp*Hu_perp))
  err_hz = (Hu_perp%*%y_t) * (Hu_perp%*%y_t)
  err_hz = err_threshold(err_hz)
  sigma_hz = diag(as.numeric(H_inv_hz%*%err_hz),  nrow=length(y_t), ncol=length(y_t))
  var_hz = t(w_vt)%*%sigma_hz%*%w_vt
  
  # vt 
  H_inv_vt = pseudo.inv((Hv_perp*Hv_perp))
  err_vt = (Hv_perp%*%y_n) * (Hv_perp%*%y_n)
  err_vt = err_threshold(err_vt)
  sigma_vt =  diag(as.numeric(H_inv_vt%*%err_vt),  nrow=length(y_n), ncol=length(y_n))
  var_vt = t(w_hz)%*%sigma_vt%*%w_hz
  
  # interaction 
  
  A = pseudo.inv(Y0) %*% sigma_hz %*% pseudo.inv(t(Y0)) %*% sigma_vt
  trA <- sum(diag(A))
  
  list(v.hz =var_hz,v.vt = var_vt, trA=trA)
}

###
# Compute variance estimate 
###

compute_var <- function(y_n, y_t, Y0, w_hz, w_vt, Hu_perp, Hv_perp, v_alg){
  if(v_alg=='homoskedastic'){
    v.pred = var_homo(y_n, y_t, Y0, w_hz, w_vt, Hu_perp, Hv_perp)
  }
  if(v_alg=='jackknife'){
    v.pred = var_jack(y_n, y_t, Y0, w_hz, w_vt, Hu_perp, Hv_perp)
  }
  if(v_alg=='HRK'){
    v.pred = var_hrk(y_n, y_t, Y0, w_hz, w_vt, Hu_perp, Hv_perp)
  }
  
  list(v.hz =v.pred$v.hz,v.vt = v.pred$v.vt, trA=v.pred$trA)
}
