#' Synthetic Control
#' 
#' Function to compute synthetic control weights given a V matrix
#' Also implements L2 penalty
#' 
#' @param X0 is a p x n0 matrix
#' @param X1 is a p x 1 vector
#' @param V is a p x p matrix of weights
#' @param pen is l2 penalty level
#' 
#' @autor Jeremy LHour

SCM <- function(v,x){
  n=ncol(x)
  library(LowRankQP)
  P = 2*t(x)%*%x 
  q = t(-2*t(x)%*%v)
  sol = LowRankQP(Vmat=P,dvec=q,Amat=matrix(1, ncol=n),bvec=1,uvec=rep(1,n), method="LU")
  return(sol$alpha)
}