################################################################################
########### FUNCTION TO CALCULATE THE FUSED VERSION OF SCM #####################
################################################################################

# @y: target vector, dims=c(times*bandwiths)
# @tx: ordered version of the x matrix
# @l1: ridge penalty term

ridge <- function(y, x, l1, SCM) {
  #tx=x_matrix(x.pre)
  tx=x_matrix(x)
  D<-bands
  C<-num_controls
  N<-t0
  library(Matrix)
  
  d = matrix(0, ncol = D, nrow = D)
  for (i in 2:(D)) {
    d[1, 1] = 1
    d[i, i] = 1
    d[i, (i - 1)] = -1
  }
  alpha_d=matrix(0, ncol=bands, nrow=bands)
  d_list = list()
  d_list[[1]]=alpha_d
  
  for (i in 2:(C+1)) {
    d_list[[i]] = d
  }
  f = bdiag(d_list)
  d = as.matrix(f)
  ## adding intercepts
  int=matrix(data=0, nrow=t0, ncol=bands)
  int[,1]=1
  loc=list()
  for (i in 1:bands){
    loc[[i]]=rep(1, t0)
  }
  int=bdiag(loc)
  int=as.matrix(int)
  tx=cbind(int, tx)
  
  
  if (SCM == T) {
    x0 = (t(tx) %*% tx + l11 * diag(ncol(tx)) )
    #x0 = 2 * (t(tx) %*% tx + l2 * t(d) %*% d)
    # costruisco la matrice q
    q = as.matrix(t(-2 * t(tx) %*% as.vector(y)))
    
    # costruisco la matrice A delle equality constraints
    
    c = matrix(0, nrow = D, ncol = D * C)
    for (j in 1:D) {
      for (i in 1:C) {
        c[j, (j + D * (i - 1))] = 1
      }
    }
    
    
    # trovo i beta complessivi
    
    sol = LowRankQP(
      Vmat = x0,
      dvec = q,
      Amat = c,
      bvec = rep(1, D),
      uvec = rep(1, C * D),
      method = "LU"
    )
    beta = round(matrix(sol$alpha, nrow = D), 2)
    gamma = round(matrix(sol$alpha, nrow = D), 2)
    beta
    
    
    p.fus.scm = matrix(tx %*% sol$alpha, ncol = D)
    r.fus.scm = sqrt(sum((yv - p.fus.scm) ^ 2))
    c.fus.scm = matrix(beta, ncol = C)
    res = list(p.fus.scm, r.fus.scm, c.fus.scm)
  } else{
    ## full quadratic version - if we wish to investigate different penalization terms for 
    ## ridge and fused penalty
    x0 = (t(tx) %*% tx + l1 * diag(ncol(tx)))
    ## simplified version - faster 
    # x01 =  (t(tx) %*% tx + l1 * (diag(ncol(tx)) + t(d) %*% d))
    # x01 =  (t(tx) %*% tx + l1 * (t(d) %*% d))
    q = as.matrix(t(t(tx) %*% y))
    c.fus.rid = solve(x0) %*% t(q)
    p.fus.rid = tx %*% c.fus.rid
    r.fus.rid = sqrt(sum((y - p.fus.rid) ^ 2))
    p.fus.rid = matrix(p.fus.rid, ncol = D)
    c.fus.rid = matrix(c.fus.rid, ncol = (C+1))
    round(matrix(c.fus.rid, ncol = (C+1)), 2)
    res = list(p.fus.rid, r.fus.rid, c.fus.rid)
    
  }
  return(res)
}