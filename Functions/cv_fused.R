################################################################################
########### FUNCTION TO CALCULATE THE FUSED VERSION OF SCM #####################
################################################################################

# @y: target vector, dims=c(times*bandwiths)
# @tx: ordered version of the x matrix
# @lambda: sequence of ridge penalty term
# @ SCM=T: performs the fused SCM 
# @ B: number of k-fold cross-validation



cv_fused <- function(y, x, lambda, SCM, B) {
  # data preparation for cross validation 
  # sample the control units for cross validation
  res=matrix(nrow=length(lambda), ncol=B)
  
  
  # First cycle in the number of Cross validation B
  for (b in 1:B){
    samp=sample(1:ncol(x),(round(ncol(x)/3))*2)
    loc_list = list()
    tx = vector()
    for (j in 1:bands) {
      rad = vector()
      for (i in 1:length(samp)) {
        loc = cbind(matrix(0, ncol = (j - 1), nrow = nrow(x)),
                    x[, samp[i]],
                    matrix(0, ncol = (bands - j), nrow = nrow(x)))
        rad = cbind(rad, loc)
      }
      tx = rbind(tx, rad)
    }
    dim(tx)
    
    N = t0 # number of time points
    D = bands  # number of bandwidhts
    C = length(samp) # number of control units
    
    
    library(Matrix)
    # d = matrix(0, ncol = D, nrow = D)
    # for (i in 1:(D - 1)) {
    #   d[i, i] = -1
    #   d[i, (i + 1)] = 1
    #   d[D, D] = 1
    # }
    d = matrix(0, ncol = D, nrow = D)
    for (i in 2:(D)) {
      d[1, 1] = 1
      d[i, i] = 1
      d[i, (i - 1)] = -1
    }
    d_list = list()
    for (i in 1:C) {
      d_list[[i]] = d
    }
    f = bdiag(d_list)
    bias=vector()
    for (k in 1:length(lambda)){
      l1 = 0
      l2 = lambda[k]
      d = as.matrix(f)
      tx = as.matrix(tx)
      
      if (SCM == T) {
        x0 = 2 * (t(tx) %*% tx + l1 * diag(ncol(tx)) + l2 * t(d) %*% d)
        x0 = 2 * (t(tx) %*% tx + l2 * t(d) %*% d)
        # costruisco la matrice q
        q = as.matrix(t(-2 * t(tx) %*% y))
        
        # costruisco la matrice A delle equality constraints
        
        c = matrix(0, nrow = D, ncol = D * C)
        for (j in 1:D) {
          for (i in 1:C) {
            c[j, (j + D * (i - 1))] = 1
          }
        }

        sol = LowRankQP(
          Vmat = x0,
          dvec = q,
          Amat = c,
          bvec = rep(1, D),
          uvec = rep(1, C * D),
          method = "LU"
        )
        p.fus.scm = matrix(tx %*% sol$alpha, ncol = D)
        r.fus.scm = sqrt(sum((y - p.fus.scm) ^ 2))
        bias=c(bias, r.fus.scm)
      } else{
        x0 = 2 * (t(tx) %*% tx + l2 * t(d) %*% d)
        q = as.matrix(t(-2 * t(tx) %*% y))
        c.fus.rid = solve(x0) %*% -t(q)
        p.fus.rid = tx %*% c.fus.rid
        r.fus.rid = sqrt(sum((y - p.fus.rid) ^ 2))
        p.fus.rid = matrix(p.fus.rid, ncol = D)
        bias=c(bias, r.fus.rid)
    }
    }
    res[,b]=bias
    }
  
star=rowSums(res)
lstar=cbind(lambda,star)
l=min(lstar[,2])
lambdas=subset(lstar, lstar[,2]==l)
  
  
  return(lambdas)
  
}


# d = matrix(0, ncol = D, nrow = D)
# for (i in 2:(D)) {
#   d[1, 1] = 1
#   d[i, i] = 1
#   d[i, (i - 1)] = -1
# }
# d
