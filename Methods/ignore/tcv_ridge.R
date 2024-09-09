################################################################################
#### FUNCTION TO CALCULATE THE CROSS VALIDATION FUSED VERSION OF SCM ###########
################################################################################

# @y: target vector, dims=c(times*bandwiths)
# @tx: ordered version of the x matrix
# @l1: sequence of shrinkage penalty term
# @ SCM=T: performs the fused SCM 
# @ train: length of the cross validation period

tcv_ridge <- function(num_points, x, l1, train) {
  # data preparation for cross validation 
  # sample the control units for cross validation
  print("k")
  y=as.vector(num_points[1:train,1:bands])
  print("bro")
  y.test=as.vector(num_points[(train+1):t0,1:bands])
  
  x_train=x[1:train,]
  x_test=x[(train+1):t0,]
  tx=x_matrix(x_train)
  tx1=x_matrix(x_test)
  
  N = t0 # number of time points
  D = bands  # number of bandwidhts
  C = ncol(x) # number of control units
  
  
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
  
  
  lstar=vector()
  bias=vector()
  
  int=matrix(data=0, nrow=train, ncol=bands)
  int[,1]=1
  loc=list()
  for (i in 1:bands){
    loc[[i]]=rep(1, train)
  }
  int=bdiag(loc)
  int=as.matrix(int)
  tx=cbind(int, tx)
  tx = as.matrix(tx)
  
  ## set intercett for test matrix 
  
  int=matrix(data=0, nrow=t0-train, ncol=bands)
  int[,1]=1
  loc=list()
  for (i in 1:bands){
    loc[[i]]=rep(1, t0-train)
  }
  int=bdiag(loc)
  int=as.matrix(int)
  tx1=cbind(int, tx1)
  tx1 = as.matrix(tx1)
  
  
  
  for(h in 1:length(l1)){
    loc=vector()
    
      
      l11 = l1[h]
    
      
      # if (SCM == T) {
      #   ## full quadratic model - to investigate different patterns across ridge and fusion
      #   
      #   #x0 = 2 * (t(tx) %*% tx + l22 * t(d) %*% d + l11 * diag(ncol(tx)))
      #   
      #   ## simplified quadratic model - faster
      #   
      #   x0 =   (t(tx) %*% tx + l11 * (t(d) %*% d + diag(ncol(tx))))
      #   q = as.matrix(t(-2 * t(tx) %*% y[1:(train*D)]))
      #   
      #   # costruisco la matrice A delle equality constraints
      #   
      #   c = matrix(0, nrow = bands, ncol = bands * num_controls)
      #   for (j in 1:bands) {
      #     for (i in 1:num_controls) {
      #       c[j, (j + num_controls * (i - 1))] = 1
      #     }
      #   }
      #   
      #   sol = LowRankQP(
      #     Vmat = x0,
      #     dvec = q,
      #     Amat = c,
      #     bvec = rep(1, bands),
      #     uvec = rep(1, bands * num_controls),
      #     method = "LU"
      #   )
      #   p.fus.scm = tx1 %*% sol$alpha
      #   
      #   r.fus.scm = sqrt(sum((y.test - p.fus.scm) ^ 2))
      #   r.fus.scm= c(l1[j],l2[k], r.fus.scm)
      #   bias=rbind(bias, r.fus.scm)
      # } else{
      
      ## full quadratic model - to investigate different patterns across ridge and fusion
      
      x0 = (t(tx) %*% tx  + l11 * diag(ncol(tx)))
      
      ## simplified quadratic model - faster
      
      #x0 =   (t(tx) %*% tx + l11 * (t(d) %*% d + diag(ncol(tx))))
      q = as.matrix(t(t(tx) %*% y[1:(train*bands)]))
      c.fus.rid = solve(x0) %*% t(q)
      p.fus.rid = tx1 %*% c.fus.rid
      r.fus.rid = sqrt(sum((y.test - p.fus.rid) ^ 2))
      p.fus.rid = matrix(p.fus.rid, ncol = bands)
      r.fus.rid = c(l1[h], r.fus.rid)
      bias=rbind(bias, r.fus.rid)
      
      #      } #livello else
    
  } #livello l1
  return(bias)
  
}