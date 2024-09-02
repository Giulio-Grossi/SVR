x_matrix<-function(x){
  tx = vector()
  for (j in 1:bands) {
    rad = vector()
    for (i in 1:num_controls) {
      loc = cbind(matrix(0, ncol = (j - 1), nrow = nrow(x)),
                  x[, i],
                  matrix(0, ncol = (bands - j), nrow = nrow(x)))
      rad = cbind(rad, loc)
    }
    tx = rbind(tx, rad)
  }
  dim(tx)
  return(tx)
}