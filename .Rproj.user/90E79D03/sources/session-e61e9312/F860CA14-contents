###############################################################################
################ MSE-BIAS AFTER COEFFICIENT ESTIMATION #####################
###############################################################################
## @ sim <- data or simulation matrix
## @ cal <- results from calculation function


point_estimate <- function(sim, cal) {
  
  # The bias is the prediction minus the true value.
  bias <- list()
  # I call this squared error because this is not the MEAN squared error.
  SqE <- list()
  
  # All elements in cal have the bands as columns, so we can get bands once.
  bands <- ncol(cal[[1]])
  
  # Loop over the predictions from the different methods. They are all in the
  # form of matrix with rows being time periods and columns being treated
  # units.
  
  for (ii in 1 : length(cal)) {
    
    bias[[ii]] <- cal[[ii]] - sim[, 1 : bands]
    SqE[[ii]] <- bias[[ii]] ^ 2
    
  }
  
  names(bias) <- names(cal)
  names(SqE) <- names(cal)
  
  out <- list(bias = bias, SqE = SqE)
  
  return(out)
}
