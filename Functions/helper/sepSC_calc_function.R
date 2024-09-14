#' Calculating separate synthetic control for each band.
#' 
#' @param est is the list of estimation results
#' @param x Matrix Time_periods x (# controls) for 
#' the control units.
#' @param means The mean of treated units used in standardization.
#' @param sds The standard deviation of treated units used in standardization.
#' 
#' return the predicted values for the synthetic control estimation at the
#' data's original scale if means and sds are provided.
#' 
sepSC_calc <- function(estSC, x, means, sds) {
  
  # setup
  time_periods <- nrow(x)
  bands <- ncol(estSC)
  
  # calculation  
  out <- matrix(nrow = time_periods, ncol = bands)
  for (i in 1 : bands) {
    out[, i] <- x %*% estSC[, i]
    out[, i] <- out[, i] * sds[i] + means[i]
  }
  
  return(out)
}
