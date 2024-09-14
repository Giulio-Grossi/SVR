#' Calculating separate vertical regression with ridge pen for each band.
#' 
#' @param est is the list of estimation results
#' @param x Matrix Time_periods x (# controls) for 
#' the control units.
#' @param means The mean of treated units used in standardization.
#' @param sds The standard deviation of treated units used in standardization.
#' 
#' return the predicted values for the vertical regression estimation at the
#' data's original scale if means and sds are provided.
#' 
sepSR_calc <- function(estSR, x, means, sds) {
  
  ## setup
  time_periods <- nrow(x)
  bands <- ncol(estSR)
  
  if (is.null(means)) {
    means <- rep(0, bands)
  }
  if (is.null(sds)) {
    sds <- rep(1, bands)
  }
  
  ## calculation  
  out <- matrix(nrow = time_periods, ncol = bands)
  for (i in 1 : bands) {
    out[, i] <- cbind(1, x) %*% estSR[,i]
    out[, i] <- out[, i] * sds[i] + means[i]
  }
  return(out)
}


