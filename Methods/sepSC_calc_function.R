#' Calculating separate synthetic control for each band.
#' 
#' @param est is the list of estimation results
#' @param x Matrix Time_periods x (# controls) for 
#' the control units.
#' @param means The mean of treated untis used in standardization. If left
#' NULL defaults to a vector of 0s.
#' @param sds The standard deviation of treated units used in standardization.
#' If left NULL defaults to a vector of 1s.
#' 
#' return the predicted values for the synthetic control estimation at the
#' data's original scale if means and sds are provided.
#' 
sepSC_calc <- function(estSC, x, means = NULL, sds = NULL) {
  
  # setup
  time_periods <- nrow(x)
  bands <- ncol(estSC)
  
  if (is.null(means)) {
    means <- rep(0, bands)
  }
  if (is.null(sds)) {
    sds <- rep(1, bands)
  }
  
  # calculation  
  out <- matrix(nrow = time_periods, ncol = bands)
  for (i in 1 : bands) {
    out[, i] <- x %*% estSC[, i]
    out[, i] <- out[, i] * sds[i] + means[i]
  }
  
  return(out)
}
