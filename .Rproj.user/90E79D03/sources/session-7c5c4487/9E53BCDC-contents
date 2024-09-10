#' Calculating separate vertical regression with ridge pen for each band.
#' 
#' @param est is the list of estimation results
#' @param x Matrix Time_periods x (# controls) for 
#' the control units.
#' 
#' return the predicted values for the vertical regression estimation 
cal_sepSR <- function(est, x) {
  
  ## setup
  c.sep.rid=est[["SR"]]
  time_periods=nrow(x)
  bands=nrow(c.sep.rid)-1 ## nrow(c.sep.rid) = #treated + constant
  ## calculation  
  out = matrix(nrow = time_periods, ncol = bands)
  for (i in 1:bands) {
    out[, i] = cbind(1,x) %*% c.sep.rid[,i]
  }
  return(out)
}


