#' Function that takes in a matrix or data frame, and standardizes each column
#' using the mean and standard deviation of the first t0 rows.
#' 
#' Returns the standardized data, along with the means and standard deviations
#' used to standardize them.
#' 
preset <- function(data, t0){
  
  # keep only the first t0 rows
  data0 = data[1:t0,]
  
  # Get the mean and standard deviation of each column
  m0=apply(data0, 2, mean)
  s0=apply(data0, 2, sd)
  
  # Standardize.
  std=data
  for(ii in 1:ncol(data0)){
    std[,ii] = (data[, ii] - m0[ii]) / s0[ii]
  }
  
  return(list(std = std, means = m0, sds = s0))
}
