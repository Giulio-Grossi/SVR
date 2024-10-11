#' Bayesian confidence interval
#' 
#' @param est_method the list of posterior distributions from bayes methods
#' We use this function just for bayesian vertical reg, bayesian synthetic 
#' control, and SMAC.
#' @param bands The number of treated units.
#' @param means If the posterior samples for the predictions are standardized,
#' provide the pre-intervention mean for the treated units. If left equal to
#' NULL, a vector of 0s will be used, as if the method is fit to data that have
#' not been standardized.
#' @param sds If the posterior samples for the predictions are standardized,
#' provide the pre-intervention standard deviation for the treated units. If
#' left equal to NULL, a vector of 1s will be used, as if the method is fit to
#' data that have not been standardized.
#' 
#' @autor Giulio Grossi
#' 


ci_bayes <- function(est_method, bands, means = NULL, sds = NULL) {
  
  if (is.null(means)) means <- rep(0, bands)
  if (is.null(sds)) sds <- rep(1, bands)
  
  lower <- matrix(nrow = time_periods, ncol = bands)
  upper <- matrix(nrow = time_periods, ncol = bands)
  
  for (i in 1 : bands) {
    
    # Posterior samples for treated unit i based on the Bayesian methods:
    
    if ('ynn' %in% names(est_method)) {  # For SMAC
      loc <- est_method$ynn[, , i]
    } else {                             # For BVR and BSC.
      loc <- est_method[[i]]$y_new
    }
    
    loc <- loc * sds[i] + means[i]  # Re-standardizing them.
    lower[,i] <- apply(loc, 2, quantile, prob = 0.025)
    upper[,i] <- apply(loc, 2, quantile, prob = 0.975)
  }
  
  out = list(lower, upper) 
  names(out) = c("lower_bound","upper_bound" )
  return(out)  
}
