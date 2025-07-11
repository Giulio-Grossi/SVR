################################################################################
##################### postestimation auxiliary fx ##############################
################################################################################
#' 
#' @param dir1 is the char vector with the
#' directories  in which are stores the first 200 datasets
#' 
#' @param dir2 is the char vector with the 
#' directories in which are stores the second 200 datasets
#' 
#' @param rho is the vector of dimension 3 with the spatial correlations 
#' 
#' @param exp is to estimate bias (exp=1) or MSE (exp=2) of the point estimate
#' Depends on the binder.R, loader.R, tabler.R and wrapper.R
#' 
#' @param mode indicates what tipe of test statistics will be computed 
#' mode==1 <--- BIAS
#' mode==2 <--- MSE
#' mode==3 <--- COVERAGE
#' 
#' postestimation.R depends on binder.R
#' #############################################################################


postestimation <- function(dir, rho, mode,   scenarios){

oo=binder(dir,rho)
sim_l=oo$sims; cal_l=oo$cal; cov_l=oo$cov
point_l=oo$point

## calculation of overall statistics based on the user choice 

if (mode==1){
  out <- wrapper_point(input = oo$point, scenarios)
} else if (mode==2){
  out <- wrapper_cov(input = oo$cov, scenarios)
} 

return(out)

}


