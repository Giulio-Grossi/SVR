#' Performing separate vertical regression using OLS with SVD as in 
#' Shen et al (2023)
#' 
#' @param ym.pre Matrix T0 x bands for the pre-intervention period and the
#' outcomes units.
#' @param x.pre Matrix T0 x (# controls) for the pre-intervention period and
#' the control units.
#' 
#' 
sepOLS <- function(ym.pre, x.pre){
  bands <- ncol(ym.pre)
  num_controls <- ncol(x.pre)
  c.sep.ols = vector()
  for (i in 1 : bands) {
    v = ym.pre[, i]
    beta = pseudo.inv(x.pre)%*%v  # alpha = 0 specifies ridge.
    c.sep.rid = cbind(c.sep.ols, beta)
  }
  colnames(c.sep.ols) <- paste('treated', 1 : bands)
  return(c.sep.ols)
}
