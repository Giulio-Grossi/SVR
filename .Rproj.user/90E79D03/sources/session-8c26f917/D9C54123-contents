#' Performing separate synthetic control for each band.
#' 
#' @param ym.pre Matrix T0 x bands for the pre-intervention period and the
#' outcomes units.
#' @param x.pre Matrix T0 x (# controls) for the pre-intervention period and
#' the control units.
#' 
#' Depends on the SCM_function.R
#' 
sepSC <- function(ym.pre, x.pre) {
  
  bands <- ncol(ym.pre)
  num_controls <- ncol(x.pre)
  
  c.sep.scm = vector()
  
  for (i in 1:bands) {
    v = ym.pre[,i]
    gg = SCM(v, x.pre)
    c.sep.scm=cbind(c.sep.scm, gg)
  }
  dimnames(c.sep.scm) <- list(control = 1 : num_controls, treated = 1 : bands)
  return(c.sep.scm)
}