#' Performing separate vertical regression using ridge penalty for each band.
#' 
#' @param ym.pre Matrix T0 x bands for the pre-intervention period and the
#' outcomes units.
#' @param x.pre Matrix T0 x (# controls) for the pre-intervention period and
#' the control units.
#' 
#' Depends on the glmnet library
#' 
sepSR <- function(ym.pre, x.pre) {
  bands <- ncol(ym.pre)
  num_controls <- ncol(x.pre)
  c.sep.rid = vector()
  for (i in 1:bands) {
    v = ym.pre[,i]
    gg = cv.glmnet(x = x.pre, y = v, alpha = 0)  # alpha = 0 specifies ridge.
    l=gg$lambda.min
    best_model <- glmnet(x.pre, v, alpha = 0, lambda = l)
    gg=as.matrix(coef(best_model))
    c.sep.rid=cbind(c.sep.rid, gg)
  }
  colnames(c.sep.rid) <- paste('treated', 1 : bands)
  return(c.sep.rid)
}