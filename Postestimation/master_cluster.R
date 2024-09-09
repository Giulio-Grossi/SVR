#### libraries
wd=getwd()
source(paste0(wd, "/Postestimation/helper/loader.R"))
source(paste0(wd, "/Postestimation/helper/wrapper.R"))
source(paste0(wd, "/Postestimation/helper/tabler.R"))
source(paste0(wd, "/Postestimation/helper/binder.R"))
source(paste0(wd, "/Postestimation/helper/postestimation_function.R"))

library(kableExtra)


#### Utility vectors. to be used inside the helper functions to estimate
# test statistics for different time lenghts

tt0=c(rep(10,3), rep(20, 3), rep(40,3))
pt1=rep(5,9)
pt2=rep(10,9)
pt3=c(rep(5,3), rep(10, 3), rep(20,3))

## -- BIAS -- ##
exp=1
bias_001=postestimation(dir1, dir2, "0.01", exp)
bias_03=postestimation(dir1, dir2, "0.3", exp)
bias_06=postestimation(dir1, dir2, "0.6", exp)

## -- MSE -- ##
exp=2
mse_001=postestimation(dir1, dir2, "0.01", exp)
mse_03=postestimation(dir1, dir2, "0.3", exp)
mse_06=postestimation(dir1, dir2, "0.6", exp)


