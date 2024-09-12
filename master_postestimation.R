#### libraries
wd=getwd()
source(paste0(wd, "/Methods/helper/loader.R"))
source(paste0(wd, "/Methods/helper/wrapper.R"))
source(paste0(wd, "/Methods/helper/tabler.R"))
source(paste0(wd, "/Methods/helper/binder.R"))
source(paste0(wd, "/Methods/helper/postestimation_function.R"))

library(kableExtra)


#### Utility vectors. to be used inside the helper functions to estimate
# test statistics for different time lenghts

tt0=c(rep(10,3), rep(20, 3), rep(40,3))
pt1=rep(5,9)
pt2=rep(10,9)
pt3=c(rep(5,3), rep(10, 3), rep(20,3))

methods=c("SC", "SR", "BVR", "BSC", "SMAC")
scenarios <- c("t0=10, IID", "t0=10, 40%","t0=10, 70%",
               "t0=20, IID", "t0=20, 40%","t0=20, 70%",
               "t0=40, IID", "t0=40, 40%","t0=40, 70%")

## -- BIAS -- ##
bias_001=postestimation(dir1, dir2, "0.01", mode = 1, methods, scenarios)
bias_03=postestimation(dir1, dir2, "0.3", mode = 1, methods, scenarios)
bias_06=postestimation(dir1, dir2, "0.6",mode = 1, methods, scenarios)

## -- MSE -- ##
mse_001=postestimation(dir1, dir2, "0.01", mode = 2, methods, scenarios)
mse_03=postestimation(dir1, dir2, "0.3", mode = 2, methods, scenarios)
mse_06=postestimation(dir1, dir2, "0.6", mode = 2, methods, scenarios)

## -- COVERAGE -- ##
cov_001=postestimation(dir1, dir2, "0.01", mode = 3, methods, scenarios)
cov_03=postestimation(dir1, dir2, "0.3", mode = 3, methods, scenarios)
cov_06=postestimation(dir1, dir2, "0.6", mode = 3, methods, scenarios)


