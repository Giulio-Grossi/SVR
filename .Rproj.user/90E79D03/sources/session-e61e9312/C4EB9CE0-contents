#### libraries
wd=getwd()
source(paste0(wd, "/Methods/helper/loader.R"))
source(paste0(wd, "/Methods/helper/wrapper.R"))
source(paste0(wd, "/Methods/helper/tabler.R"))
source(paste0(wd, "/Methods/helper/binder.R"))
source(paste0(wd, "/Methods/helper/postestimation.R"))

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

## TABLER FUNCTION - it generates the table to be used in kable
################################################################################
## we need to generate three different tables according to the length
## of post-treatment
#################################

## bias tables

tab_bias001<- cbind(tabler(bias_001, tt0, pt1), tabler(bias_001, tt0, pt2),
                 tabler(bias_001, tt0, pt3))  

kable(round(tab_bias001,3), format="latex",booktabs=T)

tab_bias03<- cbind(tabler(bias_03, tt0, pt1), tabler(bias_03, tt0, pt2),
                 tabler(bias_03, tt0, pt3))  

kable(round(tab_bias03,3), format="latex",booktabs=T)

tab_bias06<- cbind(tabler(bias_06, tt0, pt1), tabler(bias_06, tt0, pt2),
                 tabler(bias_06, tt0, pt3))  

kable(round(tab_bias06,3), format="latex",booktabs=T)

## MSE tables

tab_mse001<- cbind(tabler(mse_001, tt0, pt1), tabler(mse_001, tt0, pt2),
                 tabler(mse_001, tt0, pt3))  

kable(round(tab_mse001,3), format="latex",booktabs=T)

tab_mse03<- cbind(tabler(mse_03, tt0, pt1), tabler(mse_03, tt0, pt2),
                   tabler(mse_03, tt0, pt3))  

kable(round(tab_mse03,3), format="latex",booktabs=T)

tab_mse06<- cbind(tabler(mse_06, tt0, pt1), tabler(mse_06, tt0, pt2),
                   tabler(mse_06, tt0, pt3))  

kable(round(tab_mse06,3), format="latex",booktabs=T)

## Coverage tables

tab_cov001<- cbind(tabler(cov_001, tt0, pt1), tabler(cov_001, tt0, pt2),
                tabler(cov_001, tt0, pt3))  

kable(round(tab_cov001,3), format="latex",booktabs=T)

tab_cov03<- cbind(tabler(cov_03, tt0, pt1), tabler(cov_03, tt0, pt2),
                   tabler(cov_03, tt0, pt3))  

kable(round(tab_cov03,3), format="latex",booktabs=T)

tab_cov06<- cbind(tabler(cov_06, tt0, pt1), tabler(cov_06, tt0, pt2),
                   tabler(cov_06, tt0, pt3))  

kable(round(tab_cov06,3), format="latex",booktabs=T)

### end of tables
################################################################################
### GRAPHS 
###############################################################################


