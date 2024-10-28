wrapper_point<-function(input, scenarios){ 
out <- list()
for (kk in 1:length(input)){
point_estimates<-input[[kk]]  
bias=list()
MSE=list()

bias_sc=list()
bias_sr=list()
bias_bvr=list()
bias_bsc=list()
bias_smac=list()
bias_ols=list()

mse_sc=list()
mse_sr=list()
mse_ols=list()
mse_bvr=list()
mse_bsc=list()
mse_smac=list()

for(ii in 1:length(point_estimates)){
  bias_sc[[ii]]<-point_estimates[[ii]]$bias$SC
  bias_sr[[ii]]<-point_estimates[[ii]]$bias$SR
  bias_ols[[ii]]<-point_estimates[[ii]]$bias$OLS
  bias_bvr[[ii]]<-point_estimates[[ii]]$bias$BVRmedian
  bias_bsc[[ii]]<-point_estimates[[ii]]$bias$BSCmedian
  bias_smac[[ii]]<-point_estimates[[ii]]$bias$SMACmedian
  
  mse_sc[[ii]]<-point_estimates[[ii]]$SqE$SC
  mse_sr[[ii]]<-point_estimates[[ii]]$SqE$SR
  mse_ols[[ii]]<-point_estimates[[ii]]$SqE$OLS
  mse_bvr[[ii]]<-point_estimates[[ii]]$SqE$BVRmedian
  mse_bsc[[ii]]<-point_estimates[[ii]]$SqE$BSCmedian
  mse_smac[[ii]]<-point_estimates[[ii]]$SqE$SMACmedian
}

bias$SC=apply(simplify2array(bias_sc), c(1,2), mean)
bias$SR=apply(simplify2array(bias_sr), c(1,2), mean)
bias$OLS=apply(simplify2array(bias_ols), c(1,2), mean)
bias$BVR=apply(simplify2array(bias_bvr), c(1,2), mean)
bias$BSC=apply(simplify2array(bias_bsc), c(1,2), mean)
bias$SMAC=apply(simplify2array(bias_smac), c(1,2), mean)

MSE$SC=apply(simplify2array(mse_sc), c(1,2), mean)
MSE$SR=apply(simplify2array(mse_sr), c(1,2), mean)
MSE$OLS=apply(simplify2array(mse_ols), c(1,2), mean)
MSE$BVR=apply(simplify2array(mse_bvr), c(1,2), mean)
MSE$BSC=apply(simplify2array(mse_bsc), c(1,2), mean)
MSE$SMAC=apply(simplify2array(mse_smac), c(1,2), mean)

out[[kk]]<-list(bias, MSE)
names(out[[kk]])<-c("bias", "MSE")

}
names(out) <-scenarios
return(out)
}
