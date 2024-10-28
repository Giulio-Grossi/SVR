

wrapper_cov<-function(input, scenarios){ 
  out <- list()
  for (kk in 1:length(input)){
    copertura<-input[[kk]]  
    coverage=list()

    

    cov_bvr=list()
    cov_bsc=list()
    cov_smac=list()
    cov_ols=list()
    

    for(ii in 1:length(point_estimates)){
      cov_ols[[ii]]<-copertura[[ii]]$OLS
      cov_bvr[[ii]]<-copertura[[ii]]$BVR
      cov_bsc[[ii]]<-copertura[[ii]]$BSC
      cov_smac[[ii]]<-copertura[[ii]]$SMAC
      

    }

    coverage$OLS=apply(simplify2array(cov_ols), c(1,2), mean)
    coverage$BVR=apply(simplify2array(cov_bvr), c(1,2), mean)
    coverage$BSC=apply(simplify2array(cov_bsc), c(1,2), mean)
    coverage$SMAC=apply(simplify2array(cov_smac), c(1,2), mean)

    out[[kk]]<-coverage
    names(out[[kk]])<-c("OLS", "BVR", "BSC", "SMAC")
    
  }
  names(out) <-scenarios
  return(out)
}
