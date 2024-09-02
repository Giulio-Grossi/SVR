s1=0.01




source("replication_april/preset.R")

for (ss in s1){
  for (tt in c(10,20,40)){
    for(ee in c(1,2,3)){
      
      coverage=list()
      active=as.vector(list.files(paste0("Output/apr_sims/Results","/ss",ss, "/tt", tt, "/ee", ee)))
      for (ii in active) {
	tryCatch({
        load(paste0('Output/apr_sims/Results',"/ss",ss, "/tt", tt,"/ee", ee,  "/", ii))
        hh=which(active==ii)
        sim<-res[[1]]
        center= apply(sim[1:tt,],2, mean)
        scale = apply(sim[1:tt,], 2, sd)
        est<-res[[2]]
        time_periods=tt+20
        if ("BVR" %in% names(est)){
          up=matrix(nrow=time_periods, ncol=bands)
          low=matrix(nrow=time_periods, ncol=bands)
          mse=matrix(nrow=time_periods, ncol=bands)
          for(i in 1:bands){
            ff=est[["BVR"]][[i]]$y_new
            for(hh in 1:nrow(ff)){
              ff[hh,]=(ff[hh,]*scale[i])+center[i]
            }	     
            up[,i]=apply(ff,2,quantile, p=0.975)
            low[,i]=apply(ff,2,quantile, p=0.025)
            loc=(sweep(ff, 2, sim[,i]))^2
            mse[,i]=apply(loc,2,median)
          }
          bvr=list(up,low, mse)
        }
        
        if ("BSC" %in% names(est)){
          up=matrix(nrow=time_periods, ncol=bands)
          low=matrix(nrow=time_periods, ncol=bands)
          mse=matrix(nrow=time_periods, ncol=bands)
          for(i in 1:bands){
            ff=est[["BSC"]][[i]]$y_new
            for(hh in 1:nrow(ff)){
              ff[hh,]=(ff[hh,]*scale[i])+center[i]
            }	     
            up[,i]=apply(ff,2,quantile, p=0.975)
            low[,i]=apply(ff,2,quantile, p=0.025)
            loc=(sweep(ff, 2, sim[,i]))^2
            mse[,i]=apply(loc,2,median)
          }
          bsc=list(up,low, mse)
        }      
        
        if ("MGP" %in% names(est)){
          ff=rstan::extract(est[["MGP"]])
          up=matrix(nrow=time_periods, ncol=bands)
          low=matrix(nrow=time_periods, ncol=bands)
          mse=matrix(nrow=time_periods, ncol=bands)
          
          for(i in 1:bands){
            
            for(hh in 1:nrow(ff$ynn[,,1])){
              ff$ynn[hh,,i]=(ff$ynn[hh,,i]*scale[i]) + center[i]
            }		
            up[,i]=apply(ff$ynn[,,i],2,quantile, prob=0.975)
            low[,i]=apply(ff$ynn[,,i],2,quantile, prob=0.025)
            loc=(sweep(ff$ynn[,,i], 2, sim[,i]))^2
            mse[,i]=apply(loc, 2, median)
          }
          mgp=list(up,low, mse)
        }
        cov=list(bvr, bsc, mgp)
        coverage[[ii]]<-cov
     }, error = function(e) {
    # Gestione dell'errore
    print(paste("Errore durante l'iterazione", i, ": ", e$message))
  })
       	}
      
      save_file=paste0("Output/apr_sims/cov.", "ss",ss, "tt", tt, "ee", ee, ".RData")
      save(coverage, file =save_file)
    }
  }
}
