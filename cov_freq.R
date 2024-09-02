
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
        cal<-res[[3]]
        time_periods=tt+20
        if ("SC" %in% names(cal)){
          up=matrix(nrow=time_periods, ncol=bands)
          low=matrix(nrow=time_periods, ncol=bands)
          error=matrix(nrow=time_periods, ncol=bands)
          error=sim[,1:bands] - cal[["SC"]]
          se_treated=apply(error,2, sd)
          
          for(i in 1:bands){
            up[,i] = cal[["SC"]][,i] + 1.96*se_treated[i]
            low[,i] = cal[["SC"]][,i] - 1.96*se_treated[i]
          }
          sc=list(up, low)
        }
        
        if ("SR" %in% names(cal)){
          up=matrix(nrow=time_periods, ncol=bands)
          low=matrix(nrow=time_periods, ncol=bands)
          error=matrix(nrow=time_periods, ncol=bands)
          error=sim[,1:bands] - cal[["SR"]]
          se_treated=apply(error,2, sd)
          
          for(i in 1:bands){
            up[,i] = cal[["SR"]][,i] + 1.96*se_treated[i]
            low[,i] = cal[["SR"]][,i] - 1.96*se_treated[i]
          }
          sr=list(up, low)
        }
          
        cov=list(sc,sr)
        coverage[[ii]]<-cov
      }, error = function(e) {
    # Gestione dell'errore
    print(paste("Errore durante l'iterazione", i, ": ", e$message))
  })
	}
      
      save_file=paste0("Output/apr_sims/cov_freq.", "ss",ss, "tt", tt, "ee", ee, ".RData")
      save(coverage, file =save_file)
    }
  }
}
