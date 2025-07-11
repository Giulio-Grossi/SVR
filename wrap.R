################################################################################
#'  wrap file to store the results from 200 iter into one
#' 
#' @param spatial_range is the level of spatial correlation and name of folders
#' 
#' This function wrap over the 200 dataset and store toghether all sims
#' calculation, MSE and bias, confidence intervals and coverage 
#'
################################################################################

wrap <- function(spatial_range){
  s1 <- spatial_range
for (ss in s1){
  for (tt in c(10, 20, 40)){
   for (ee in c(1,2,3)){

    simulations =list()
    calculations=list()
    point_estimates = list()
    confidence_intervals = list()
    coverage = list()
    active=as.vector(list.files(paste0("Output/apr_sims/Results","/ss",ss,
                                       "/tt", tt, "/ee", ee)))
    for(hh in 1:length(active)){
     tryCatch({
	   
	    ii=active[hh]
      load(paste0('Output/apr_sims/Results',"/ss",ss, "/tt", tt,
                  "/ee", ee, "/", ii))
      hh=which(active==ii)
      simulations[[hh]]<-res[[1]]
      calculations[[hh]]<-res[[3]]
      bias[[hh]]<-res[[5]][[1]]
      mse[[hh]]<-res[[5]][[2]]
      confidence_intervals[[hh]]<-res[[6]]
      coverage <- res[[7]]
     }, error = function(e) {
    # Gestione dell'errore
    print(paste("Errore durante l'iterazione", i, ": ", e$message))
  })
    }
    ## - save simulations
    save_file=paste0("Output/apr_sims/sim.", "ss",ss, "tt", tt,"ee",
                     ee, ".RData")
    print(save_file)
    save(simulations, file =save_file)
    
    ##- save calculations
    save_file=paste0("Output/apr_sims/cal.", "ss",ss, "tt", tt, "ee",
                     ee,  ".RData")
    save(calculations, file =save_file)

    ## save BIAS
    save_file=paste0("Output/apr_sims/bias.", "ss",ss, "tt", tt, "ee",
                     ee,  ".RData")
    save(bias, file =save_file)
        
    ## save MSE
    save_file=paste0("Output/apr_sims/mse.", "ss",ss, "tt", tt, "ee",
                     ee,  ".RData")
    save(mse, file =save_file)
    
    ## save confidence intervals
    save_file=paste0("Output/apr_sims/ci.", "ss",ss, "tt", tt, "ee",
                     ee,  ".RData")
    save(confidence_intervals, file =save_file)
    
    ## save confidence intervals
    save_file=paste0("Output/apr_sims/cov.", "ss",ss, "tt", tt, "ee",
                     ee,  ".RData")
    save(coverage, file =save_file)

  }
}
}
}
