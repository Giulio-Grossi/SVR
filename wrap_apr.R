################################################################################
###################   wrap file to store the results from 200 iter into one ####
s1=0.01

for (ss in s1){
  for (tt in c(10, 20, 40)){
   for (ee in c(1,2,3)){

    pippo=list()
    pluto=list()
    active=as.vector(list.files(paste0("Output/apr_sims/Results","/ss",ss,
                                       "/tt", tt, "/ee", ee)))
    for(hh in 1:length(active)){
     tryCatch({
	   
	    ii=active[hh]
      load(paste0('Output/apr_sims/Results',"/ss",ss, "/tt", tt,
                  "/ee", ee, "/", ii))
      hh=which(active==ii)
      pippo[[hh]]<-res[[1]]
      pluto[[hh]]<-res[[3]]
     }, error = function(e) {
    # Gestione dell'errore
    print(paste("Errore durante l'iterazione", i, ": ", e$message))
  })
    }

    save_file=paste0("Output/apr_sims/sim.", "ss",ss, "tt", tt,"ee",
                     ee, ".RData")
    print(save_file)
    save(pippo, file =save_file)
    save_file=paste0("Output/apr_sims/cal.", "ss",ss, "tt", tt, "ee",
                     ee,  ".RData")
    save(pluto, file =save_file)

  }
}
}
