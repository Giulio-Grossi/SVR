################################################################################
#' This is a function to average over the whole set of simulated datasets 
#' @param input is a list of 200*#methods objects in which the test stat
#' is stored (MSE, bias or coverage).
#' @methods is the vector of methods considered, it should be a vector of char
#' 
#' output: a list of #methods with the average of the test statistics considered
#'##############################################################################




wrapper<-function(input, methods, mode){
  
  ## Here we generate an auxiliary list of #methods$#datasets to store the test 
  ## statistics, and we assign the name of the methods 
  
  rr <- list()
  for(ii in 1:length(methods)){
    rr[[ii]]=list()
  }
  names(rr)=methods
  
### looping through the several scenarios 
  
  for(ii in c(1:length(input))){
    ll=length(input[i]) ## it counts the length of the specific input 
    tryCatch({   ## in case of errors
      
      ### We reorganize the input in the rr list having as first level the method,
      ### and as second level the datasets
      
      for(kk in 1:ll){
        
      if(mode==1){
        rr[[methods[i]]][[kk]]<-input[[kk]]$bias[[methods[i]]]
      }
        if(mode==2){
        rr[[methods[i]]][[kk]]<-input[[kk]]$SqE[[methods[i]]]
        }
        if(mode==3){
        rr[[methods[i]]][[kk]]<-input[[kk]][[methods[i]]] 
        }
      
        
      }
      
      ### We generate the auxiliary list of output
        
      out <- list()
      for(ii in 1:length(methods)){
        out[[ii]]=list()
      }
      names(out)=methods 
      
      ### Finally, we average over the whole set of datasets
      
      for(ii in 1:length(methods)){
        out[[methods[ii]]]<-apply(simplify2array(rr[[methods[ii]]]),
                                  1:2, mean, na.rm=T)
        
      }

      
    }, error = function(e) {
      # Gestione dell'errore
      print(paste("Errore durante l'iterazione", i, ": ", e$message))
    })
  }
  
  ## out will be a list of $scenarios$methods, in our case, 9
  
  return(out)
}
