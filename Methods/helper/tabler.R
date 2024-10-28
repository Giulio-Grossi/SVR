#' ############################################################################
#' Function to generate tabled results over the post_treatment period for 
#' bias, MSE and coverage. 
#' @param res is the input list of structure obj$scenario$method 
#' @param tt0 is the pre_treatment period length
#' @param post_treatment is the vector of post_treatment period, as we consider
#' three different scenarios 
#' #############################################################################


tabler<-function(res, tt0, post_treatment, stat){
tab=vector()
nn=c("tt10 - IID", "tt10 - 40%", "tt10 - 70%",
     "tt20 - IID", "tt20 - 40%", "tt20 - 70%",
     "tt40 - IID", "tt40 - 40%", "tt40 - 70%"  )

tavola=vector()
for (ii in 1:length(nn)){
  t0=tt0[ii]
  time_periods=tt0[ii] + post_treatment[ii]
  if(stat=="bias"){
    loc <- res[[ii]]$bias  
  }
  if(stat=="MSE"){
    loc <- res[[ii]]$MSE
  }
  if(stat=="coverage"){
    loc <- res[[ii]]
  }
  
  tab=vector()
  for (kk in 1:length(loc)){
     tab<-c(tab, mean(apply(loc[[kk]], 1, mean)[(t0+1):time_periods], na.rm=T))
  }
  names(tab)=names(loc)
  
tavola=rbind(tavola, tab)
}
rownames(tavola) = nn
return(tavola)
}


