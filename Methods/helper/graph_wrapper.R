################################################################################
#' helper function to use all the dataset for a given statistics (bias, MSE 
#' and coverage) and generate a dataframe to be used to generate the graphs
#' 
#'@param stat001; @param stat03; @param stat06 will be the three list of test
#' statistics across the different lengthscale
#' 
#' 

graph_wrapper <- function(stat001, stat03, stat06){
  
  nna=matrix(ncol=2, nrow=9)
  
  nna[1,] = c("IID, 40%", 10)
  nna[2,] = c("50-50, 40%", 10)
  nna[3,] = c("50-50, 70%", 10)
  nna[4,] = c("IID, 40%", 20)
  nna[5,] = c("50-50, 40%", 20)
  nna[6,] = c("50-50, 70%", 20)
  nna[7,] = c("IID, 40%", 40)
  nna[8,] = c("50-50, 40%", 40)
  nna[9,] = c("50-50, 70%", 40)
  
  mm=vector()
  
  for(ii in 1:3){
    
    lista
    
    
    for(ii in 1:length(res_gp1)){
      loc=cbind(nna[ii,1], nna[ii,2], seq(1:(tt0[ii]+20)), apply(res_gp1[[ii]],1,mean), "SMaC", 0.001)
      mm=rbind(mm,loc)
    }
    
    for(ii in 1:length(res_sc1)){
      loc=cbind(nna[ii,1], nna[ii,2], seq(1:(tt0[ii]+20)),apply(res_sc1[[ii]],1,mean), "Synth", 0.001)
      mm=rbind(mm,loc)
    }
    
    for(ii in 1:length(res_sr1)){
      loc=cbind(nna[ii,1], nna[ii,2], seq(1:(tt0[ii]+20)),apply(res_sr1[[ii]],1,mean), "Ridge", 0.001)
      mm=rbind(mm,loc)
    }
    
    for(ii in 1:length(res_br1)){
      loc=cbind(nna[ii,1], nna[ii,2], seq(1:(tt0[ii]+20)),apply(res_br1[[ii]],1,mean), "BayVR", 0.001)
      mm=rbind(mm,loc)
    }
    
    for(ii in 1:length(res_bsc1)){
      loc=cbind(nna[ii,1], nna[ii,2], seq(1:(tt0[ii]+20)),apply(res_bsc1[[ii]],1,mean), "BaySC", 0.001)
      mm=rbind(mm,loc)
    }
    
    
    ######
    
    for(ii in 1:length(res_gp2)){
      loc=cbind(nna[ii,1], nna[ii,2], seq(1:(tt0[ii]+20)), apply(res_gp2[[ii]],1,mean), "SMaC", 0.2)
      mm=rbind(mm,loc)
    }
    
    for(ii in 1:length(res_sc2)){
      loc=cbind(nna[ii,1], nna[ii,2], seq(1:(tt0[ii]+20)),apply(res_sc2[[ii]],1,mean), "Synth", 0.2)
      mm=rbind(mm,loc)
    }
    
    for(ii in 1:length(res_sr2)){
      loc=cbind(nna[ii,1], nna[ii,2], seq(1:(tt0[ii]+20)),apply(res_sr2[[ii]],1,mean), "Ridge", 0.2)
      mm=rbind(mm,loc)
    }
    
    for(ii in 1:length(res_br2)){
      loc=cbind(nna[ii,1], nna[ii,2], seq(1:(tt0[ii]+20)),apply(res_br2[[ii]],1,mean), "BayVR", 0.2)
      mm=rbind(mm,loc)
    }
    
    for(ii in 1:length(res_bsc2)){
      loc=cbind(nna[ii,1], nna[ii,2], seq(1:(tt0[ii]+20)),apply(res_bsc2[[ii]],1,mean), "BaySC", 0.2)
      mm=rbind(mm,loc)
    }
    
    ###
    
    for(ii in 1:length(res_gp3)){
      loc=cbind(nna[ii,1], nna[ii,2], seq(1:(tt0[ii]+20)), apply(res_gp3[[ii]],1,mean), "SMaC", 0.4)
      mm=rbind(mm,loc)
    }
    
    for(ii in 1:length(res_sc3)){
      loc=cbind(nna[ii,1], nna[ii,2], seq(1:(tt0[ii]+20)),apply(res_sc3[[ii]],1,mean), "Synth", 0.4)
      mm=rbind(mm,loc)
    }
    
    for(ii in 1:length(res_sr3)){
      loc=cbind(nna[ii,1], nna[ii,2], seq(1:(tt0[ii]+20)),apply(res_sr3[[ii]],1,mean), "Ridge", 0.4)
      mm=rbind(mm,loc)
    }
    
    for(ii in 1:length(res_br3)){
      loc=cbind(nna[ii,1], nna[ii,2], seq(1:(tt0[ii]+20)),apply(res_br3[[ii]],1,mean), "BayVR", 0.4)
      mm=rbind(mm,loc)
    }
    
    for(ii in 1:length(res_bsc3)){
      loc=cbind(nna[ii,1], nna[ii,2], seq(1:(tt0[ii]+20)),apply(res_bsc3[[ii]],1,mean), "BaySC", 0.4)
      mm=rbind(mm,loc)
    }
    
    
    
    
  }
  
  
  for(ii in 1:length(loc))
  

  
  
  colnames(mm)=c("Error", "T0", "Time", "Bias", "Method", "Lengthscale")
  mm=as.data.frame(mm)
  mm$Method=as.factor(mm$Method)
  mm$Time=as.integer(mm$Time)
  mm$Error=as.factor(mm$Error)
  mm$Lengthscale=as.factor(mm$Lengthscale)
  mm$T0=as.factor(mm$T0)
  mm$Bias=as.numeric(mm$Bias)
  
  
  
  
  
  
  
  
  
  
  
}