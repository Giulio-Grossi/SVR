### this function should be used in the case we work with 400 datasets
## it binds the results from 2 round of simulations, with same pars, 200 each
## we do so because 400 dataset in a shot is too heavy for hpg 

binder<-function(wd, sp){
  sim_l=list()
  cal_l=list()
  cov_l=list()
  point_l=list()
  mse_l=list()
  method=c("SC","SR","BVR", "BSC", "SMAC")
  wd=paste0(wd, "/ss", sp, sep="")
  #wd2=paste("C:/Users/giuli/Desktop/cluster/",folder2, sep="")
  
  #wd="C:/Users/giuli/Desktop/cluster/0502/SS0.01"
  
  ## simulazioni
  load(paste0(wd, "/sim.ss", sp, "tt10ee1.RData"))
  sim_l[[1]]=simulations

  
  load(paste0(wd, "/sim.ss", sp, "tt10ee2.RData"))
  sim_l[[2]]=simulations

  
  load(paste0(wd, "/sim.ss", sp, "tt10ee3.RData"))
  sim_l[[3]]=simulations

  
  load(paste0(wd, "/sim.ss", sp, "tt20ee1.RData"))
  sim_l[[4]]=simulations

  
  load(paste0(wd, "/sim.ss", sp, "tt20ee2.RData"))
  sim_l[[5]]=simulations

  
  load(paste0(wd, "/sim.ss", sp, "tt20ee3.RData"))
  sim_l[[6]]=simulations

  
  load(paste0(wd, "/sim.ss", sp, "tt40ee1.RData"))
  sim_l[[7]]=simulations

  
  load(paste0(wd, "/sim.ss", sp, "tt40ee2.RData"))
  sim_l[[8]]=simulations

  
  load(paste0(wd, "/sim.ss", sp, "tt40ee3.RData"))
  sim_l[[9]]=simulations

  
  ##############################################################################
  
  load(paste0(wd, "/cal.ss", sp, "tt10ee1.RData"))
  cal_l[[1]]=calculations

  
  load(paste0(wd, "/cal.ss", sp, "tt10ee2.RData"))
  cal_l[[2]]=calculations

  
  load(paste0(wd, "/cal.ss", sp, "tt10ee3.RData"))
  cal_l[[3]]=calculations

  
  load(paste0(wd, "/cal.ss", sp, "tt20ee1.RData"))
  cal_l[[4]]=calculations

  
  load(paste0(wd, "/cal.ss", sp, "tt20ee2.RData"))
  cal_l[[5]]=calculations

  
  load(paste0(wd, "/cal.ss", sp, "tt20ee3.RData"))
  cal_l[[6]]=calculations

  
  load(paste0(wd, "/cal.ss", sp, "tt40ee1.RData"))
  cal_l[[7]]=calculations

  
  load(paste0(wd, "/cal.ss", sp, "tt40ee2.RData"))
  cal_l[[8]]=calculations

  
  load(paste0(wd, "/cal.ss", sp, "tt40ee3.RData"))
  cal_l[[9]]=calculations
 
  
  
  ##############################################################################
  
  load(paste0(wd, "/cov.ss", sp, "tt10ee1.RData"))
  cov_l[[1]]=coverage

  
  load(paste0(wd, "/cov.ss", sp, "tt10ee2.RData"))
  cov_l[[2]]=coverage

  
  load(paste0(wd, "/cov.ss", sp, "tt10ee3.RData"))
  cov_l[[3]]=coverage

  
  load(paste0(wd, "/cov.ss", sp, "tt20ee1.RData"))
  cov_l[[4]]=coverage

  
  load(paste0(wd, "/cov.ss", sp, "tt20ee2.RData"))
  cov_l[[5]]=coverage

  
  load(paste0(wd, "/cov.ss", sp, "tt20ee3.RData"))
  cov_l[[6]]=coverage

  
  load(paste0(wd, "/cov.ss", sp, "tt40ee1.RData"))
  cov_l[[7]]=coverage

  
  load(paste0(wd, "/cov.ss", sp, "tt40ee2.RData"))
  cov_l[[8]]=coverage

  
  load(paste0(wd, "/cov.ss", sp, "tt40ee3.RData"))
  cov_l[[9]]=coverage

  
  ##############################################################################
  
  
  ##############################################################################
  
  load(paste0(wd, "/point.ss", sp, "tt10ee1.RData"))
  point_l[[1]]=point_estimates
 
  
  load(paste0(wd, "/point.ss", sp, "tt10ee2.RData"))
  point_l[[2]]=point_estimates

  
  load(paste0(wd, "/point.ss", sp, "tt10ee3.RData"))
  point_l[[3]]=point_estimates

  
  load(paste0(wd, "/point.ss", sp, "tt20ee1.RData"))
  point_l[[4]]=point_estimates

  
  load(paste0(wd, "/point.ss", sp, "tt20ee2.RData"))
  point_l[[5]]=point_estimates

  
  load(paste0(wd, "/point.ss", sp, "tt20ee3.RData"))
  point_l[[6]]=point_estimates

  
  load(paste0(wd, "/point.ss", sp, "tt40ee1.RData"))
  point_l[[7]]=point_estimates

  
  load(paste0(wd, "/point.ss", sp, "tt40ee2.RData"))
  point_l[[8]]=point_estimates

  
  load(paste0(wd, "/point.ss", sp, "tt40ee3.RData"))
  point_l[[9]]=point_estimates

  out=list()
  out$sims=sim_l
  out$cal=cal_l
  out$point=point_l
  out$cov=cov_l
  return(out)
  
}
