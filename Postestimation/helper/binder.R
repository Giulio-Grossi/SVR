binder<-function(wd1, wd2, sp){
  sim_l=list()
  cal_l=list()
  cov_l=list()
  cov_freq=list()
  method=c("SC","SR","PR","BVR", "BSC", "MGP")
  wd1=paste("C:/Users/giuli/Desktop/cluster/",folder1, sep="")
  #wd2=paste("C:/Users/giuli/Desktop/cluster/",folder2, sep="")
  
  #wd1="C:/Users/giuli/Desktop/cluster/0502/SS0.01"
  
  ## simulazioni
  load(paste0(wd1, "/sim.ss", sp, "tt10ee1.RData"))
  sim_l[[1]]=pippo
  load(paste0(wd2, "/sim.ss", sp, "tt10ee1.RData"))
  sim_l[[1]]=c(sim_l[[1]], pippo)
  
  
  load(paste0(wd1, "/sim.ss", sp, "tt10ee2.RData"))
  sim_l[[2]]=pippo
  load(paste0(wd2, "/sim.ss", sp, "tt10ee2.RData"))
  sim_l[[2]]=c(sim_l[[2]], pippo)
  
  load(paste0(wd1, "/sim.ss", sp, "tt10ee3.RData"))
  sim_l[[3]]=pippo
  load(paste0(wd2, "/sim.ss", sp, "tt10ee3.RData"))
  sim_l[[3]]=c(sim_l[[3]], pippo)
  
  load(paste0(wd1, "/sim.ss", sp, "tt20ee1.RData"))
  sim_l[[4]]=pippo
  load(paste0(wd2, "/sim.ss", sp, "tt20ee1.RData"))
  sim_l[[4]]=c(sim_l[[4]], pippo)
  
  load(paste0(wd1, "/sim.ss", sp, "tt20ee2.RData"))
  sim_l[[5]]=pippo
  load(paste0(wd2, "/sim.ss", sp, "tt20ee2.RData"))
  sim_l[[5]]=c(sim_l[[5]], pippo)
  
  load(paste0(wd1, "/sim.ss", sp, "tt20ee3.RData"))
  sim_l[[6]]=pippo
  load(paste0(wd2, "/sim.ss", sp, "tt20ee3.RData"))
  sim_l[[6]]=c(sim_l[[6]], pippo)
  
  load(paste0(wd1, "/sim.ss", sp, "tt40ee1.RData"))
  sim_l[[7]]=pippo
  load(paste0(wd2, "/sim.ss", sp, "tt40ee1.RData"))
  sim_l[[7]]=c(sim_l[[7]], pippo)
  
  load(paste0(wd1, "/sim.ss", sp, "tt40ee2.RData"))
  sim_l[[8]]=pippo
  load(paste0(wd2, "/sim.ss", sp, "tt40ee2.RData"))
  sim_l[[8]]=c(sim_l[[8]], pippo)
  
  load(paste0(wd1, "/sim.ss", sp, "tt40ee3.RData"))
  sim_l[[9]]=pippo
  load(paste0(wd2, "/sim.ss", sp, "tt40ee3.RData"))
  sim_l[[9]]=c(sim_l[[9]], pippo)
  
  ##############################################################################
  
  load(paste0(wd1, "/cal.ss", sp, "tt10ee1.RData"))
  cal_l[[1]]=pluto
  load(paste0(wd2, "/cal.ss", sp, "tt10ee1.RData"))
  cal_l[[1]]=c(cal_l[[1]], pluto)
  
  load(paste0(wd1, "/cal.ss", sp, "tt10ee2.RData"))
  cal_l[[2]]=pluto
  load(paste0(wd2, "/cal.ss", sp, "tt10ee2.RData"))
  cal_l[[2]]=c(cal_l[[2]], pluto)
  
  load(paste0(wd1, "/cal.ss", sp, "tt10ee3.RData"))
  cal_l[[3]]=pluto
  load(paste0(wd2, "/cal.ss", sp, "tt10ee3.RData"))
  cal_l[[3]]=c(cal_l[[3]], pluto)
  
  load(paste0(wd1, "/cal.ss", sp, "tt20ee1.RData"))
  cal_l[[4]]=pluto
  load(paste0(wd2, "/cal.ss", sp, "tt20ee1.RData"))
  cal_l[[4]]=c(cal_l[[4]], pluto)
  
  load(paste0(wd1, "/cal.ss", sp, "tt20ee2.RData"))
  cal_l[[5]]=pluto
  load(paste0(wd2, "/cal.ss", sp, "tt20ee2.RData"))
  cal_l[[5]]=c(cal_l[[5]], pluto)
  
  load(paste0(wd1, "/cal.ss", sp, "tt20ee3.RData"))
  cal_l[[6]]=pluto
  load(paste0(wd2, "/cal.ss", sp, "tt20ee3.RData"))
  cal_l[[6]]=c(cal_l[[6]], pluto)
  
  load(paste0(wd1, "/cal.ss", sp, "tt40ee1.RData"))
  cal_l[[7]]=pluto
  load(paste0(wd2, "/cal.ss", sp, "tt40ee1.RData"))
  cal_l[[7]]=c(cal_l[[7]], pluto)
  
  load(paste0(wd1, "/cal.ss", sp, "tt40ee2.RData"))
  cal_l[[8]]=pluto
  load(paste0(wd2, "/cal.ss", sp, "tt40ee2.RData"))
  cal_l[[8]]=c(cal_l[[8]], pluto)
  
  load(paste0(wd1, "/cal.ss", sp, "tt40ee3.RData"))
  cal_l[[9]]=pluto
  load(paste0(wd2, "/cal.ss", sp, "tt40ee3.RData"))
  cal_l[[9]]=c(cal_l[[9]], pluto) 
  
  
  ##############################################################################
  
  load(paste0(wd1, "/cov.ss", sp, "tt10ee1.RData"))
  cov_l[[1]]=coverage
  load(paste0(wd2, "/cov.ss", sp, "tt10ee1.RData"))
  cov_l[[1]]=c(cov_l[[1]], coverage)
  
  load(paste0(wd1, "/cov.ss", sp, "tt10ee2.RData"))
  cov_l[[2]]=coverage
  load(paste0(wd2, "/cov.ss", sp, "tt10ee2.RData"))
  cov_l[[2]]=c(cov_l[[2]], coverage)
  
  load(paste0(wd1, "/cov.ss", sp, "tt10ee3.RData"))
  cov_l[[3]]=coverage
  load(paste0(wd2, "/cov.ss", sp, "tt10ee3.RData"))
  cov_l[[3]]=c(cov_l[[3]], coverage)
  
  load(paste0(wd1, "/cov.ss", sp, "tt20ee1.RData"))
  cov_l[[4]]=coverage
  load(paste0(wd2, "/cov.ss", sp, "tt20ee1.RData"))
  cov_l[[4]]=c(cov_l[[4]], coverage)
  
  load(paste0(wd1, "/cov.ss", sp, "tt20ee2.RData"))
  cov_l[[5]]=coverage
  load(paste0(wd2, "/cov.ss", sp, "tt20ee2.RData"))
  cov_l[[5]]=c(cov_l[[5]], coverage)
  
  load(paste0(wd1, "/cov.ss", sp, "tt20ee3.RData"))
  cov_l[[6]]=coverage
  load(paste0(wd2, "/cov.ss", sp, "tt20ee3.RData"))
  cov_l[[6]]=c(cov_l[[6]], coverage)
  
  load(paste0(wd1, "/cov.ss", sp, "tt40ee1.RData"))
  cov_l[[7]]=coverage
  load(paste0(wd2, "/cov.ss", sp, "tt40ee1.RData"))
  cov_l[[7]]=c(cov_l[[7]], coverage)
  
  load(paste0(wd1, "/cov.ss", sp, "tt40ee2.RData"))
  cov_l[[8]]=coverage
  load(paste0(wd2, "/cov.ss", sp, "tt40ee2.RData"))
  cov_l[[8]]=c(cov_l[[8]], coverage)
  
  load(paste0(wd1, "/cov.ss", sp, "tt40ee3.RData"))
  cov_l[[9]]=coverage
  load(paste0(wd2, "/cov.ss", sp, "tt40ee3.RData"))
  cov_l[[9]]=c(cov_l[[9]], coverage) 
  
  ##############################################################################
  
  
  ##############################################################################
  
  load(paste0(wd1, "/cov_freq.ss", sp, "tt10ee1.RData"))
  cov_freq[[1]]=coverage
  load(paste0(wd2, "/cov_freq.ss", sp, "tt10ee1.RData"))
  cov_freq[[1]]=c(cov_freq[[1]], coverage)
  
  load(paste0(wd1, "/cov_freq.ss", sp, "tt10ee2.RData"))
  cov_freq[[2]]=coverage
  load(paste0(wd2, "/cov_freq.ss", sp, "tt10ee2.RData"))
  cov_freq[[2]]=c(cov_freq[[2]], coverage)
  
  load(paste0(wd1, "/cov_freq.ss", sp, "tt10ee3.RData"))
  cov_freq[[3]]=coverage
  load(paste0(wd2, "/cov_freq.ss", sp, "tt10ee3.RData"))
  cov_freq[[3]]=c(cov_freq[[3]], coverage)
  
  load(paste0(wd1, "/cov_freq.ss", sp, "tt20ee1.RData"))
  cov_freq[[4]]=coverage
  load(paste0(wd2, "/cov_freq.ss", sp, "tt20ee1.RData"))
  cov_freq[[4]]=c(cov_freq[[4]], coverage)
  
  load(paste0(wd1, "/cov_freq.ss", sp, "tt20ee2.RData"))
  cov_freq[[5]]=coverage
  load(paste0(wd2, "/cov_freq.ss", sp, "tt20ee2.RData"))
  cov_freq[[5]]=c(cov_freq[[5]], coverage)
  
  load(paste0(wd1, "/cov_freq.ss", sp, "tt20ee3.RData"))
  cov_freq[[6]]=coverage
  load(paste0(wd2, "/cov_freq.ss", sp, "tt20ee3.RData"))
  cov_freq[[6]]=c(cov_freq[[6]], coverage)
  
  load(paste0(wd1, "/cov_freq.ss", sp, "tt40ee1.RData"))
  cov_freq[[7]]=coverage
  load(paste0(wd2, "/cov_freq.ss", sp, "tt40ee1.RData"))
  cov_freq[[7]]=c(cov_freq[[7]], coverage)
  
  load(paste0(wd1, "/cov_freq.ss", sp, "tt40ee2.RData"))
  cov_freq[[8]]=coverage
  load(paste0(wd2, "/cov_freq.ss", sp, "tt40ee2.RData"))
  cov_freq[[8]]=c(cov_freq[[8]], coverage)
  
  load(paste0(wd1, "/cov_freq.ss", sp, "tt40ee3.RData"))
  cov_freq[[9]]=coverage
  load(paste0(wd2, "/cov_freq.ss", sp, "tt40ee3.RData"))
  cov_freq[[9]]=c(cov_freq[[9]], coverage) 
  

  out=list()
  out[[1]]=sim_l
  out[[2]]=cal_l
  out[[3]]=cov_l
  out[[4]]=cov_freq
  return(out)
  
}
