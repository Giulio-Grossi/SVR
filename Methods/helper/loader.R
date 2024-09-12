
loader<-function(folder, sp){
sim_l=list()
cal_l=list()
cov_l=list()
method=c("SC","SR","PR","BVR", "MGP")
wd=paste("C:/Users/giuli/Desktop/cluster/",folder, sep="")
#wd="C:/Users/giuli/Desktop/cluster/0502/SS0.01"

## simulazioni
load(paste0(wd, "/sim.ss", sp, "tt10ee1.RData"))
sim_l[[1]]=pippo
load(paste0(wd, "/sim.ss", sp, "tt10ee2.RData"))
sim_l[[2]]=pippo
load(paste0(wd, "/sim.ss", sp, "tt10ee3.RData"))
sim_l[[3]]=pippo

load(paste0(wd, "/sim.ss", sp, "tt20ee1.RData"))
sim_l[[4]]=pippo
load(paste0(wd, "/sim.ss", sp, "tt20ee2.RData"))
sim_l[[5]]=pippo
load(paste0(wd, "/sim.ss", sp, "tt20ee3.RData"))
sim_l[[6]]=pippo

load(paste0(wd, "/sim.ss", sp, "tt40ee1.RData"))
sim_l[[7]]=pippo
load(paste0(wd, "/sim.ss", sp, "tt40ee2.RData"))
sim_l[[8]]=pippo
load(paste0(wd, "/sim.ss", sp, "tt40ee3.RData"))
sim_l[[9]]=pippo

### calcoli

load(paste0(wd, "/cal.ss", sp, "tt10ee1.RData"))
cal_l[[1]]=pluto
load(paste0(wd, "/cal.ss", sp, "tt10ee2.RData"))
cal_l[[2]]=pluto
load(paste0(wd, "/cal.ss", sp, "tt10ee3.RData"))
cal_l[[3]]=pluto

load(paste0(wd, "/cal.ss", sp, "tt20ee1.RData"))
cal_l[[4]]=pluto
load(paste0(wd, "/cal.ss", sp, "tt20ee2.RData"))
cal_l[[5]]=pluto
load(paste0(wd, "/cal.ss", sp, "tt20ee3.RData"))
cal_l[[6]]=pluto

load(paste0(wd, "/cal.ss", sp, "tt40ee1.RData"))
cal_l[[7]]=pluto
load(paste0(wd, "/cal.ss", sp, "tt40ee2.RData"))
cal_l[[8]]=pluto
load(paste0(wd, "/cal.ss", sp, "tt40ee3.RData"))
cal_l[[9]]=pluto

### coperture

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

out=list()
out[[1]]=sim_l
out[[2]]=cal_l
out[[3]]=cov_l
return(out)

}
