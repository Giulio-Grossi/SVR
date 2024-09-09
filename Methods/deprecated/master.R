##################################
#################################
rm (list = ls (all = TRUE)) # caution: removes everything
source("fused.R")
source("synth.R")
source("x_matrix.R")
source("sim_fx.R")
source("sim_trend.R")
source("est_fx.R")
source("tcv_fused.R")
library(spatstat)
library(rstan)
library(doSNOW)
library(Matrix)
library(LowRankQP)
library(parallel)
library(doParallel)
library(foreach)
rstan_options(auto_write = TRUE)


# setting hyperparameters 

num_controls=20
bands=8
time_periods=20
t0=15
iter=10
warm=3
loop=4
m=vector();st=vector()
for (i in 1:(num_controls+bands)){
  locm=mean(sim[,i])
  locst=sd(sim[,i])
  sim[,i]=(sim[,i]+locm)*locst
}


theta=list()
effi=list()
for (l in 1:loop){
  set.seed(l)
  sim=simulations(num_controls, time_periods, t0, bands)
  est=estimation_flo(scale(sim), iter, warm)
  theta[[l]]=est[[2]]
  p.sep.scm=est[[1]][[1]]
  p.fus.rid=est[[1]][[2]]
  p.gp=est[[1]][[3]]
  ym =sim[, 1:bands]
  bias=vector()
  for (i in 1: 3){
    loc=est[[1]][[i]]
    loc2=t(t(ym-loc)/ym[1,])
    bias=cbind(bias,rowSums(sqrt(loc2^2)))
  }
  effi[[l]]=bias
}


## non normali
effi=list()
theta=list()
loop=80
time_periods=80
for (i in 1:loop){
  effi[[i]]=res[[1]][[i]]
  theta[[i]]=res[[2]][[i]]
}

scm=cbind(seq(1,time_periods,1),1)
rid=cbind(seq(1,time_periods,1),2)
gp=cbind(seq(1,time_periods,1),3)
loop=80
time_periods=80
for (h in 1:loop){
  loc=effi[[h]]
  scm=cbind(scm, loc[,1])
  rid=cbind(rid, loc[,2])
  gp=cbind(gp, loc[,3])
}

total=as.data.frame(rbind(scm, rid, gp))
colnames(total)[1:2]=c("time", "method")
library(reshape2)
long <- melt(total, id.vars = c("time", "method"))

long=subset(long, time>59)

library(ggplot2)
p <- ggplot(data = long, aes(x = time, y = value, group=interaction(time, method)))+theme_bw()+
 geom_boxplot(aes(fill = factor(method), color = factor(method)), alpha=.5)+ ylim(0,5)
 
 p
 
 plot(ym[,1], type="l")
lines(p.sep.scm[,1], col="red")
lines(p.fus.rid[,1], col="blue")

## PARALLEL #####
numCores <- detectCores()
numCores
registerDoParallel(20)

sim=simulations_2(num_controls, time_periods, t0, bands)
est=estimation(sim)


hhh=foreach (q =1:loop)%dopar%{
  sim=simulations_2(num_controls, time_periods, t0, bands)
  est=estimation(sim)
}

hhh
stopImplicitCluster()
save(hhh, file="sim50.Rdata")

save(listr, file="sim30.Rdata")

## simulations and data settings  
sim=simulations(num_controls, time_periods, t0, bands)
## estimations and collecting results 
res=estimation(sim)
system.time({
  listr=list()
  for ( i in 1:30){
    sim=simulations(num_controls, time_periods, t0, bands)
    ## estimations and collecting results 
    res=estimation(sim)
    listr[[i]]=res
    print(i)
  }
  
})

listr


# full<-function(num_controls, time_periods, t0, bands){
#   #set.seed(i)
#   ## simulations and data settings  
#   sim=simulations(num_controls, time_periods, t0, bands)
#   ## estimations and collecting results 
#   res=estimation(sim)
#   return(res)
# }

beep(8)

hhh




foreach(q =loop)%dopar%{print(q)}