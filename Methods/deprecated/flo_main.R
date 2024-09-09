##################################
#################################
rm (list = ls (all = TRUE)) # caution: removes everything
source("fused.R")
source("synth.R")
source("x_matrix.R")
source("sim_fx.R")
source("est_fx.R")
source("tcv_fused.R")
source("sim2.R")
library(spatstat)
library(rstan)
library(Matrix)
library(LowRankQP)
library(parallel)
library(doParallel)
library(foreach)

# setting hyperparameters 
num_controls=40
bands=4
time_periods=20
t0=15

## simulations and data settings  
sim=simulations_2(num_controls, time_periods, t0, bands)
## estimations and collecting results 
res=estimation(sim, iter=500, warm=150)
system.time({
listr=list()
for ( i in 1:2){
  sim=simulations(num_controls, time_periods, t0, bands)
  ## estimations and collecting results 
  res=estimation(sim)
  listr[[i]]=res
}
})

full<-function(num_controls, time_periods, t0, bands){
  #set.seed(i)
  ## simulations and data settings  
  sim=simulations(num_controls, time_periods, t0, bands)
  ## estimations and collecting results 
  res=estimation(sim)
return(res)
  }

beep(8)


## PARALLEL #####
numCores <- detectCores()
numCores
registerDoParallel(4)
loop=seq(1,10,1)
system.time({
 hhh= foreach (q =loop)%dopar%{
    sim=simulations(num_controls, time_periods, t0, bands)
    est=estimation(sim)
    
  }
})
stopImplicitCluster()




