######################################################
########################################################
## master script for computations on hipergator  #######
########################################################
########################################################

args <- commandArgs(TRUE)
args
sp_range <- as.numeric(args[1])
tt_periods<-as.numeric(args[2])
errors_sp<-as.numeric(args[3])
index<-as.numeric(args[4])
set.seed(index)

setwd('C:/Users/giuli/Documents/SMAC')

# Sourcing in code from other files.
source("Functions/sim_model_function.R")
source("Functions/estimation_function.R")
source("Functions/calculation_function.R")
source("Functions/point_estimate_function.R")
source("Functions/ci_function.R")
source("Functions/coverage_function.R")
source("Methods/helper/preset_function.R")
source("Methods/helper/ci_shen.R")
source("Methods/helper/ci_bayes.R")
source("Methods/helper/coverage.R")

# Function for performing synthetic controls.
source("Methods/helper/SCM_function.R")
source('Methods/sepSC_method.R')
source('Methods/sepSC_calculation.R')

# Function for performing separate vertical regression with ridge.
source('Methods/sepSR_method.R')
source('Methods/sepSR_calculation.R')


# Function for performing Bayesian separate vertical regression.
source('Methods/sepBVR_method.R')

# Function for performing Bayesian synthetic control.
source('Methods/sepBSC_method.R')

# Function for performing SMAC. 
source('Methods/SMAC_method.R')

#------- -GP- I did not check the following functions --------- #
# ------- Functions for fused and pooled ridge ------- # 

# # Functions for performing the pooled ridge.
# source("Methods/ignore/tcv_ridge.R")
# source("Methods/ignore/x_matrix.R")
# source("Methods/ignore/ridge.R")
# 
# # Functions for performing fused ridge.
# source("Methods/ignore/fused.R")
# source("Methods/ignore/tcv_fused.R")

# ------- End of functions I did not check. ----------- #

library(LowRankQP)
library(glmnet)
library(rstan)
library(spatstat)
library(Matrix)
library(fungible)
rstan_options(auto_write = FALSE)
#out_path <- 'Output/1_sims/Results/'

# setting hyperparame
## dim parameter
num_controls=10
t0=tt_periods
time_periods=t0 + 20
time_periods_controls <- 80  # -GP- Do not change this with t0.
bands <-3
## sampling pars
iter=6000
warm=2000
## sim pars

sp_var=.4
tt_var=0.3 ^ 2  # -GP- I reduced the tt_var a little.
ti_var=0.7 ^ 2  # -GP- I squared this to make it a variance and comparable to tt_var
bi_var=0.5 ^ 2  # -GP- I squared this to make it a variance and comparable to sp_var
tt_range=.05
sp_nugget=0.001
tt_nugget=0.15 ^ 2  # -GP- Adding some temporal nugget to the controls.
rho_error=.2	  

## errors def
seed_b=seed_t=seed_e=index
if(errors_sp==1){
  e_weight=0  # Proportion of error that is spatial
  share_error=0.4  # Noise-signal ratio (sd for error term as % of signal)
} else if(errors_sp==2){
  e_weight=.5
  share_error=0.4
} else if(errors_sp==3){
  e_weight=.5
  share_error=0.7 # -GG- : corrected mistake (21/08)
}
print(errors_sp)
#error=sqrt(.55)
#treated_radius = sort(scale(runif(bands, 0.01, 1)))
treated_radius <- seq(0, 1, length.out = bands)

## store results over loop
method=c("SC","SR", "BVR","BSC","SMAC")
print(treated_radius)
#tt_range=0.01

sim=sim_model(seed_b = seed_b, seed_t = seed_t, seed_e = seed_e,
              time_periods = time_periods, 
              time_periods_controls = time_periods_controls,
              bands = bands, num_controls = num_controls,
              sp_var = sp_var, sp_range = sp_range, bi_var = bi_var, 
              tt_var = tt_var,
              tt_range = tt_range, ti_var = ti_var, sp_nugget = sp_nugget,
              tt_nugget = tt_nugget,
              e_weight = e_weight, share_error = share_error)

beta_true=sim$beta
sim=sim$sim  # The potential outcomes under control.

set.seed(index)

# -GP- The standardization using preset() is moved inside estimation_mode and
# calculations_mode().

iter=10
warm=5

est = estimation(sim = sim, t0 = t0, bands = bands, iter = iter, warm = warm,
                 norm = T, method = method)

cal = calculation(sim=sim, est=est, norm=T)
point = point_est(sim,cal) ## it calculates bias and MSE
ci= ci_function(sim=sim, est=est, cal=cal, norm = T)
coverage<-coverage_function(sim, ci)



res=list()
res[[1]]=sim
res[[2]]=est
res[[3]]<-cal
res[[4]]<- beta_true
res[[5]]<- point
res[[6]]<- ci 
res[[7]]<- coverage



out_path <- paste0('Output/apr_sims/Results/ss', sp_range, '/tt', tt_periods,
                   '/ee', errors_sp)
out_path
out_file=paste0(out_path, "/res_",index, ".RData")
out_file
save(res, file=out_file)
print("fine primo esperimento")
################################################################################

