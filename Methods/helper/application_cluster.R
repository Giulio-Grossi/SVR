args <- commandArgs(TRUE)
args
index<-as.numeric(args[1])
puppi=1
out_path <- 'Output/1_sims/Results'
source("Functions/fused.R")
source ("Functions/synth.R")
source("Functions/x_matrix.R")
source("Functions/tcv.fused.R")
source("Functions/calculations.R")
source("Functions/ridge.R")
source("Functions/tcv.ridge.R")
source("Functions/t.stat.R")
source("Functions_december/normalized_stats.R")
source("Functions/x_matrix_int.R")
source("replication_novembre/calculation_mode.R")
source("replication_novembre/est_mode.R")
# source("replication_agosto/sim_june.R")
# source("replication_novembre/sim_ale.R")
# source("replication_novembre/sim_feb.R")
source("replication/gaussian_kernel.R")
library(glmnet)

density <- read.csv("density.csv")
density<- t(density)
ss<-density[-c(1:3, nrow(density)),]
View(ss)
num_controls=ncol(density)-5
time_periods=length(seq(1996:2018))
t0=10
iter=5000
warm=2500
bands <- 5
treated_radius=seq(120, 600, 120)
treated_radius = treated_radius/max(treated_radius)
print(ss)
print(iter)
mode=c("SC", "SR")
est=estimation_mode(ss, iter, warm, norm=F, mode )
cal=calculations_mode(ss, est, norm=F, mode, .5)
theta=nt.stat(ss, cal, norm=F)
stima_sr=ss[,1:5] - cal$SR
stima_sc=ss[,1:5] - cal$SC


res=list()
res[[1]]=ss
res[[2]]=est

out_file=paste0(out_path, "/application_",puppi, ".RData")
out_file
save(res, file=out_file)