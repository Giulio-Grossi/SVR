##################### MSE CALCULATIONS IN LOCAL ################################
################################################################################
#' 
#' @param dir1 is the char vector with the
#' directories  in which are stores the first 200 datasets
#' 
#' @param dir2 is the char vector with the 
#' directories in which are stores the second 200 datasets
#' 
#' @param rho is the vector of dimension 3 with the spatial correlations 
#' 
#' @param exp is to estimate bias (exp=1) or MSE (exp=2) of the point estimate
#' Depends on the binder.R, loader.R, tabler.R and wrapper.R


postestimation <- function(dir1, dir2, rho, exp){
  # rho_loc=rho[ii]
  # dir_loc=dir[ii]

# ss=0.0001
oo=binder(dir,dir,  rho)
sim_l=oo[[1]]; cal_l=oo[[2]]; cov_l=oo[[3]]
res=wrapper(sim_l, cal_l, cov_l, exp)
m1=tabler(res, tt0, pt1)
m2=tabler(res, tt0, pt2)
m3=tabler(res, tt0, pt3)
mse_tab=cbind(m1,m2,m3)
out=kable(round(mse_tab,3), format="latex",booktabs=T)
print(out)
return(res)
}
# 
# # ss=0.2
# oo=binder("2507/SS0.2","2507/SS0.2",  "0.2")
# sim_l=oo[[1]]; cal_l=oo[[2]]; cov_l=oo[[3]]; cov_freq=oo[[4]]
# res=wrapper(sim_l, cal_l, cov_l, 2)
# m1=tabler(res, tt0, pt1)
# m2=tabler(res, tt0, pt2)
# m3=tabler(res, tt0, pt3)
# 
# library(kableExtra)
# mse_tab=cbind(m1,m2,m3)
# out=kable(round(mse_tab,3), format="latex",booktabs=T)
# out
# res_gp2=res[[1]]
# res_sc2=res[[2]]
# res_sr2=res[[3]]
# res_vr2=res[[4]]
# res_br2=res[[5]]
# res_bsc2=res[[6]]
# #ss=0.4
# oo=binder("2507/SS0.4","2507/SS0.4",  "0.4")
# sim_l=oo[[1]]; cal_l=oo[[2]]; cov_l=oo[[3]]
# res=wrapper(sim_l, cal_l, cov_l, 2)
# m1=tabler(res, tt0, pt1)
# m2=tabler(res, tt0, pt2)
# m3=tabler(res, tt0, pt3)
# 
# library(kableExtra)
# mse_tab=cbind(m1,m2,m3)
# out=kable(round(mse_tab,3), format="latex",booktabs=T)
# out
# res_gp3=res[[1]]
# res_sc3=res[[2]]
# res_sr3=res[[3]]
# res_vr3=res[[4]]
# res_br3=res[[5]]
# res_bsc3=res[[6]]
# 
# }
