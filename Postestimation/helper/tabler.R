
tabler<-function(res, tt0, post_treatment){
mse_tab=vector()
nn=c("tt10 - IID", "tt10 - 40%", "tt10 - 70%",
     "tt20 - IID", "tt20 - 40%", "tt20 - 70%",
     "tt40 - IID", "tt40 - 40%", "tt40 - 70%"  )
res_gp=res[[1]]
res_sc=res[[2]]
res_sr=res[[3]]
res_vr=res[[4]]
res_br=res[[5]]
res_bsc=res[[6]]
for(ii in 1:9){
  t0=tt0[ii]
  time_periods=tt0[ii] + post_treatment[ii]
  mse_tab=rbind(mse_tab, c(mean(apply(res_gp[[ii]], 1, mean)[(t0+1):time_periods], na.rm=T),
                           mean(apply(res_sc[[ii]], 1, mean)[(t0+1):time_periods], na.rm=T),
                           mean(apply(res_sr[[ii]], 1, mean)[(t0+1):time_periods], na.rm=T),
                           mean(apply(res_br[[ii]], 1, mean)[(t0+1):time_periods], na.rm=T), 
                           mean(apply(res_bsc[[ii]], 1, mean)[(t0+1):time_periods], na.rm=T)))
}


colnames(mse_tab)=c("SMaC", "SCM", "Ridge", "BayVR", "BaySC")
rownames(mse_tab)=nn
return(mse_tab)
}

# 
# post_treatment=pt1
# 
# 
# mse_tab=vector()
# res_gp=res[[1]]
# res_sc=res[[2]]
# res_sr=res[[3]]
# res_vr=res[[4]]
# res_br=res[[5]]
# for(ii in 1:9){
#   t0=tt0[ii]
#   time_periods=tt0[ii] + post_treatment[ii]
#   mse_tab=rbind(mse_tab, c(mean(apply(res_gp[[ii]], 1, mean)[(t0+1):time_periods], na.rm=T),
#                            mean(apply(res_sc[[ii]], 1, mean)[(t0+1):time_periods], na.rm=T),
#                            mean(apply(res_sr[[ii]], 1, mean)[(t0+1):time_periods], na.rm=T),
#                            mean(apply(res_br[[ii]], 1, mean)[(t0+1):time_periods], na.rm=T)))
# }
# 
# colnames(mse_tab)=c("SMaC", "SCM", "Ridge", "BayVR")
# rownames(mse_tab)=nn
# m1=mse_tab
# 
# 
# post_treatment=pt2
# mse_tab=vector()
# res_gp=res[[1]]
# res_sc=res[[2]]
# res_sr=res[[3]]
# res_vr=res[[4]]
# res_br=res[[5]]
# for(ii in 1:9){
#   t0=tt0[ii]
#   time_periods=tt0[ii] + post_treatment[ii]
#   mse_tab=rbind(mse_tab, c(mean(apply(res_gp[[ii]], 1, mean)[(t0+1):time_periods], na.rm=T),
#                            mean(apply(res_sc[[ii]], 1, mean)[(t0+1):time_periods], na.rm=T),
#                            mean(apply(res_sr[[ii]], 1, mean)[(t0+1):time_periods], na.rm=T),
#                            mean(apply(res_br[[ii]], 1, mean)[(t0+1):time_periods], na.rm=T)))
# }
# 
# colnames(mse_tab)=c("SMaC", "SCM", "Ridge", "BayVR")
# rownames(mse_tab)=nn
# m2=mse_tab
# 
# 
# 
# mse_tab=vector()
# res_gp=res[[1]]
# res_sc=res[[2]]
# res_sr=res[[3]]
# res_vr=res[[4]]
# res_br=res[[5]]
# for(ii in 1:9){
#   t0=tt0[ii]
#   time_periods=tt0[ii] + post_treatment[ii]
#   mse_tab=rbind(mse_tab, c(mean(apply(res_gp[[ii]], 1, mean)[(t0+1):time_periods], na.rm=T),
#                            mean(apply(res_sc[[ii]], 1, mean)[(t0+1):time_periods], na.rm=T),
#                            mean(apply(res_sr[[ii]], 1, mean)[(t0+1):time_periods], na.rm=T),
#                            mean(apply(res_br[[ii]], 1, mean)[(t0+1):time_periods], na.rm=T)))
# }
# 
# colnames(mse_tab)=c("SMaC", "SCM", "Ridge", "BayVR")
# rownames(mse_tab)=nn
# return(mse_tab)
