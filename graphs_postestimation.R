################################################################################
### GRAPHS 
###############################################################################
## matrix for names

library(ggplot2)

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

melt_001<-melter(point_001, stat="bias",
                scenarios=scenarios, nna=nna, rho=0.01, tt0)

melt_02<-melter(point_02, stat="MSE",
                 scenarios=scenarios, nna=nna, rho=0.2, tt0)

melt_04<-melter(point_04, stat="bias",
                scenarios=scenarios, nna=nna, rho=0.4, tt0)

melt_06<-melter(point_06, stat="bias",
                scenarios=scenarios, nna=nna, rho=0.6, tt0)

melt_all<-rbind(melt_001,  melt_04, melt_06)

mm=as.data.frame(mm)
melt_all$Method=as.factor(melt_all$Method)
melt_all$Time=as.integer(melt_all$Time)
melt_all$Error=as.factor(melt_all$Error)
melt_all$Lengthscale=as.factor(melt_all$Lengthscale)
melt_all$T0=as.factor(melt_all$T0)

mm10=subset(melt_all, melt_all$T0==10)
mm20=subset(melt_all, melt_all$T0==20)
mm40=subset(mm, mm$T0==40)

mm10$MSE<-(mm10$MSE)^2

g <- ggplot() + geom_vline(xintercept = 10,  
                           color = "black", size=.3)+
  geom_line(aes(x = Time, y = MSE, group = Method, color = Method),
            data = mm10, size=.7)+ 
  ylim(0,.1)

g + facet_grid(Lengthscale  ~ Error) + theme_bw() +
  scale_color_manual(values =c('#D55E00',   '#E69F00',   "#56B4E9", '#009E73',
                               '#CC79A7' ,'#999999' ))


m_smac = subset(melt_all, melt_all$Method=="SMAC")
g <- ggplot() + ggtitle("SMAC") + 
  geom_line(aes(x = Time, y = MSE, group = Lengthscale, color = Lengthscale),
            data = m_smac,  size=.7)

g + facet_grid(Error  ~ T0, scales = "free_x") + theme_bw() +
  scale_color_manual(values =c('#D55E00',   '#E69F00',   "#56B4E9", '#009E73',
                               '#CC79A7' ,'#999999' ))

