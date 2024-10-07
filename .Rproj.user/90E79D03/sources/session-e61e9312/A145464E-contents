dim(scpi_germany)

library(tidyr)
sim <- as.data.frame(sim)
sim_t<-cbind(seq(1,nrow(sim),1), sim)
colnames(sim_t)[1]="time"
data_long <- sim_t %>%
  pivot_longer(
    cols = -time,
    names_to = "id",
    values_to = "outcome"
  )
data_long <- data_long[order(data_long$id), ]
df <- scdata(df = data_long, id.var = "id", time.var = "time",
             outcome.var = "outcome",
             period.pre = (1:t0), 
             period.post = ((t0+1):nrow(sim_t)),
             unit.tr = paste0("treated", " ", i),
             unit.co = unique(data_long$id[1:nrow(sim)*((ncol(sim)-bands)-1)]))
result <- scest(df, w.constr = list(name = "simplex"))
result <- scpi(df, w.constr = list(name = "simplex", Q = 1), cores = 1, sims = 100)
scplot(result)
head(sim)


sepSC(sim[1:t0, 1:bands], sim[1:t0, (bands+1):ncol(sim)])
