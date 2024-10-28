################################################################################

### MELTER FUNCTION, TO BE USED TO GET A SINGLE DATAFRAME FOR ALL THE SCENARIOS

################################################################################

## auxiliary function 
melter_auxiliary <- function(loc, ii, rho){
exit<-vector()
methods=names(loc)
for(kk in 1:length(loc)){
  aux <- loc[[kk]]
  out<-as.data.frame(cbind(nna[ii,1], nna[ii,2],
                           seq(1:(tt0[ii]+20)),
                           apply(aux,1,mean),
                           methods[kk], rho) )
  colnames(out)<-c("Error", "T0", "Time", stat, "Method", "Lengthscale")
  out[,c(2)]= as.numeric(out[,c(2)])
  out[,c(3)]= as.numeric(out[,c(3)])
  out[,c(4)]= as.numeric(out[,c(4)])
  out[,c(6)]= as.numeric(out[,c(6)])
  exit=rbind(exit, out)
}
return(exit)
}



melter <- function (input, stat, scenarios, nna, rho, tt0){
  
  if(stat=="bias"){
    mm=vector()
    for(ii in 1:length(input)){
    mm<- rbind(mm, melter_auxiliary(input[[scenarios[ii]]]$bias, ii, rho))
    }
  }
  
  if(stat=="MSE"){
    mm=vector()
    for(ii in 1:length(input)){
      mm<- rbind(mm, melter_auxiliary(input[[scenarios[ii]]]$MSE, ii, rho))
    }
  }
  
  if(stat=="coverage"){
    mm=vector()
    for(ii in 1:length(input)){
      mm<- rbind(mm, melter_auxiliary(input[[scenarios[ii]]], ii, rho))
    }
  }
  
  return(mm)
  
}
