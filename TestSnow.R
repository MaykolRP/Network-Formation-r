rm(list=ls())

#install.packages("parallel")
library(parallel)
#install.packages("snow")
library(snow)

z=vector('list',4)
z=1:4

#not parallel
system.time(lapply(z,function(x) Sys.sleep(1)))

numCores <- detectCores()
numCores

#parallel
cl<-makeCluster(numCores,type="SOCK")
  system.time(clusterApply(cl, z,function(x) Sys.sleep(1)))
  stopCluster(cl)