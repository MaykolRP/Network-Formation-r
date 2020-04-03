rm(list=ls())

#install.packages("igraph")
#install.packages("poweRlaw")
#install.packages("parallel")
#install.packages("MASS")
#install.packages("snow")

library(igraph)
library(poweRlaw)
library(parallel)
library(MASS)
library(snow)


medidas<-function(trial,pr){
# pr=0  #solo debe cambiar esto
  dime<-1000
  mr<-10
  mn<-10
  pn<-1-pr
  ph<-0.5
  #r=(pr*mr)/(pn*mn)
  #Matriz de dimensión=dime x dime de ceros
  G<-matrix(0,nrow=dime,ncol=dime)
  #Para mr+mn+1 x mr+mn+1 se construye el gráfo inicial, antes de la escogencia de los
  #parents nodes de los nuevos nodos(mr+mn+2,dime)
  for (i in 1:(mn+mr+1)){
    for (j in 1:(mn+mr+1)){
      if(rbinom(1, 1, ph)==1){
        G[i,j]=1
        G[j,i]=1
      }
      if (i==j){
        G[i,j]<-0
      }
    }
  }
  #G
  #Escogencia de los nuevos nodos i in (mr+mn+2,dime)
  for(i in ((mn+mr+1)+1):dime){
    #Se crea una secuencia 
    h<- sample(seq(1,i-1,1),replace=FALSE)
    #(h<mr+1) arroja un boleano para los elementos de la secuencia h, que son menores
    #a mr+1. Luego which(h<(mr+1)) identifica la posición de los individuos que son menores
    #a mr+1(EStos conforman el conjunto parents).
    parents<-which(h<(mr+1))
    #La matriz debe ser simétrica, aqu???? entra pr para que los parents se conecten o no.
    lp<-length(parents)
    for(j in 1:lp){
      if (rbinom(1, 1, pr)==1){
        G[i,parents[j]]=1
        G[parents[j],i]=1
      }
    }
    #Ahora se construirá el conjunto de los amigos de los parents
    nghbdparents<-rep(0,dime)
    lp<-length(parents)
    #Para cada uno de los parents nodes se precisa saber, cuales son sus vecinos.
    for(j in 1:lp){
      nghbdparents<-nghbdparents+G[parents[j],]
    }
    #Ahora se indexaran los vecinos de cada parent.
    indexnghbdparents<-which(as.logical(nghbdparents))
    lip<-length(indexnghbdparents)
    l<- sample(seq(1,lip,1),replace=FALSE)
    #el individuo i debe escoger de los vecinos de los parent nodes a lo más mn posibles amigos
    nonzeroindm<-which(l<(mn+1))
    lnon<-length(nonzeroindm)
    for (j in 1:lnon){
      heads<-rbinom(1, 1, pn)
      if (i==indexnghbdparents[j]){
        G[i,indexnghbdparents[j]]<-0
        G[indexnghbdparents[j],i]<-0
      }
      if (heads==1 & G[i,indexnghbdparents[j]]==0){
        G[i,indexnghbdparents[j]]<-1
        G[indexnghbdparents[j],i]<-1
      } 
    }
  }
  #Degree Distribution
  library(igraph)
  #Calcular y reportar los arcos generados entre grafo i,i-1
  H<-graph_from_adjacency_matrix(G)
  d=degree(H)
  #fd=degree_distribution(H)
  #r fit
  m<-pr*mr+pn*mn
  Fd<-ecdf(d)
  y<-log(1-Fd(d))
  regr<-function(r){
    xi<-log(d+r*m)
    yx<-data.frame(cbind(y,xi))
    yx <- yx[!is.infinite(y),]
    regi<-lm(y~xi,yx)
    return(as.numeric(-(1+coef(regi)[2])))
  }
  r0<-0
  r_i<-regr(r0)
  while(abs(r0-r_i)>0.001){
    r0=r_i
    r_i<-regr(r0)
  }
  return(out=list(rfit=r_i,deg=d))
}

#Estimate rfit and obtain degree distribution
#out<-medidas(1,0.5)

#test one time
#prV<-c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
#trials <- seq(1,2)
#for (i in 1:length(prV)){
#outL=lapply(trials, medidas, pr=prV[i])
#}

#time
#not parallel
#system.time(lapply(trials, medidas, pr=0))
#parallel
#numCores <- detectCores()
#cl<-makeCluster(numCores,type="SOCK")
#  system.time(clusterApply(cl, trials,medidas))
#  stopCluster(cl)

#use
prV<-c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
trials <- seq(1,2)  
numCores <- detectCores()
cl<-makeCluster(numCores,type="SOCK")
for (i in 1:length(prV)){
 results_p <-clusterApply(cl, trials,medidas,pr=prV[i])
 save(results_p,file=paste('DegreeD_',toString(prV[i]),'.RData'))
}
stopCluster(cl)
