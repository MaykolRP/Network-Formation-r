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
  #Arcs
  arc <- vector(mode="integer", length=979)
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
  #Calcular y reportar los arcos generados entre grafo i,i-1
  kk<-1
  library(igraph)
  H<-graph_from_adjacency_matrix(G)
  arc[kk]<-gsize(H)
  #print(gsize(H))
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
    library(igraph)
    #Calcular y reportar los arcos generados entre grafo i,i-1
    kk<-kk+1
    H<-graph_from_adjacency_matrix(G)
    arc[kk]<-gsize(H)
    #print(gsize(H))
  }
  #Degree Distribution
  library(igraph)
  H<-graph_from_adjacency_matrix(G)
  d=degree(H)
  return(out=list(arcos=arc,deg=d))
}

#Estimate rfit and obtain degree distribution
out_0<-medidas(1,0)
delta_arc_0<-diff(out_0$arcos)
summary(out_0$deg)
cat("Sharpe:",mean(out_0$deg)/sqrt(var(out_0$deg)),"\n")
summary(delta_arc_0)

out_05<-medidas(1,0.5)
delta_arc_05<-diff(out_05$arcos)
summary(out_05$deg)
cat("Sharpe:",mean(out_05$deg)/sqrt(var(out_05$deg)),"\n")
summary(delta_arc_05)

out_1<-medidas(1,1)
delta_arc_1<-diff(out_1$arcos)
summary(out_1$deg)
cat("Sharpe:",mean(out_1$deg)/sqrt(var(out_1$deg)),"\n")
summary(delta_arc_1)

#obtain degree distribution
PA<-out_0$deg
HI<-out_05$deg
ER<-out_1$deg

Fd_pa<-ecdf(PA)
Fd_hi<-ecdf(HI)
Fd_er<-ecdf(ER)

plot(Fd_pa)
plot(Fd_hi)
plot(Fd_er)

#summary(Fd_pa)
#cat("Sharpe:",mean(PA)/sqrt(var(PA)),"\n")
#summary(Fd_hi)
#cat("Sharpe:",mean(HI)/sqrt(var(HI)),"\n")
#summary(Fd_er)
#cat("Sharpe:",mean(ER)/sqrt(var(ER)),"\n")


aPA<-sort(PA)
Fd_pat<-1-Fd_pa(aPA)
aHI<-sort(HI)
Fd_hit<-1-Fd_hi(aHI)
aER<-sort(ER)
Fd_ert<-1-Fd_er(aER)

plot(Fd_pat,type="l",col="red",ylab="1-F(d)")
lines(Fd_hit,col="green")
lines(Fd_ert,col="blue")
legend("bottomleft", legend=c("pr=0.1","pr=0.5","pr=1"),
       col=c("red","green", "blue"), lty=1, cex=0.8)

