install.packages("xlsx")
library(xlsx)
install.packages("ggplot2")
library("ggplot2")


rm(list=ls())
mini_val<-function(G,pr){
  mr<-10
  mn<-10
  #pr<-0
  pn<-1-pr
  ph<-0.5
  dime<-1000
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
  #Valores Propios
  eigvv<-eigen(G, only.values = TRUE)
  #M?n Valor propio
  mmvv<-lapply(eigvv[1],min) 
  return(mmvv)
}


p<-numeric(1000)
q<-numeric(1000)
r<-numeric(1000)
s<-numeric(1000)
t<-numeric(1000)
u<-numeric(1000)
v<-numeric(1000)
w<-numeric(1000)
x<-numeric(1000)
y<-numeric(1000)
z<-numeric(1000)

for(i in 1:1000){
  p[i]=mini_val(G,0)
  q[i]=mini_val(G,0.1)
  r[i]=mini_val(G,0.2)
  s[i]=mini_val(G,0.3)
  t[i]=mini_val(G,0.4)
  u[i]=mini_val(G,0.5)
  v[i]=mini_val(G,0.6)
  w[i]=mini_val(G,0.7)
  x[i]=mini_val(G,0.8)
  y[i]=mini_val(G,0.9)
  z[i]=mini_val(G,1)
}

pv<-unlist(p,FALSE)
qv<-unlist(q,FALSE)
rv<-unlist(r,FALSE)
sv<-unlist(s,FALSE)
tv<-unlist(t,FALSE)
uv<-unlist(u,FALSE)
vv<-unlist(v,FALSE)
wv<-unlist(w,FALSE)
xv<-unlist(x,FALSE)
yv<-unlist(y,FALSE)
zv<-unlist(z,FALSE)

df1<-data.frame(pv,qv,rv,sv,tv,uv,vv,wv,yv,zv)
colnames(df1)<-c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
write.xlsx(df1, "min_lambdas1.xlsx")



#z=list()
#for(k in 1:10){
#    for(i in seq(0.1,1,by=0.1)){
#     j=1
#     while(j<2){
#     z[[k]]=mini_val(G,i)
#print(mini_val(G,i))
#     j=j+1
#    }
#  }
#}
#z

#z=list()
#for(k in 1:3){
#  for(i in seq(0.1,1,by=0.1)){
#      for(j in 1:3){
#      z[[k]][j]=mini_val(G,i)
#    }
#  }
#}










