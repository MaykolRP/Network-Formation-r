rm(list=ls())
#parametros iniciales
mr<-6
mn<-5
pr<-0.7
pn<-0.5
ph<-0.5
dime<-16

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

G

#Escogencia de los nuevos nodos i in (mr+mn+2,dime)
for(i in ((mn+mr+1)+1):dime){
  #Se crea una secuencia 
  h<- sample(seq(1,i-1,1),replace=FALSE)
  #(h<mr+1) arroja un boleano para los elementos de la secuencia h, que son menores
  #a mr+1. Luego which(h<(mr+1)) identifica la posición de los individuos que son menores
  #a mr+1(EStos conforman el conjunto parents).
  parents<-which(h<(mr+1))
  #La matriz debe ser simétrica, aquí entra pr para que los parents se conecten o no.
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
G
isSymmetric(G)
