install.packages("xlsx")
library(xlsx)
#wd<-setwd("/home/tata/Dropbox/Network Theory/Simulaciones R/")
wd<-setwd("C:/Users/prestamour/Documents")

#Funcion para simulacion pn=1, pr=0 dimension=1000 
rm(list=ls())
mini_val_pn_1_pr_0_1000<-function(G){
  mr<-10
  mn<-10
  pr<-0
  pn<-1
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

mini_val_pn_1_pr_0_1000(G)

#Simulaciones para pn=1 y dimension=100
data_pn_1_pr_0_dim_1000<-cbind(sapply(1:10000, mini_val_pn_1_pr_0_1000))



#Funcion para simulacion pn=1, pr=1 dimension=1000 
#rm(list=ls())
mini_val_pn_1_pr_1_1000<-function(G){
  mr<-10
  mn<-10
  pr<-1
  pn<-1
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

mini_val_pn_1_pr_1_1000(G)

#Simulaciones para pn=1 y dimension=100
data_pn_1_pr_1_dim_1000<-cbind(sapply(1:10000, mini_val_pn_1_pr_1_1000))

#Funcion para simulacion pn=0, pr=1 dimension=1000 
#rm(list=ls())
mini_val_pn_0_pr_1_1000<-function(G){
  mr<-10
  mn<-10
  pr<-1
  pn<-0
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

mini_val_pn_0_pr_1_1000(G)

#Simulaciones para pn=1 y dimension=100
data_pn_0_pr_1_dim_1000<-cbind(sapply(1:10000, mini_val_pn_0_pr_1_1000))

data_1000<-cbind(data_pn_1_pr_0_dim_1000, data_pn_1_pr_1_dim_1000, data_pn_0_pr_1_dim_1000)

colnames(data_100) <- c("Pref. Attach. pn=1; pr=0", "Hybrid pn=1; pr=1", "Unif. Random pn=0; pr=1")


write.xlsx(data_1000, "wd_1000.xlsx")
