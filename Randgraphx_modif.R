rm(list=ls())
#parametros iniciales
mr<-6
mn<-5
pr<-0.25
pn<-0.15
dime<-16
#Escoger un número "k" aleatorio. El cual debe estar entre mr+mn y dim. 
k<-sample(seq(mr+mn+1,dime,1),1)
k
#Dado el número aleatorio k, se construye la matriz G de dimensión dime x dime
G<-matrix(0,nrow=dime,ncol=dime)
G
#Se creará el grafo inicial, luego, para i de 1 hasta k. Se decidirá cuantos vecinos
#tendrá cada nodo. Al menos un nodo tendrá mn+mr vecinos.Y a lo sumo tendran k-1.

for(i in 1:k){
  #r indica un número aleatorio el cual significa la cantidad de vecinos que cada nodo tendrá  
  r<-sample(seq(mr+mn,k-1,1),1)
  #Esa cantidad r de vecinos. ¿Cuales de esos r serán mis vecinos?
  #para esto se toma una permutación de {1,2,....,i-1,i+1,....,k-1}
  h<- sample(seq(1,k-1,1),replace=FALSE)
  #Luego se identifica, cuales nodos son menores a r+1. Exactamente habrán r.
  g<-h<(r+1)
  #Se extraen los identicos
  mask<-c(rep(1,k),rep(0,dime-k))
  mask[i]<-0
  G[i,as.logical(mask)]=g
}
G

#Ya creado el grafo incial.El cual se tiene los individuos de 1 hasta k, con quienes se relacionan 
#Se quieres simula, como se conectará un nuevo nodo k+1 


 

#OJO este for se cierra abajo

#Para los k+1(nuevos nodos) se quiere saber con quienes se relacionan. Tenemos un gráfo inicial
#de 1 hasta k. Ahora los individuos que analiceremos serán nuevos nodos de k+1 hasta dime.
 for(i in (k+1):dime){
  #Se crea una secuencia 
  h<- sample(seq(1,i-1,1),replace=FALSE)
  parents<-which(h<(mr+1))
  lp<-length(parents)
   for(j in 1:lp){
  if (rbinom(1, 1, pr)==1){
    G[i,parents[j]]=1  
  } else {
    G[i,parents[j]]=0  
  }
    }
 


nghbdparents<-rep(0,dime)
lp<-length(parents)
for(j in 1:lp){
  nghbdparents<-nghbdparents+G[parents[j],]
}

indexnghbdparents<-which(as.logical(nghbdparents))
lip<-length(indexnghbdparents)
l<- sample(seq(1,lip,1),replace=FALSE)
nonzeroindm<-which(l<(mn+1))

lnon<-length(nonzeroindm)

for (j in 1:lnon){
heads<-rbinom(1, 1, pr)
if (heads==1 & G[i,indexnghbdparents[j]]==0){
G[i,indexnghbdparents[j]]<-1
} else if(heads==1 & G[i,indexnghbdparents[j]]==1){
G[i,indexnghbdparents[j]]<-1
} else if(heads==0 & G[i,indexnghbdparents[j]]==0){
G[i,indexnghbdparents[j]]<-0
}  else {
#if (heads==0 & G[i,indexnghbdparents[j]]==1){
G[i,indexnghbdparents[j]]<-1
}
}

}

G
