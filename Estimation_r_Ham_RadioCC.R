rm(list=ls()) #clear data

#Estimaci?n del "r" para la red "Ham Radio" N?mero total de nodos de esta red=44

#Average degree
m=3.5

d<-c(0:27)

#Guess Inicial
r0<-0.1

#Para poder obtener la funci?n de distribuci?n de acumulaci?n se debe primero construir 
#La degree distribution p_k:= N_k/N
#Funci?n de acumulaci?n (Degree Distribution)
#Con la informaci?n que se tiene de "Ham Radio" Se construye degree distribution (dd) 
degree<-c(0:27)
operators<-c(3,11,5,2,1,0,2,2,3,3,2,2,1,1,0,0,1,0,0,0,1,1,1,0,0,1,0,1)
tabla<-cbind(degree,operators)
tabla
tabla[1,2]
dd<-tabla[,2]/44
dd
cdf<-cumsum(tabla[,2])/44

#Se define y y x para estimar b1 y as? construir r1 apartir de r0, d, m y cdf

y<-log(1-cdf)
y<-y[-length(y)]
d<-d[-length(d)]
x<-function(d,r,m){
   log(d+r*m)
}

dr<-5
r<-0.5
i<-0
while(dr>0.0001){
  x_<-x(d,r,m)
#  reg<-lm(y~x_-1,na.action = na.omit)
#  rtemp<- (1-coef(reg)[2])
  reg<-lm(y~x_,na.action = na.omit)
  rtemp<- (1-coef(reg)[2])
  print(rtemp)
  dr=abs(r-rtemp)
  r<-rtemp
  rm(rtemp)
  i<-i+1
  }

#Se define una grilla de 1 a 10. Que corre de a 0.1
rv<-seq(0.1,10, by=0.1)
#Se define la función repreg que realiza la regresión y~x_ y toma el beta
repreg<-function(rv1){
  x_<-x(d,rv1,m)
  reg<-lm(y~x_,na.action = na.omit)
  (1-coef(reg)[2])
}
#rconv lo que va a hacer es a cada número de la grilla rv le a aplicar la regres
rconv<-sapply(rv,repreg)
#resgrid combina la grilla rv con rconv
resgrid<-cbind(rv,rconv)
#resconv identifica el min valor absoluto entre rv y rconv
resconv<-resgrid[which.min(abs(rv-rconv)),]
resconv
