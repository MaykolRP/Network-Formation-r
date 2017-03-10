m=3.5
d<-c(0:27)

#Guess Inicial
r0<-0.1

#Para poder obtener la función de distribución de acumulación se debe primero construir 
#La degree distribution p_k:= N_k/N
#Función de acumulación (Degree Distribution)
#Con la información que se tiene de "Ham Radio" Se construye degree distribution (dd) 
degree<-c(0:27)
operators<-c(3,11,5,2,1,0,2,2,3,3,2,2,1,1,0,0,1,0,0,0,1,1,1,0,0,1,0,1)
tabla<-cbind(degree,operators)
tabla
tabla[1,2]
dd<-tabla[,2]/44
dd

#Ya teniendo degree distribution (dd) hallamos la función de distribución acumulada cdf

for(i in c(2:28)){
  dd[0]<-0
  dd[1]<-dd[1]
  dd[i]=dd[i-1]+dd[i]
  print(c(dd[i]))
}
cdf<-dd
cdf

#Se define y y x para estimar b1 y así construir r1 apartir de r0, d, m y cdf

y<-log(1-cdf)



while(abs(r0-r1)<0.00001){
  y<-log(1-cdf)
  x<-log(d+r0*m)
  reg<-lm(y~x, na.action=na.omit)
  b1<-coef(reg)[2]
  b1
  r1=1+b1
  print(r1)
  r0=r1
}
