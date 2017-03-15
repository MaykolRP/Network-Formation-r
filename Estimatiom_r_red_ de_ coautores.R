rm(list=ls()) #clear data
##### proceso de iteración para la red de coautores#####
d<-c(0:61)
authors<-c(24578,25078,17139,5069,3089,1853,1232,815,631,443,329,226,156,134,87,73,60,51,35,19,27,16,9,11,15,7,7,6,7,1,2,3,1,2,2,0,0,0,0,0,0,2,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1)
length(authors)
tabla_aut<-cbind(d,authors)
tabla_aut
dd_c<-tabla_aut[,2]/81217
dd_c
cdf<-cumsum(tabla_aut[,2])/81217

r0=0.1
m=0.84

y<-log(abs(1-cdf))
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

length(y)
length(x_)
length(d)

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