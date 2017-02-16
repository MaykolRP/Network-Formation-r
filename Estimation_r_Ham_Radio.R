#Estimación del "r" para la red "Ham Radio"

#Definimos r0
#Vector degree d
d<-c(0:27)
#Average degree
m=3.5
#Guess Inicial
r0<- 0.1
#Función de acumulación (Degree Distribution)
degree<-c(0:27)
operators<-c(3,11,5,2,1,0,2,2,3,3,2,2,1,1,0,0,1,0,0,0,1,1,1,0,0,1,0,1)
tabla<-cbind(degree,operators)
tabla
tabla[1,2]
dd<-tabla[,2]/44
dd

y<-log(1-dd)
x<-function(d,r0,m){
  log(d+r0*m)
}
x_<-x(d,r0,m)
#Se realiza la regresión para obtener b1 y así obtener r1
reg<-lm(y~x_)
b1<-coef(reg)[2]
b1
r1=1+b1
r1
abs(r0-r1)

#Se halla la regresión con el r1 obtenido, para encontrar b2 y así r2.
x1<-x(d,r1,m)
reg1<-lm(y~x1)
b2<-coef(reg1)[2]
b2
r2=1+b2
r2


x2<-x(d,r2,m)
reg2<-lm(y~x2)
b3<-coef(reg2)[2]
b3
r3=b3+1
r3
abs(r2-r3)

x3<-x(d,r3,m)

reg3<-lm(y~x3)
b4<-coef(reg3)[2]
b4
r4=1+b4
r4
abs(r4-r3)

x4<-x(d,r4,m)
reg4<-lm(y~x4)
b5<-coef(reg4)[2]
b5
r5=1+b5
r5
abs(r4-r5)

x5<-x(d,r5,m)

reg5<-lm(y~x5)
b6<-coef(reg5)[2]
b6
r6=1+b6
r6
abs(r6-r5)

x6<-x(d,r6,m)

reg6<-lm(y~x6)
b7<-coef(reg6)[2]
b7
r7=1+b7
r7
abs(r6-r7)

x7<-x(d,r7,m)
reg7<-lm(y~x7)
b8<-coef(reg7)[2]
b8
r8<-1+b8
r8
abs(r7-r8)

######### Dandole valores a r0 hasta que r0-r1<0.001

r0=1.062
r0
m=3.5
d<-c(0:27)
dd

y<-log(1-dd)
x<-function(d,r0,m){
  log(d+r0*m)
}
x_<-x(d,r0,m)
#Se realiza la regresión para obtener b1 y así obtener r1
reg<-lm(y~x_)
b1<-coef(reg)[2]
b1
r1=1+b1
r1
abs(r0-r1)



#while(abs(r0-r1)<0.00001){
 # print(r1)
  #r0=r0+0.1
#}
