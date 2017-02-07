#Generación de datos para red "Ham Rdio" 
#Utilizando la iteración que Jackson & Rogers plantean en el artículo estudiado
#El cual busca estimar r, que representa si la red es formada de manera aleatoria
#O es dada por potential attachment.
#d representa el vector degree distribution de dicha red. 
d<-c(0:27)
#Guess Inicial
r0=0.1
#m se define como el promedio del grado de la red
m=3.5
#Se define la función de acumulación del modelo, la cual es econtrada
#mediante mean-field theory
fda_<-function(d,r0,m){
  1-(d+r0*m)^(-1-r0)
}
#Se define la regresión para hallar el beta estimado y así construir el r=beta+1
regr<-function(d,r0,m){
  yi<-log(1-fda_(d,r0,m))
  xi<-x(d,r0,m)
  regi<-lm(yi~xi)
  return(coef(regi)[2])
  
} 
#b_i es el coeficiente de intercepto de la regresión
b_i<-regr(d,r0,m)
b_i
#r_i, entonces es el resultado de regresar y sobre x con el guess inicial r0.
r_i<-b_i+1
r_i
abs(r0-r_i)
#Ahora se quiere iterar el proceso anterior, hasta que no se presenten cambios en
#el r.
while(abs(r0-r_i)>0.001){
  print(b_i)
  print(r_i)
  print(abs(r0-r_i))
  r0=r_i
  b_i<-regr(d,r0,m)
  r_i=b_i+1
}



  
