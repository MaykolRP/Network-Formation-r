rm(list=ls())

#######Leer datos de base###########
#install.packages("foreign")
#install.packages("highfrequency")
#install.packages("xts")
#install.packages("plyr")
#install.packages("ggplot2")

library(foreign)
#library(highfrequency)
library(xts)
library(plyr)
library(ggplot2)

#####################################
######Lectura de datos###############
#####################################

ws=read.csv("indxw.csv",sep=";") #Acciones se debe quitar CLH que no tiene datos

#pdf("Windx.pdf")
par(mar=c(6,5,1,1))
boxplot(ws, ylab="Weights of the index(%)",las=2,cex.axis=0.7)
#dev.off()
#meanW=colMeans(ws,na.rm=TRUE)
#vrna<-function(x){var(x,na.rm=TRUE)}
#varW=apply(ws,MARGIN = 2,vrna)



