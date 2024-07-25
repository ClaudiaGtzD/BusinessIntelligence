rm(list=ls())

#####################################################################
#PASO 1. SE INSTALAN LAS LIBRERIAS NECESARIAS.

library(openxlsx, quietly = TRUE)
library(Hmisc, quietly = TRUE)
library(scales)
library(officer)
library(flextable)
library(forecast)
library(datasets)
library(tseries)
library(WriteXLS)
library(stringr)

#####################################################################
#PASO 2. SE DEFINEN LAS RUTAS DE CATALOGOS Y RESULTADOS.

ruta.1 <- "C:/Users/riemm/OneDrive/Escritorio/Business Intelligence/"

################################################################################
################################################################################
################################################################################
###############################IPC##############################################
################################################################################
################################################################################
################################################################################

#####################################################################
#PASO 3. LEE INSUMOS PRONOSTICO IPC


Competitor1 <- read.csv("C:/Users/riemm/OneDrive/Hazel and Big Data/Business Intelligence/Competidor1.csv",header=TRUE, sep=",")

ts.Competidor1 <- ts(Competitor1$Product1, frequency = 52, start = c(2019,1))
plot(ts.Competidor1)

ARIMA.Competidor1 <- auto.arima(ts.Competidor1,D=1)
summary(ARIMA.Competidor1)
plot(ARIMA.Competidor1$x,col="blue")
par(new=TRUE)
plot(ARIMA.Competidor1$fitted,col="red") 
title(main="Real vs Forecast")
Forecast.Competidor1<- forecast(ARIMA.Competidor1,25,level = 99)
Forecast.Competidor1
plot(Forecast.Competidor1,main="Competidor1 Forecasting",
     ylab="Competidor1 Volume")





