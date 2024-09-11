setwd("C:/Users/Rodrigo/Documents/MAESTRÍA/Tesis/Data/BVC_iColcap/")
#library(RODBC)
#conexion<-odbcConnectExcel("Precios_Series_2008_70.xlsx")
#Datos <-sqlQuery(channel=conexion,"select * from [Hoja1$]")
#close(conexion)

library(TSA) ### PRUEBA DE ESTACIONARIEDAD SERIES PRECIOS Y DIFERENCIADA
library(fBasics)
library(forecast) ### 2. ETAPA DE AJUSTE (ESPECIFICACIÓN) DE LOS MODELOS DE MEDIA CONDICIONAL
library(car) # Test de normalidad Shapiro-Wilk (stats)
library(nortest) # Test de normalidad Anderson-Darling (nortest)
library(lawstat)
library(FinTS)
library(tseries)
library(fGarch)
library(rugarch)

# Datos<-read.table("Precios_Series_2008_70.txt",header=T,dec=",")
Datos<-read.table("Precios_Series_2008_2015.txt",header=T,dec=",")
names(Datos)
dim(Datos)

### SERIES DE PRECIOS Y SELECCIÓN DE ACCION
#PFBCOLOM<-Datos[,2];PFBCOLOM
#PFBCOLOM<-ts(Datos[,2],start=c(2008,1),end=c(2015,6),frequency=252);PFBCOLOM
# PFBCOLOM<-ts(Datos[,2],start=1,end=1827,frequency=1)
PFBCOLOM<-ts(Datos[,2],start=1,end=1950,frequency=1)
ECOPETROL<-ts(Datos[,3],start=1,end=1950,frequency=1)
ISA<-ts(Datos[,4],start=1,end=1950,frequency=1)
ISAGEN<-ts(Datos[,5],start=1,end=1950,frequency=1)
EXITO<-ts(Datos[,6],start=1,end=1950,frequency=1)
BOGOTA<-ts(Datos[,7],start=1,end=1950,frequency=1)
BCOLOMBIA<-ts(Datos[,8],start=1,end=1950,frequency=1)
BVC<-ts(Datos[,9],start=1,end=1950,frequency=1)
CORFICOLCF<-ts(Datos[,10],start=1,end=1950,frequency=1)
GRUPOSURA<-ts(Datos[,11],start=1,end=1950,frequency=1)
NUTRESA<-ts(Datos[,12],start=1,end=1950,frequency=1)
GRUPOARGOS<-ts(Datos[,13],start=1,end=1950,frequency=1)
CELSIA<-ts(Datos[,14],start=1,end=1950,frequency=1)

windows()
plot(PFBCOLOM,col="blue",ylab='Precio', xlab='Días', main='Gráfico 1. Acción BANCOLOMBIA' ,type="l",las=1)
grid()

###RETORNOS LOGARÍTMICOS
#retornos<-diff(log(serie));retornos
retornos_PFBCOLOM<-diff(log(PFBCOLOM))
retornos_ECOPETROL<-diff(log(ECOPETROL))
retornos_ISA<-diff(log(ISA))
retornos_ISAGEN<-diff(log(ISAGEN))
retornos_EXITO<-diff(log(EXITO))
retornos_BOGOTA<-diff(log(BOGOTA))
retornos_BCOLOMBIA<-diff(log(BCOLOMBIA))
retornos_BVC<-diff(log(BVC))
retornos_CORFICOLCF<-diff(log(CORFICOLCF))
retornos_GRUPOSURA<-diff(log(GRUPOSURA))
retornos_NUTRESA<-diff(log(NUTRESA))
retornos_GRUPOARGOS<-diff(log(GRUPOARGOS))
retornos_CELSIA<-diff(log(CELSIA))

#=====================================================================================================================================================
# windows()
# plot(retornos,col="blue",ylab='Log Returns', xlab='Días', main='Gráfico 2. Retornos BANCOLOMBIA' ,type="l",las=1)
# media<-mean(retornos);media
# abline(h=c(media))
# grid()

windows()
par(mfrow=c(4,4))
plot(retornos_PFBCOLOM,col="blue",ylab='Log Returns', xlab='Días', main='PFBCOLOM' ,type="l",las=1)
media1<-mean(retornos_PFBCOLOM)
abline(h=c(media1))

plot(retornos_ECOPETROL,col="blue",ylab='Log Returns', xlab='Días', main='ECOPETROL' ,type="l",las=1)
media2<-mean(retornos_ECOPETROL)
abline(h=c(media2))

plot(retornos_ISA,col="blue",ylab='Log Returns', xlab='Días', main='ISA' ,type="l",las=1)
media3<-mean(retornos_ISA)
abline(h=c(media3))

plot(retornos_ISAGEN,col="blue",ylab='Log Returns', xlab='Días', main='ISAGEN' ,type="l",las=1)
media4<-mean(retornos_ISAGEN)
abline(h=c(media4))

plot(retornos_EXITO,col="blue",ylab='Log Returns', xlab='Días', main='EXITO' ,type="l",las=1)
media5<-mean(retornos_EXITO)
abline(h=c(media5))

plot(retornos_BOGOTA,col="blue",ylab='Log Returns', xlab='Días', main='BOGOTA' ,type="l",las=1)
media6<-mean(retornos_BOGOTA)
abline(h=c(media6))

plot(retornos_BCOLOMBIA,col="blue",ylab='Log Returns', xlab='Días', main='BCOLOMBIA' ,type="l",las=1)
media7<-mean(retornos_BCOLOMBIA)
abline(h=c(media7))

plot(retornos_BVC,col="blue",ylab='Log Returns', xlab='Días', main='BVC' ,type="l",las=1)
media8<-mean(retornos_BVC)
abline(h=c(media8))

plot(retornos_CORFICOLCF,col="blue",ylab='Log Returns', xlab='Días', main='CORFICOLCF' ,type="l",las=1)
media9<-mean(retornos_CORFICOLCF)
abline(h=c(media9))

plot(retornos_GRUPOSURA,col="blue",ylab='Log Returns', xlab='Días', main='GRUPOSURA' ,type="l",las=1)
media10<-mean(retornos_GRUPOSURA)
abline(h=c(media10))

plot(retornos_NUTRESA,col="blue",ylab='Log Returns', xlab='Días', main='NUTRESA' ,type="l",las=1)
media11<-mean(retornos_NUTRESA)
abline(h=c(media11))

plot(retornos_GRUPOARGOS,col="blue",ylab='Log Returns', xlab='Días', main='GRUPOARGOS' ,type="l",las=1)
media12<-mean(retornos_GRUPOARGOS)
abline(h=c(media12))

plot(retornos_CELSIA,col="blue",ylab='Log Returns', xlab='Días', main='CELSIA' ,type="l",las=1)
media13<-mean(retornos_CELSIA)
abline(h=c(media13))
#=====================================================================================================================================================
### VARIABLE GLOBAL
serie<-PFBCOLOM
retornos<-retornos_PFBCOLOM

### PRUEBA DE ESTACIONARIEDAD SERIES PRECIOS Y DIFERENCIADA
adf.test(serie)
adf.test(retornos)
### SERIE ESTACIONARIA
### SE IDENTIFICAN CLUSTERS DE VOLATILIDAD - INDICIOS DE EFECTOS ARCH           

basicStats(serie)

#=====================================================================================================================================================
#=====================================================================================================================================================

### 1. ETAPA DE IDENTIFICACIÓN DEL COMPONENTE AR Y MA

# AUTOCORRELOGRAMAS SIMPLES (ACF)
windows()
par(mfrow=c(4,4),las=1)
acf(retornos_PFBCOLOM,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_PFBCOLOM")
acf(retornos_ECOPETROL,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_ECOPETROL")
acf(retornos_ISA,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_ISA")
acf(retornos_ISAGEN,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_ISAGEN")
acf(retornos_EXITO,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_EXITO")
acf(retornos_BOGOTA,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_BOGOTA")
acf(retornos_BCOLOMBIA,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_BCOLOMBIA")
acf(retornos_BVC,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_BVC")
acf(retornos_CORFICOLCF,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_CORFICOLCF")
acf(retornos_GRUPOSURA,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_GRUPOSURA")
acf(retornos_NUTRESA,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_NUTRESA")
acf(retornos_GRUPOARGOS,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_GRUPOARGOS")
acf(retornos_CELSIA,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_CELSIA")

#=====================================================================================================================================================
# AUTOCORRELOGRAMAS PARCIALES (PACF)
windows()
par(mfrow=c(4,4),las=1)
pacf(retornos_PFBCOLOM,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_PFBCOLOM")
pacf(retornos_ECOPETROL,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_ECOPETROL")
pacf(retornos_ISA,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_ISA")
pacf(retornos_ISAGEN,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_ISAGEN")
pacf(retornos_EXITO,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_EXITO")
pacf(retornos_BOGOTA,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_BOGOTA")
pacf(retornos_BCOLOMBIA,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_BCOLOMBIA")
pacf(retornos_BVC,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_BVC")
pacf(retornos_CORFICOLCF,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_CORFICOLCF")
pacf(retornos_GRUPOSURA,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_GRUPOSURA")
pacf(retornos_NUTRESA,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_NUTRESA")
pacf(retornos_GRUPOARGOS,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_GRUPOARGOS")
pacf(retornos_CELSIA,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_CELSIA")


### THESE ACF's SUGGEST THAT THERE ARE NO STRONG SERIAL CORRELATIONS IN THE LOG RETURN SERIES

#=====================================================================================================================================================
# FUNCION DE AUTOCORRELACIÓN EXTENDIDA (EACF)
eacf(retornos_PFBCOLOM)
eacf(retornos_ECOPETROL)
eacf(retornos_ISA)
eacf(retornos_ISAGEN)
eacf(retornos_EXITO)
eacf(retornos_BOGOTA)
eacf(retornos_BCOLOMBIA)
eacf(retornos_BVC)
eacf(retornos_CORFICOLCF)
eacf(retornos_GRUPOSURA)
eacf(retornos_NUTRESA)
eacf(retornos_GRUPOARGOS)
eacf(retornos_CELSIA)

### LOS EACF IDENTIFICAN UN ARMA (0,0) POR LO QUE SE CONCLUYE QUE
### NO SE PUEDE AJUSTAR AL CONJUNTO DE DATOS UN PROCESO ARIMA
### APARANTEMENTE LA VARIABLE SIGUE UN PROCESO DE RUIDO, POR LO TANTO
### SE PUEDE SEGUIR TRABAJANDO DIRECTAMENTE SOBRE LA VARIABLE.
### LOS PROCESOS ARCH-GARCH SE AJUSTAN A LOS RESIDUALES, A DIFERENCIA DE
### LOS PROCESOS ARIMA QUE SE AJUSTAN A LA VARIABLE DIRECTAMENTE
#=====================================================================================================================================================
#=====================================================================================================================================================

### 2. ETAPA DE AJUSTE (ESPECIFICACIÓN) DE LOS MODELOS DE MEDIA CONDICIONAL

# modelo1<-arima(retornos_GRUPOARGOS,order=c(0,0,0));modelo1
# bestfit_ARMA<-auto.arima(retornos_PFBCOLOM,d=0,D=0,stationary=TRUE,seasonal=FALSE,ic=c("aicc","aic","bic"),stepwise=FALSE,trace=TRUE,
# test=c("kpss","adf","pp"),seasonal.test=c("ocsb","ch"),allowdrift=TRUE, allowmean=TRUE);bestfit_ARMA

bestfit_ARMA_PFBCOLOM<-auto.arima(retornos_PFBCOLOM,d=0,D=0,stationary=TRUE,seasonal=FALSE,ic=c("aicc","aic","bic"),stepwise=FALSE,trace=TRUE,
test=c("kpss","adf","pp"),seasonal.test=c("ocsb","ch"),allowdrift=TRUE, allowmean=TRUE);bestfit_ARMA_PFBCOLOM

bestfit_ARMA_ECOPETROL<-arima(retornos_ECOPETROL,order=c(0,0,0));bestfit_ARMA_ECOPETROL

bestfit_ARMA_ISA<-auto.arima(retornos_ISA,d=0,D=0,stationary=TRUE,seasonal=FALSE,ic=c("aicc","aic","bic"),stepwise=FALSE,trace=TRUE,
test=c("kpss","adf","pp"),seasonal.test=c("ocsb","ch"),allowdrift=TRUE, allowmean=TRUE);bestfit_ARMA_ISA

bestfit_ARMA_ISAGEN<-auto.arima(retornos_ISAGEN,d=0,D=0,stationary=TRUE,seasonal=FALSE,ic=c("aicc","aic","bic"),stepwise=FALSE,trace=TRUE,
test=c("kpss","adf","pp"),seasonal.test=c("ocsb","ch"),allowdrift=TRUE, allowmean=TRUE);bestfit_ARMA_ISAGEN

bestfit_ARMA_EXITO<-auto.arima(retornos_EXITO,d=0,D=0,stationary=TRUE,seasonal=FALSE,ic=c("aicc","aic","bic"),stepwise=FALSE,trace=TRUE,
test=c("kpss","adf","pp"),seasonal.test=c("ocsb","ch"),allowdrift=TRUE, allowmean=TRUE);bestfit_ARMA_EXITO

bestfit_ARMA_BOGOTA<-arima(retornos_BOGOTA,order=c(0,0,4));bestfit_ARMA_BOGOTA

bestfit_ARMA_BCOLOMBIA<-auto.arima(retornos_BCOLOMBIA,d=0,D=0,stationary=TRUE,seasonal=FALSE,ic=c("aicc","aic","bic"),stepwise=FALSE,trace=TRUE,
test=c("kpss","adf","pp"),seasonal.test=c("ocsb","ch"),allowdrift=TRUE, allowmean=TRUE);bestfit_ARMA_BCOLOMBIA

bestfit_ARMA_BVC<-auto.arima(retornos_BVC,d=0,D=0,stationary=TRUE,seasonal=FALSE,ic=c("aicc","aic","bic"),stepwise=FALSE,trace=TRUE,
test=c("kpss","adf","pp"),seasonal.test=c("ocsb","ch"),allowdrift=TRUE, allowmean=TRUE);bestfit_ARMA_BVC

bestfit_ARMA_CORFICOLCF<-auto.arima(retornos_CORFICOLCF,d=0,D=0,stationary=TRUE,seasonal=FALSE,ic=c("aicc","aic","bic"),stepwise=FALSE,trace=TRUE,
test=c("kpss","adf","pp"),seasonal.test=c("ocsb","ch"),allowdrift=TRUE, allowmean=TRUE);bestfit_ARMA_CORFICOLCF

bestfit_ARMA_GRUPOSURA<-auto.arima(retornos_GRUPOSURA,d=0,D=0,stationary=TRUE,seasonal=FALSE,ic=c("aicc","aic","bic"),stepwise=FALSE,trace=TRUE,
test=c("kpss","adf","pp"),seasonal.test=c("ocsb","ch"),allowdrift=TRUE, allowmean=TRUE);bestfit_ARMA_GRUPOSURA

bestfit_ARMA_NUTRESA<-auto.arima(retornos_NUTRESA,d=0,D=0,stationary=TRUE,seasonal=FALSE,ic=c("aicc","aic","bic"),stepwise=FALSE,trace=TRUE,
test=c("kpss","adf","pp"),seasonal.test=c("ocsb","ch"),allowdrift=TRUE, allowmean=TRUE);bestfit_ARMA_NUTRESA

bestfit_ARMA_GRUPOARGOS<-arima(retornos_GRUPOARGOS,order=c(0,0,0));bestfit_ARMA_GRUPOARGOS

bestfit_ARMA_CELSIA<-auto.arima(retornos_CELSIA,d=0,D=0,stationary=TRUE,seasonal=FALSE,ic=c("aicc","aic","bic"),stepwise=FALSE,trace=TRUE,
test=c("kpss","adf","pp"),seasonal.test=c("ocsb","ch"),allowdrift=TRUE, allowmean=TRUE);bestfit_ARMA_CELSIA

#=====================================================================================================================================================
#=====================================================================================================================================================

### 3. ETAPA DE DIAGNÓSTICO DEL MODELO (ANÁLISIS DE RESIDUALES ESTANDARIZADOS - VALIDACIÓN DE SUPUESTOS)

#modelo_sel<-arima(retornos_PFBCOLOM,order=c(2,0,2));modelo_sel
#names(modelo_sel)
#residuals_modelo_sel<-modelo_sel$residuals;residuals_modelo_sel
#summary(bestfit_ARMA)
#names(modelo1)
#residuals_bestfit_ARMA<-modelo1$residuals;residuals_bestfit_ARMA
names(bestfit_ARMA)
residuals_bestfit_ARMA<-bestfit_ARMA$residuals;residuals_bestfit_ARMA

residuals_bestfit_ARMA_PFBCOLOM<-bestfit_ARMA_PFBCOLOM$residuals
residuals_bestfit_ARMA_ECOPETROL<-bestfit_ARMA_ECOPETROL$residuals
residuals_bestfit_ARMA_ISA<-bestfit_ARMA_ISA$residuals
residuals_bestfit_ARMA_ISAGEN<-bestfit_ARMA_ISAGEN$residuals
residuals_bestfit_ARMA_EXITO<-bestfit_ARMA_EXITO$residuals
residuals_bestfit_ARMA_BOGOTA<-bestfit_ARMA_BOGOTA$residuals
residuals_bestfit_ARMA_BCOLOMBIA<-bestfit_ARMA_BCOLOMBIA$residuals
residuals_bestfit_ARMA_BVC<-bestfit_ARMA_BVC$residuals
residuals_bestfit_ARMA_CORFICOLCF<-bestfit_ARMA_CORFICOLCF$residuals
residuals_bestfit_ARMA_GRUPOSURA<-bestfit_ARMA_GRUPOSURA$residuals
residuals_bestfit_ARMA_NUTRESA<-bestfit_ARMA_NUTRESA$residuals
residuals_bestfit_ARMA_GRUPOARGOS<-bestfit_ARMA_GRUPOARGOS$residuals
residuals_bestfit_ARMA_CELSIA<-bestfit_ARMA_CELSIA$residuals

windows()
plot(residuals_bestfit_ARMA,col="blue",ylab='Gráfico de residuales estandarizados', xlab='Días', main='Gráfico 1. Acción BANCOLOMBIA' ,type="l",las=1)
grid()

### 3.1. SUPUESTO DE NORMALIDAD

windows()
par(mfrow=c(1,2),las=1)
hist(residuals_bestfit_ARMA,main="Histograma Residuales",col="blue",xlab="residuos",ylab="frecuencia")
qqnorm(residuals_bestfit_ARMA,main="Gráfico Q-Q",col="blue",xlab="Cuantiles Teóricos",ylab="Cuantiles Muestrales"); qqline(residuals_bestfit_ARMA, col = 1)

#windows()
#par(mfrow=c(4,4),las=1)
#qqnorm(residuals_bestfit_ARMA_PFBCOLOM,main="PFBCOLOM",col="blue",xlab="Cuantiles Teóricos"); qqline(residuals_bestfit_ARMA_PFBCOLOM, col = 1)
#qqnorm(residuals_bestfit_ARMA_ECOPETROL,main="ECOPETROL",col="blue",xlab="Cuantiles Teóricos"); qqline(residuals_bestfit_ARMA_ECOPETROL, col = 1)
#qqnorm(residuals_bestfit_ARMA_ISA,main="ISA",col="blue",xlab="Cuantiles Teóricos"); qqline(residuals_bestfit_ARMA_ISA, col = 1)
#qqnorm(residuals_bestfit_ARMA_ISAGEN,main="ISAGEN",col="blue",xlab="Cuantiles Teóricos"); qqline(residuals_bestfit_ARMA_ISAGEN, col = 1)
#qqnorm(residuals_bestfit_ARMA_EXITO,main="EXITO",col="blue",xlab="Cuantiles Teóricos"); qqline(residuals_bestfit_ARMA_EXITO, col = 1)
#qqnorm(residuals_bestfit_ARMA_BOGOTA,main="BOGOTA",col="blue",xlab="Cuantiles Teóricos"); qqline(residuals_bestfit_ARMA_BOGOTA, col = 1)
#qqnorm(residuals_bestfit_ARMA_BCOLOMBIA,main="BCOLOMBIA",col="blue",xlab="Cuantiles Teóricos"); qqline(residuals_bestfit_ARMA_BCOLOMBIA, col = 1)
#qqnorm(residuals_bestfit_ARMA_BVC,main="BVC",col="blue",xlab="Cuantiles Teóricos"); qqline(residuals_bestfit_ARMA_BVC, col = 1)
#qqnorm(residuals_bestfit_ARMA_CORFICOLCF,main="CORFICOLCF",col="blue",xlab="Cuantiles Teóricos"); qqline(residuals_bestfit_ARMA_CORFICOLCF, col = 1)
#qqnorm(residuals_bestfit_ARMA_GRUPOSURA,main="GRUPOSURA",col="blue",xlab="Cuantiles Teóricos"); qqline(residuals_bestfit_ARMA_GRUPOSURA, col = 1)
#qqnorm(residuals_bestfit_ARMA_NUTRESA,main="NUTRESA",col="blue",xlab="Cuantiles Teóricos"); qqline(residuals_bestfit_ARMA_NUTRESA, col = 1)
#qqnorm(residuals_bestfit_ARMA_GRUPOARGOS,main="GRUPOARGOS",col="blue",xlab="Cuantiles Teóricos"); qqline(residuals_bestfit_ARMA_GRUPOARGOS, col = 1)
#qqnorm(residuals_bestfit_ARMA_CELSIA,main="CELSIA",col="blue",xlab="Cuantiles Teóricos"); qqline(residuals_bestfit_ARMA_CELSIA, col = 1)

shapiro.test(residuals_bestfit_ARMA) # Test de normalidad Shapiro-Wilk (stats)
ad.test(residuals_bestfit_ARMA) # Test de normalidad Anderson-Darling (nortest)

### 3.2. SUPUESTO DE INDEPENDENCIA

Ljung_Box1<-Box.test(residuals_bestfit_ARMA, lag = 500, type="Ljung");Ljung_Box1

Ljung_Box <- NULL
for(i in 1:500){
  Ljung_Box1 <- Box.test(residuals_bestfit_ARMA, lag = i, type="Ljung")$p.value
  Ljung_Box <- rbind(Ljung_Box, cbind(i,Ljung_Box1))
};Ljung_Box

#Ljung_Box_residuals_PFBCOLOM <- NULL
#for(i in 1:500){
#Ljung_Box1 <- Box.test(residuals_bestfit_ARMA_PFBCOLOM, lag = i, type="Ljung")$p.value
#Ljung_Box_residuals_PFBCOLOM <- rbind(Ljung_Box_residuals_PFBCOLOM, cbind(i,Ljung_Box1))
#}

#Ljung_Box_residuals_ECOPETROL <- NULL
#for(i in 1:500){
  #Ljung_Box1 <- Box.test(residuals_bestfit_ARMA_ECOPETROL, lag = i, type="Ljung")$p.value
  #Ljung_Box_residuals_ECOPETROL <- rbind(Ljung_Box_residuals_ECOPETROL, cbind(i,Ljung_Box1))
#}

#Ljung_Box_residuals_ISA <- NULL
#for(i in 1:500){
  #Ljung_Box1 <- Box.test(residuals_bestfit_ARMA_ISA, lag = i, type="Ljung")$p.value
  #Ljung_Box_residuals_ISA <- rbind(Ljung_Box_residuals_ISA, cbind(i,Ljung_Box1))
#}

#Ljung_Box_residuals_ISAGEN <- NULL
#for(i in 1:500){
  #Ljung_Box1 <- Box.test(residuals_bestfit_ARMA_ISAGEN, lag = i, type="Ljung")$p.value
  #Ljung_Box_residuals_ISAGEN <- rbind(Ljung_Box_residuals_ISAGEN, cbind(i,Ljung_Box1))
#}

#Ljung_Box_residuals_EXITO <- NULL
#for(i in 1:500){
  #Ljung_Box1 <- Box.test(residuals_bestfit_ARMA_EXITO, lag = i, type="Ljung")$p.value
  #Ljung_Box_residuals_EXITO <- rbind(Ljung_Box_residuals_EXITO, cbind(i,Ljung_Box1))
#}

#Ljung_Box_residuals_BOGOTA <- NULL
#for(i in 1:500){
  #Ljung_Box1 <- Box.test(residuals_bestfit_ARMA_BOGOTA, lag = i, type="Ljung")$p.value
  #Ljung_Box_residuals_BOGOTA <- rbind(Ljung_Box_residuals_BOGOTA, cbind(i,Ljung_Box1))
#}

#Ljung_Box_residuals_BCOLOMBIA <- NULL
#for(i in 1:500){
  #Ljung_Box1 <- Box.test(residuals_bestfit_ARMA_BCOLOMBIA, lag = i, type="Ljung")$p.value
  #Ljung_Box_residuals_BCOLOMBIA <- rbind(Ljung_Box_residuals_BCOLOMBIA, cbind(i,Ljung_Box1))
#}

#Ljung_Box_residuals_BVC <- NULL
#for(i in 1:500){
  #Ljung_Box1 <- Box.test(residuals_bestfit_ARMA_BVC, lag = i, type="Ljung")$p.value
  #Ljung_Box_residuals_BVC <- rbind(Ljung_Box_residuals_BVC, cbind(i,Ljung_Box1))
#}

#Ljung_Box_residuals_CORFICOLCF <- NULL
#for(i in 1:500){
  #Ljung_Box1 <- Box.test(residuals_bestfit_ARMA_CORFICOLCF, lag = i, type="Ljung")$p.value
  #Ljung_Box_residuals_CORFICOLCF <- rbind(Ljung_Box_residuals_CORFICOLCF, cbind(i,Ljung_Box1))
#}

#Ljung_Box_residuals_GRUPOSURA <- NULL
#for(i in 1:500){
  #Ljung_Box1 <- Box.test(residuals_bestfit_ARMA_GRUPOSURA, lag = i, type="Ljung")$p.value
  #Ljung_Box_residuals_GRUPOSURA <- rbind(Ljung_Box_residuals_GRUPOSURA, cbind(i,Ljung_Box1))
#}

#Ljung_Box_residuals_NUTRESA <- NULL
#for(i in 1:500){
  #Ljung_Box1 <- Box.test(residuals_bestfit_ARMA_NUTRESA, lag = i, type="Ljung")$p.value
  #Ljung_Box_residuals_NUTRESA <- rbind(Ljung_Box_residuals_NUTRESA, cbind(i,Ljung_Box1))
#}

#Ljung_Box_residuals_GRUPOARGOS<- NULL
#for(i in 1:500){
  #Ljung_Box1 <- Box.test(residuals_bestfit_ARMA_GRUPOARGOS, lag = i, type="Ljung")$p.value
  #Ljung_Box_residuals_GRUPOARGOS <- rbind(Ljung_Box_residuals_GRUPOARGOS, cbind(i,Ljung_Box1))
#}

#Ljung_Box_residuals_CELSIA <- NULL
#for(i in 1:500){
  #Ljung_Box1 <- Box.test(residuals_bestfit_ARMA_CELSIA, lag = i, type="Ljung")$p.value
  #Ljung_Box_residuals_CELSIA <- rbind(Ljung_Box_residuals_CELSIA, cbind(i,Ljung_Box1))
#}

windows()
par(mfrow=c(1,1))
colnames(Ljung_Box) <- c("Rezago","p-valor");
plot(Ljung_Box,ylim=c(0,1), main="Prueba Ljung & Box \n H0: Independencia")
abline(h=0.05,col="red")

#windows()
#par(mfrow=c(4,4))

#colnames(Ljung_Box_residuals_PFBCOLOM) <- c("Rezago","p-valor");
#plot(Ljung_Box_residuals_PFBCOLOM,ylim=c(0,1), main="PFBCOLOM")
#abline(h=0.05,col="red")

#colnames(Ljung_Box_residuals_ECOPETROL) <- c("Rezago","p-valor");
#plot(Ljung_Box_residuals_ECOPETROL,ylim=c(0,1), main="ECOPETROL")
#abline(h=0.05,col="red")

#colnames(Ljung_Box_residuals_ISA) <- c("Rezago","p-valor");
#plot(Ljung_Box_residuals_ISA,ylim=c(0,1), main="ISA")
#abline(h=0.05,col="red")

#colnames(Ljung_Box_residuals_ISAGEN) <- c("Rezago","p-valor");
#plot(Ljung_Box_residuals_ISAGEN,ylim=c(0,1), main="ISAGEN")
#abline(h=0.05,col="red")

#colnames(Ljung_Box_residuals_EXITO) <- c("Rezago","p-valor");
#plot(Ljung_Box_residuals_EXITO,ylim=c(0,1), main="EXITO")
#abline(h=0.05,col="red")

#colnames(Ljung_Box_residuals_BOGOTA) <- c("Rezago","p-valor");
#plot(Ljung_Box_residuals_BOGOTA,ylim=c(0,1), main="BOGOTA")
#abline(h=0.05,col="red")

#colnames(Ljung_Box_residuals_BCOLOMBIA) <- c("Rezago","p-valor");
#plot(Ljung_Box_residuals_BCOLOMBIA,ylim=c(0,1), main="BCOLOMBIA")
#abline(h=0.05,col="red")

#colnames(Ljung_Box_residuals_BVC) <- c("Rezago","p-valor");
#plot(Ljung_Box_residuals_BVC,ylim=c(0,1), main="BVC")
#abline(h=0.05,col="red")

#colnames(Ljung_Box_residuals_CORFICOLCF) <- c("Rezago","p-valor");
#plot(Ljung_Box_residuals_CORFICOLCF,ylim=c(0,1), main="CORFICOLCF")
#abline(h=0.05,col="red")

#colnames(Ljung_Box_residuals_GRUPOSURA) <- c("Rezago","p-valor");
#plot(Ljung_Box_residuals_GRUPOSURA,ylim=c(0,1), main="GRUPOSURA")
#abline(h=0.05,col="red")

#colnames(Ljung_Box_residuals_NUTRESA) <- c("Rezago","p-valor");
#plot(Ljung_Box_residuals_NUTRESA,ylim=c(0,1), main="NUTRESA")
#abline(h=0.05,col="red")

#colnames(Ljung_Box_residuals_GRUPOARGOS) <- c("Rezago","p-valor");
#plot(Ljung_Box_residuals_GRUPOARGOS,ylim=c(0,1), main="GRUPOARGOS")
#abline(h=0.05,col="red")

#colnames(Ljung_Box_residuals_CELSIA) <- c("Rezago","p-valor");
#plot(Ljung_Box_residuals_CELSIA,ylim=c(0,1), main="CELSIA")
#abline(h=0.05,col="red")

### SI SE CUMPLE EL SUPUESTO DE INDEPENDENCIA, SIGNIFICA QUE EL COMPONENTE
### ARIMA ESTÁ BIEN ESPECIFICADO.
### SIN EMBARGO, ESTA TAMBIÉN ES LA PRUEBA DEFINITIVA DE QUE LA SERIE 
### DE RETORNOS SIGUE UN PROCESO DE RUIDO, PORQUE TODOS LOS REZAGOS DE
### LA VARIABLE NO ESTÁN AUTOCORRELACIONADOS, ES DECIR, SON INDEPENDIENTES
### E iid

### 3.3. SUPUESTO DE HOMOCEDASTICIDAD

#=====================================================================================================================================================
# AUTOCORRELOGRAMAS SIMPLES RETORNOS ABSOLUTOS (ACF)
windows()
par(mfrow=c(4,4),las=1)
acf(abs(retornos_PFBCOLOM),lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_PFBCOLOM")
acf(abs(retornos_ECOPETROL),lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_ECOPETROL")
acf(abs(retornos_ISA),lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_ISA")
acf(abs(retornos_ISAGEN),lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_ISAGEN")
acf(abs(retornos_EXITO),lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_EXITO")
acf(abs(retornos_BOGOTA),lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_BOGOTA")
acf(abs(retornos_BCOLOMBIA),lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_BCOLOMBIA")
acf(abs(retornos_BVC),lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_BVC")
acf(abs(retornos_CORFICOLCF),lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_CORFICOLCF")
acf(abs(retornos_GRUPOSURA),lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_GRUPOSURA")
acf(abs(retornos_NUTRESA),lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_NUTRESA")
acf(abs(retornos_GRUPOARGOS),lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_GRUPOARGOS")
acf(abs(retornos_CELSIA),lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_CELSIA")
#=====================================================================================================================================================
# AUTOCORRELOGRAMAS PARCIALES RETORNOS ABSOLUTOS (PACF)
windows()
par(mfrow=c(4,4),las=1)
pacf(abs(retornos_PFBCOLOM),lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_PFBCOLOM")
pacf(abs(retornos_ECOPETROL),lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_ECOPETROL")
pacf(abs(retornos_ISA),lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_ISA")
pacf(abs(retornos_ISAGEN),lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_ISAGEN")
pacf(abs(retornos_EXITO),lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_EXITO")
pacf(abs(retornos_BOGOTA),lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_BOGOTA")
pacf(abs(retornos_BCOLOMBIA),lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_BCOLOMBIA")
pacf(abs(retornos_BVC),lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_BVC")
pacf(abs(retornos_CORFICOLCF),lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_CORFICOLCF")
pacf(abs(retornos_GRUPOSURA),lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_GRUPOSURA")
pacf(abs(retornos_NUTRESA),lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_NUTRESA")
pacf(abs(retornos_GRUPOARGOS),lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_GRUPOARGOS")
pacf(abs(retornos_CELSIA),lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_CELSIA")
#=====================================================================================================================================================
# AUTOCORRELOGRAMAS SIMPLES RETORNOS AL CUADRADO (ACF)
windows()
par(mfrow=c(4,4),las=1)
acf(retornos_PFBCOLOM^2,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_PFBCOLOM")
acf(retornos_ECOPETROL^2,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_ECOPETROL")
acf(retornos_ISA^2,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_ISA")
acf(retornos_ISAGEN^2,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_ISAGEN")
acf(retornos_EXITO^2,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_EXITO")
acf(retornos_BOGOTA^2,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_BOGOTA")
acf(retornos_BCOLOMBIA^2,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_BCOLOMBIA")
acf(retornos_BVC^2,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_BVC")
acf(retornos_CORFICOLCF^2,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_CORFICOLCF")
acf(retornos_GRUPOSURA^2,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_GRUPOSURA")
acf(retornos_NUTRESA^2,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_NUTRESA")
acf(retornos_GRUPOARGOS^2,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_GRUPOARGOS")
acf(retornos_CELSIA^2,lag=100,ylab="ACF", xlab="Rezago (días)",main="ACF_CELSIA")
#=====================================================================================================================================================
# AUTOCORRELOGRAMAS PARCIALES RETORNOS AL CUADRADO (PACF)
windows()
par(mfrow=c(4,4),las=1)
pacf(retornos_PFBCOLOM^2,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_PFBCOLOM")
pacf(retornos_ECOPETROL^2,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_ECOPETROL")
pacf(retornos_ISA^2,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_ISA")
pacf(retornos_ISAGEN^2,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_ISAGEN")
pacf(retornos_EXITO^2,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_EXITO")
pacf(retornos_BOGOTA^2,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_BOGOTA")
pacf(retornos_BCOLOMBIA^2,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_BCOLOMBIA")
pacf(retornos_BVC^2,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_BVC")
pacf(retornos_CORFICOLCF^2,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_CORFICOLCF")
pacf(retornos_GRUPOSURA^2,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_GRUPOSURA")
pacf(retornos_NUTRESA^2,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_NUTRESA")
pacf(retornos_GRUPOARGOS^2,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_GRUPOARGOS")
pacf(retornos_CELSIA^2,lag=100,ylab="PACF", xlab="Rezago (días)",main="PACF_CELSIA")

### THESE ACF's SUGGEST THAT THERE ARE STRONG SERIAL CORRELATIONS IN BOTH
### ABSOLUT AND LOG SQUARED RETURN SERIES
#=====================================================================================================================================================

### SQUARED INNOVATIONS SERIES ARE USED TO CHECK FOR CONDITIONAL
### HETEROSCEDASTICITY, WHICH IS ALSO KNOWN AS THE ARCH EFFECTS
squared_residuals_PFBCOLOM<-residuals_bestfit_ARMA_PFBCOLOM^2
squared_residuals_ECOPETROL<-residuals_bestfit_ARMA_ECOPETROL^2
squared_residuals_ISA<-residuals_bestfit_ARMA_ISA^2
squared_residuals_ISAGEN<-residuals_bestfit_ARMA_ISAGEN^2
squared_residuals_EXITO<-residuals_bestfit_ARMA_EXITO^2
squared_residuals_BOGOTA<-residuals_bestfit_ARMA_BOGOTA^2
squared_residuals_BCOLOMBIA<-residuals_bestfit_ARMA_BCOLOMBIA^2
squared_residuals_BVC<-residuals_bestfit_ARMA_BVC^2
squared_residuals_CORFICOLCF<-residuals_bestfit_ARMA_CORFICOLCF^2
squared_residuals_GRUPOSURA<-residuals_bestfit_ARMA_GRUPOSURA^2
squared_residuals_NUTRESA<-residuals_bestfit_ARMA_NUTRESA^2
squared_residuals_GRUPOARGOS<-residuals_bestfit_ARMA_GRUPOARGOS^2
squared_residuals_CELSIA<-residuals_bestfit_ARMA_CELSIA^2
###resid2<-retornos^2

### PRUEBA DE MULTIPLICADORES DE LAGRANGE
#Mult_Lagrange<-ArchTest(resid2,lag=100);Mult_Lagrange
MLagrange_squared_residuals_PFBCOLOM<-ArchTest(squared_residuals_PFBCOLOM,lag=100);MLagrange_squared_residuals_PFBCOLOM
MLagrange_squared_residuals_ECOPETROL<-ArchTest(squared_residuals_ECOPETROL,lag=100);MLagrange_squared_residuals_ECOPETROL
MLagrange_squared_residuals_ISA<-ArchTest(squared_residuals_ISA,lag=100);MLagrange_squared_residuals_ISA
MLagrange_squared_residuals_ISAGEN<-ArchTest(squared_residuals_ISAGEN,lag=100);MLagrange_squared_residuals_ISAGEN
MLagrange_squared_residuals_EXITO<-ArchTest(squared_residuals_EXITO,lag=100);MLagrange_squared_residuals_EXITO
MLagrange_squared_residuals_BOGOTA<-ArchTest(squared_residuals_BOGOTA,lag=100);MLagrange_squared_residuals_BOGOTA
MLagrange_squared_residuals_BCOLOMBIA<-ArchTest(squared_residuals_BCOLOMBIA,lag=100);MLagrange_squared_residuals_BCOLOMBIA
MLagrange_squared_residuals_BVC<-ArchTest(squared_residuals_BVC,lag=100);MLagrange_squared_residuals_BVC
MLagrange_squared_residuals_CORFICOLCF<-ArchTest(squared_residuals_CORFICOLCF,lag=100);MLagrange_squared_residuals_CORFICOLCF
MLagrange_squared_residuals_GRUPOSURA<-ArchTest(squared_residuals_GRUPOSURA,lag=100);MLagrange_squared_residuals_GRUPOSURA
MLagrange_squared_residuals_NUTRESA<-ArchTest(squared_residuals_NUTRESA,lag=100);MLagrange_squared_residuals_NUTRESA
MLagrange_squared_residuals_GRUPOARGOS<-ArchTest(squared_residuals_GRUPOARGOS,lag=100);MLagrange_squared_residuals_GRUPOARGOS
MLagrange_squared_residuals_CELSIA<-ArchTest(squared_residuals_CELSIA,lag=100);MLagrange_squared_residuals_CELSIA
###SI HAY EFECTOS ARCH EN LA SERIE

### PRUEBA DE MCLEOD-LI (LJUNG-BOX)
windows()
par(mfrow=c(4,4),las=1)
McLeod_Li_PFBCOLOM<-McLeod.Li.test(y=residuals_bestfit_ARMA_PFBCOLOM,gof.lag=100,main="PFBCOLOM")
McLeod_Li_ECOPETROL<-McLeod.Li.test(y=residuals_bestfit_ARMA_ECOPETROL,gof.lag=100,main="ECOPETROL")
McLeod_Li_ISA<-McLeod.Li.test(y=residuals_bestfit_ARMA_ISA,gof.lag=100,main="ISA")
McLeod_Li_ISAGEN<-McLeod.Li.test(y=residuals_bestfit_ARMA_ISAGEN,gof.lag=100,main="ISAGEN")
McLeod_Li_EXITO<-McLeod.Li.test(y=residuals_bestfit_ARMA_EXITO,gof.lag=100,main="EXITO")
McLeod_Li_BOGOTA<-McLeod.Li.test(y=residuals_bestfit_ARMA_BOGOTA,gof.lag=100,main="BOGOTA")
McLeod_Li_BCOLOMBIA<-McLeod.Li.test(y=residuals_bestfit_ARMA_BCOLOMBIA,gof.lag=100,main="BCOLOMBIA")
McLeod_Li_BVC<-McLeod.Li.test(y=residuals_bestfit_ARMA_BVC,gof.lag=100,main="BVC")
McLeod_Li_CORFICOLCF<-McLeod.Li.test(y=residuals_bestfit_ARMA_CORFICOLCF,gof.lag=100,main="CORFICOLCF")
McLeod_Li_GRUPOSURA<-McLeod.Li.test(y=residuals_bestfit_ARMA_GRUPOSURA,gof.lag=100,main="GRUPOSURA")
McLeod_Li_NUTRESA<-McLeod.Li.test(y=residuals_bestfit_ARMA_NUTRESA,gof.lag=100,main="NUTRESA")
McLeod_Li_GRUPOARGOS<-McLeod.Li.test(y=residuals_bestfit_ARMA_GRUPOARGOS,gof.lag=100,main="GRUPOARGOS")
McLeod_Li_CELSIA<-McLeod.Li.test(y=residuals_bestfit_ARMA_CELSIA,gof.lag=100,main="CELSIA")

# mc<-McLeod.Li.test(y=residuals_bestfit_ARMA,lag=100,main="Prueba McLeod-Li \n H0: Homocedasticidad")
# mc<-McLeod.Li.test(y=retornos,lag=2,main="Prueba McLeod-Li \n H0: Homocedasticidad")
# grid()
# abline(h=0.05,col="red")
#=====================================================================================================================================================
#=====================================================================================================================================================

### 4. MODELACIÓN COMPONENTE ARCH-GARCH

### ORDEN MODELO ARCH
windows()
par(mfrow=c(1,2),las=1)
acf(resid2,l=100,ylab='ACF', xlab='Rezago (días)',main='Autocorrelograma Simple Residuales al cuadrado')
pacf(resid2,l=100,ylab='PACF', xlab='Rezago (días)',main='Autocorrelograma Parcial Residuales al cuadrado')
### THERE ARE SIGNIFICANT PACF's AT HIGHER ORDER LAGS, INDICATING THAT
### A HIGH ORDER ARCH MODEL IS NEEDED FOR THE SERIES. IN THIS SITUATION,
### ONE WOULD EMPLOY THE MORE PARSIMONIOUS GARCH MODEL INSTEAD OF A PURE
### ARCH MODEL.

resid2_abs<-abs(resid2)
### EL EACF DEL VALOR ABSOLUTO DE LOS RESIDUALES AL CUADRADO DEL MODELO 
### ARIMA PERMITE IDENTIFICAR EL ORDEN GARCH (S) Y EL ORDEN ARCH(M) DEL
### MODELO
eacf(resid2_abs)

# library(devtools)
# install_bitbucket("rugarch","alexiosg")
# install_bitbucket("alexiosg/rugarch")

spec_ARMA_GARCH1<-ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(2,1),submodel=NULL,
external.regressors=NULL,variance.targeting=FALSE),mean.model=list(armaOrder=c(2,2),include.mean=TRUE,archm=FALSE,
archpow=1,arfima=FALSE,external.regressors=NULL,archex=FALSE),distribution.model="norm")

spec_ARMA_GARCH2<-ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(2,1),submodel=NULL,
external.regressors=NULL,variance.targeting=FALSE),mean.model=list(armaOrder=c(0,0),include.mean=FALSE,archm=FALSE,
archpow=1,arfima=FALSE,external.regressors=NULL,archex=FALSE),distribution.model="norm")

spec_ARMA_GARCH3<-ugarchspec(variance.model=list(model="apARCH",garchOrder=c(1,1),submodel=NULL,
external.regressors=NULL,variance.targeting=FALSE),mean.model=list(armaOrder=c(0,4),include.mean=FALSE,archm=FALSE,
archpow=1,arfima=FALSE,external.regressors=NULL,archex=FALSE),distribution.model="norm")

spec_ARMA_GARCH4<-ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,2),submodel=NULL,
external.regressors=NULL,variance.targeting=FALSE),mean.model=list(armaOrder=c(1,1),include.mean=FALSE,archm=FALSE,
archpow=1,arfima=FALSE,external.regressors=NULL,archex=FALSE),distribution.model="norm")

spec_ARMA_GARCH5<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1),submodel=NULL,
external.regressors=NULL,variance.targeting=FALSE),mean.model=list(armaOrder=c(0,2),include.mean=FALSE,archm=FALSE,
archpow=1,arfima=FALSE,external.regressors=NULL,archex=FALSE),distribution.model="norm")

spec_ARMA_GARCH6<-ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1),submodel=NULL,
external.regressors=NULL,variance.targeting=FALSE),mean.model=list(armaOrder=c(0,4),include.mean=FALSE,archm=FALSE,
archpow=1,arfima=FALSE,external.regressors=NULL,archex=FALSE),distribution.model="norm")

spec_ARMA_GARCH7<-ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(2,1),submodel=NULL,
external.regressors=NULL,variance.targeting=FALSE),mean.model=list(armaOrder=c(0,2),include.mean=FALSE,archm=FALSE,
archpow=1,arfima=FALSE,external.regressors=NULL,archex=FALSE),distribution.model="norm")

spec_ARMA_GARCH8<-ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1),submodel=NULL,
external.regressors=NULL,variance.targeting=FALSE),mean.model=list(armaOrder=c(3,2),include.mean=FALSE,archm=FALSE,
archpow=1,arfima=FALSE,external.regressors=NULL,archex=FALSE),distribution.model="norm")

spec_ARMA_GARCH9<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1),submodel=NULL,
external.regressors=NULL,variance.targeting=FALSE),mean.model=list(armaOrder=c(3,2),include.mean=TRUE,archm=FALSE,
archpow=1,arfima=FALSE,external.regressors=NULL,archex=FALSE),distribution.model="norm")

spec_ARMA_GARCH10<-ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,2),submodel=NULL,
external.regressors=NULL,variance.targeting=FALSE),mean.model=list(armaOrder=c(2,2),include.mean=FALSE,archm=FALSE,
archpow=1,arfima=FALSE,external.regressors=NULL,archex=FALSE),distribution.model="norm")

spec_ARMA_GARCH11<-ugarchspec(variance.model=list(model="apARCH",garchOrder=c(1,1),submodel=NULL,
external.regressors=NULL,variance.targeting=FALSE),mean.model=list(armaOrder=c(0,0),include.mean=FALSE,archm=FALSE,
archpow=1,arfima=FALSE,external.regressors=NULL,archex=FALSE),distribution.model="norm")

spec_ARMA_GARCH12<-ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1),submodel=NULL,
external.regressors=NULL,variance.targeting=FALSE),mean.model=list(armaOrder=c(0,0),include.mean=FALSE,archm=FALSE,
archpow=1,arfima=FALSE,external.regressors=NULL,archex=FALSE),distribution.model="norm")

spec_ARMA_GARCH13<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(2,2),submodel=NULL,
external.regressors=NULL,variance.targeting=FALSE),mean.model=list(armaOrder=c(2,3),include.mean=FALSE,archm=FALSE,
archpow=1,arfima=FALSE,external.regressors=NULL,archex=FALSE),distribution.model="norm")

# bestfit_ARMA_GARCH_PFBCOLOM<-ugarchfit(data=retornos_PFBCOLOM,spec=spec_ARMA_GARCH_PFBCOLOM,out.sample=0,
# solver="hybrid",fit.control=list(stationarity=1,fixed.se=0,scale=0,rec.init="all"));bestfit_ARMA_GARCH_PFBCOLOM

bestfit_ARMA_GARCH_PFBCOLOM<-ugarchfit(spec=spec_ARMA_GARCH1,data=retornos_PFBCOLOM,out.sample=123);bestfit_ARMA_GARCH_PFBCOLOM
bestfit_ARMA_GARCH_ECOPETROL<-ugarchfit(spec=spec_ARMA_GARCH2,data=retornos_ECOPETROL,out.sample=123);bestfit_ARMA_GARCH_ECOPETROL
bestfit_ARMA_GARCH_ISA<-ugarchfit(spec=spec_ARMA_GARCH3,data=retornos_ISA,out.sample=123);bestfit_ARMA_GARCH_ISA
bestfit_ARMA_GARCH_ISAGEN<-ugarchfit(spec=spec_ARMA_GARCH4,data=retornos_ISAGEN,out.sample=123);bestfit_ARMA_GARCH_ISAGEN
bestfit_ARMA_GARCH_EXITO<-ugarchfit(spec=spec_ARMA_GARCH5,data=retornos_EXITO,out.sample=123);bestfit_ARMA_GARCH_EXITO
bestfit_ARMA_GARCH_BOGOTA<-ugarchfit(spec=spec_ARMA_GARCH6,data=retornos_BOGOTA,out.sample=123);bestfit_ARMA_GARCH_BOGOTA
bestfit_ARMA_GARCH_BCOLOMBIA<-ugarchfit(spec=spec_ARMA_GARCH7,data=retornos_BCOLOMBIA,out.sample=123);bestfit_ARMA_GARCH_BCOLOMBIA
bestfit_ARMA_GARCH_BVC<-ugarchfit(spec=spec_ARMA_GARCH8,data=retornos_BVC,out.sample=123);bestfit_ARMA_GARCH_BVC
bestfit_ARMA_GARCH_CORFICOLCF<-ugarchfit(spec=spec_ARMA_GARCH9,data=retornos_CORFICOLCF,out.sample=123);bestfit_ARMA_GARCH_CORFICOLCF
bestfit_ARMA_GARCH_GRUPOSURA<-ugarchfit(spec=spec_ARMA_GARCH10,data=retornos_GRUPOSURA,out.sample=123);bestfit_ARMA_GARCH_GRUPOSURA
bestfit_ARMA_GARCH_NUTRESA<-ugarchfit(spec=spec_ARMA_GARCH11,data=retornos_NUTRESA,out.sample=123);bestfit_ARMA_GARCH_NUTRESA
bestfit_ARMA_GARCH_GRUPOARGOS<-ugarchfit(spec=spec_ARMA_GARCH12,data=retornos_GRUPOARGOS,out.sample=123);bestfit_ARMA_GARCH_GRUPOARGOS
bestfit_ARMA_GARCH_CELSIA<-ugarchfit(spec=spec_ARMA_GARCH13,data=retornos_CELSIA,out.sample=123);bestfit_ARMA_GARCH_CELSIA

names(bestfit_ARMA_GARCH_PFBCOLOM@fit)
coef(bestfit_ARMA_GARCH_GRUPOSURA)

# QQ-plots are used to check whether the standardized residuals indeed follow the assumed distribution.
# If this is the case, the points predominantly build a straight line in the QQ-plot.

STDresiduals_bestfit_ARMA_GARCH_PFBCOLOM<-residuals(bestfit_ARMA_GARCH_PFBCOLOM,standardize=TRUE)
STDresiduals_bestfit_ARMA_GARCH_ECOPETROL<-residuals(bestfit_ARMA_GARCH_ECOPETROL,standardize=TRUE)
STDresiduals_bestfit_ARMA_GARCH_ISA<-residuals(bestfit_ARMA_GARCH_ISA,standardize=TRUE)
STDresiduals_bestfit_ARMA_GARCH_ISAGEN<-residuals(bestfit_ARMA_GARCH_ISAGEN,standardize=TRUE)
STDresiduals_bestfit_ARMA_GARCH_EXITO<-residuals(bestfit_ARMA_GARCH_EXITO,standardize=TRUE)
STDresiduals_bestfit_ARMA_GARCH_BOGOTA<-residuals(bestfit_ARMA_GARCH_BOGOTA,standardize=TRUE)
STDresiduals_bestfit_ARMA_GARCH_BCOLOMBIA<-residuals(bestfit_ARMA_GARCH_BCOLOMBIA,standardize=TRUE)
STDresiduals_bestfit_ARMA_GARCH_BVC<-residuals(bestfit_ARMA_GARCH_BVC,standardize=TRUE)
STDresiduals_bestfit_ARMA_GARCH_CORFICOLCF<-residuals(bestfit_ARMA_GARCH_CORFICOLCF,standardize=TRUE)
STDresiduals_bestfit_ARMA_GARCH_GRUPOSURA<-residuals(bestfit_ARMA_GARCH_GRUPOSURA,standardize=TRUE)
STDresiduals_bestfit_ARMA_GARCH_NUTRESA<-residuals(bestfit_ARMA_GARCH_NUTRESA,standardize=TRUE)
STDresiduals_bestfit_ARMA_GARCH_GRUPOARGOS<-residuals(bestfit_ARMA_GARCH_GRUPOARGOS,standardize=TRUE)
STDresiduals_bestfit_ARMA_GARCH_CELSIA<-residuals(bestfit_ARMA_GARCH_CELSIA,standardize=TRUE)

### STANDARDIZED RESIDUALS QQ PLOTS
windows()
par(mfrow=c(4,4),las=1)
# plot(bestfit_ARMA_GARCH_PFBCOLOM)
# plot(bestfit_ARMA_GARCH_PFBCOLOM,which=9,main="ghyp - QQ Plot",col="blue",xlab="Cuantiles teóricos",ylab="Cuantiles muestrales")
plot(bestfit_ARMA_GARCH_PFBCOLOM,which=9)
plot(bestfit_ARMA_GARCH_ECOPETROL,which=9)
plot(bestfit_ARMA_GARCH_ISA,which=9)
plot(bestfit_ARMA_GARCH_ISAGEN,which=9)
plot(bestfit_ARMA_GARCH_EXITO,which=9)
plot(bestfit_ARMA_GARCH_BOGOTA,which=9)
plot(bestfit_ARMA_GARCH_BCOLOMBIA,which=9)
plot(bestfit_ARMA_GARCH_BVC,which=9)
plot(bestfit_ARMA_GARCH_CORFICOLCF,which=9)
plot(bestfit_ARMA_GARCH_GRUPOSURA,which=9)
plot(bestfit_ARMA_GARCH_NUTRESA,which=9)
plot(bestfit_ARMA_GARCH_GRUPOARGOS,which=9)
plot(bestfit_ARMA_GARCH_CELSIA,which=9)

### EMPIRICAL STANDARDIZED RESIDUALS
windows()
par(mfrow=c(4,4),las=1)
plot(bestfit_ARMA_GARCH_PFBCOLOM,which=8)
plot(bestfit_ARMA_GARCH_ECOPETROL,which=8)
plot(bestfit_ARMA_GARCH_ISA,which=8)
plot(bestfit_ARMA_GARCH_ISAGEN,which=8)
plot(bestfit_ARMA_GARCH_EXITO,which=8)
plot(bestfit_ARMA_GARCH_BOGOTA,which=8)
plot(bestfit_ARMA_GARCH_BCOLOMBIA,which=8)
plot(bestfit_ARMA_GARCH_BVC,which=8)
plot(bestfit_ARMA_GARCH_CORFICOLCF,which=8)
plot(bestfit_ARMA_GARCH_GRUPOSURA,which=8)
plot(bestfit_ARMA_GARCH_NUTRESA,which=8)
plot(bestfit_ARMA_GARCH_GRUPOARGOS,which=8)
plot(bestfit_ARMA_GARCH_CELSIA,which=8)

# windows()
# qqnorm(STDresiduals_bestfit_ARMA_GARCH_PFBCOLOM,main="PFBCOLOM",col="blue",xlab="Cuantiles Teóricos")
# qqline(STDresiduals_bestfit_ARMA_GARCH_PFBCOLOM, col = 1)

#=====================================================================================================================================================

### DIAGNÓSTICO DEL MODELO ESPECIFICADO
# To check the appropriateness of the fitted time series model, tests are performed on the
# standardized residuals. When the chosen model is correctly specified, then the standardized residuals
# are approximately independent and identically distributed.

# The independently and identically distributed assumption of the innovations can be checked by
# examining the sample ACF of the squared standardized residuals.

Sqr_STDresiduals_bestfit_ARMA_GARCH_PFBCOLOM<-STDresiduals_bestfit_ARMA_GARCH_PFBCOLOM^2
Sqr_STDresiduals_bestfit_ARMA_GARCH_ECOPETROL<-STDresiduals_bestfit_ARMA_GARCH_ECOPETROL^2
Sqr_STDresiduals_bestfit_ARMA_GARCH_ISA<-STDresiduals_bestfit_ARMA_GARCH_ISA^2
Sqr_STDresiduals_bestfit_ARMA_GARCH_ISAGEN<-STDresiduals_bestfit_ARMA_GARCH_ISAGEN^2
Sqr_STDresiduals_bestfit_ARMA_GARCH_EXITO<-STDresiduals_bestfit_ARMA_GARCH_EXITO^2
Sqr_STDresiduals_bestfit_ARMA_GARCH_BOGOTA<-STDresiduals_bestfit_ARMA_GARCH_BOGOTA^2
Sqr_STDresiduals_bestfit_ARMA_GARCH_BCOLOMBIA<-STDresiduals_bestfit_ARMA_GARCH_BCOLOMBIA^2
Sqr_STDresiduals_bestfit_ARMA_GARCH_BVC<-STDresiduals_bestfit_ARMA_GARCH_BVC^2
Sqr_STDresiduals_bestfit_ARMA_GARCH_CORFICOLCF<-STDresiduals_bestfit_ARMA_GARCH_CORFICOLCF^2
Sqr_STDresiduals_bestfit_ARMA_GARCH_GRUPOSURA<-STDresiduals_bestfit_ARMA_GARCH_GRUPOSURA^2
Sqr_STDresiduals_bestfit_ARMA_GARCH_NUTRESA<-STDresiduals_bestfit_ARMA_GARCH_NUTRESA^2
Sqr_STDresiduals_bestfit_ARMA_GARCH_GRUPOARGOS<-STDresiduals_bestfit_ARMA_GARCH_GRUPOARGOS^2
Sqr_STDresiduals_bestfit_ARMA_GARCH_CELSIA<-STDresiduals_bestfit_ARMA_GARCH_CELSIA^2

# ACF of the standardized residuals
windows()
par(mfrow=c(4,4),las=1)
acf(STDresiduals_bestfit_ARMA_GARCH_PFBCOLOM,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_PFBCOLOM")
acf(STDresiduals_bestfit_ARMA_GARCH_ECOPETROL,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_ECOPETROL")
acf(STDresiduals_bestfit_ARMA_GARCH_ISA,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_ISA")
acf(STDresiduals_bestfit_ARMA_GARCH_ISAGEN,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_ISAGEN")
acf(STDresiduals_bestfit_ARMA_GARCH_EXITO,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_EXITO")
acf(STDresiduals_bestfit_ARMA_GARCH_BOGOTA,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_BOGOTA")
acf(STDresiduals_bestfit_ARMA_GARCH_BCOLOMBIA,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_BCOLOMBIA")
acf(STDresiduals_bestfit_ARMA_GARCH_BVC,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_BVC")
acf(STDresiduals_bestfit_ARMA_GARCH_CORFICOLCF,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_CORFICOLCF")
acf(STDresiduals_bestfit_ARMA_GARCH_GRUPOSURA,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_GRUPOSURA")
acf(STDresiduals_bestfit_ARMA_GARCH_NUTRESA,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_NUTRESA")
acf(STDresiduals_bestfit_ARMA_GARCH_GRUPOARGOS,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_GRUPOARGOS")
acf(STDresiduals_bestfit_ARMA_GARCH_CELSIA,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_CELSIA")


# PACF of the standardized residuals
windows()
par(mfrow=c(4,4),las=1)
pacf(STDresiduals_bestfit_ARMA_GARCH_PFBCOLOM,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_PFBCOLOM")
pacf(STDresiduals_bestfit_ARMA_GARCH_ECOPETROL,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_ECOPETROL")
pacf(STDresiduals_bestfit_ARMA_GARCH_ISA,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_ISA")
pacf(STDresiduals_bestfit_ARMA_GARCH_ISAGEN,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_ISAGEN")
pacf(STDresiduals_bestfit_ARMA_GARCH_EXITO,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_EXITO")
pacf(STDresiduals_bestfit_ARMA_GARCH_BOGOTA,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_BOGOTA")
pacf(STDresiduals_bestfit_ARMA_GARCH_BCOLOMBIA,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_BCOLOMBIA")
pacf(STDresiduals_bestfit_ARMA_GARCH_BVC,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_BVC")
pacf(STDresiduals_bestfit_ARMA_GARCH_CORFICOLCF,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_CORFICOLCF")
pacf(STDresiduals_bestfit_ARMA_GARCH_GRUPOSURA,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_GRUPOSURA")
pacf(STDresiduals_bestfit_ARMA_GARCH_NUTRESA,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_NUTRESA")
pacf(STDresiduals_bestfit_ARMA_GARCH_GRUPOARGOS,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_GRUPOARGOS")
pacf(STDresiduals_bestfit_ARMA_GARCH_CELSIA,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_CELSIA")


# ACF of the squared standardized residuals
windows()
par(mfrow=c(4,4),las=1)
acf(Sqr_STDresiduals_bestfit_ARMA_GARCH_PFBCOLOM,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_PFBCOLOM")
acf(Sqr_STDresiduals_bestfit_ARMA_GARCH_ECOPETROL,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_ECOPETROL")
acf(Sqr_STDresiduals_bestfit_ARMA_GARCH_ISA,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_ISA")
acf(Sqr_STDresiduals_bestfit_ARMA_GARCH_ISAGEN,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_ISAGEN")
acf(Sqr_STDresiduals_bestfit_ARMA_GARCH_EXITO,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_EXITO")
acf(Sqr_STDresiduals_bestfit_ARMA_GARCH_BOGOTA,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_BOGOTA")
acf(Sqr_STDresiduals_bestfit_ARMA_GARCH_BCOLOMBIA,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_BCOLOMBIA")
acf(Sqr_STDresiduals_bestfit_ARMA_GARCH_BVC,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_BVC")
acf(Sqr_STDresiduals_bestfit_ARMA_GARCH_CORFICOLCF,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_CORFICOLCF")
acf(Sqr_STDresiduals_bestfit_ARMA_GARCH_GRUPOSURA,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_GRUPOSURA")
acf(Sqr_STDresiduals_bestfit_ARMA_GARCH_NUTRESA,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_NUTRESA")
acf(Sqr_STDresiduals_bestfit_ARMA_GARCH_GRUPOARGOS,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_GRUPOARGOS")
acf(Sqr_STDresiduals_bestfit_ARMA_GARCH_CELSIA,l=30,ylab="ACF", xlab="Rezago (días)",main="ACF_CELSIA")

# PACF of the squared standardized residuals
windows()
par(mfrow=c(4,4),las=1)
pacf(Sqr_STDresiduals_bestfit_ARMA_GARCH_PFBCOLOM,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_PFBCOLOM")
pacf(Sqr_STDresiduals_bestfit_ARMA_GARCH_ECOPETROL,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_ECOPETROL")
pacf(Sqr_STDresiduals_bestfit_ARMA_GARCH_ISA,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_ISA")
pacf(Sqr_STDresiduals_bestfit_ARMA_GARCH_ISAGEN,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_ISAGEN")
pacf(Sqr_STDresiduals_bestfit_ARMA_GARCH_EXITO,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_EXITO")
pacf(Sqr_STDresiduals_bestfit_ARMA_GARCH_BOGOTA,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_BOGOTA")
pacf(Sqr_STDresiduals_bestfit_ARMA_GARCH_BCOLOMBIA,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_BCOLOMBIA")
pacf(Sqr_STDresiduals_bestfit_ARMA_GARCH_BVC,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_BVC")
pacf(Sqr_STDresiduals_bestfit_ARMA_GARCH_CORFICOLCF,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_CORFICOLCF")
pacf(Sqr_STDresiduals_bestfit_ARMA_GARCH_GRUPOSURA,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_GRUPOSURA")
pacf(Sqr_STDresiduals_bestfit_ARMA_GARCH_NUTRESA,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_NUTRESA")
pacf(Sqr_STDresiduals_bestfit_ARMA_GARCH_GRUPOARGOS,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_GRUPOARGOS")
pacf(Sqr_STDresiduals_bestfit_ARMA_GARCH_CELSIA,l=30,ylab="PACF", xlab="Rezago (días)",main="PACF_CELSIA")

# Generalized Portmanteau Test p-values for the squared standardized residuals
# This test perform a goodness-of-fit test for the GARCH model by checking whether the standardized residuals
# are iid based on the ACF of the absolute residuals or squared residuals.
GPT_sqr_STDresiduals_bestfit_ARMA_GARCH_PFBCOLOM<-gBox(bestfit_ARMA_GARCH_PFBCOLOM,
x=retornos_PFBCOLOM,method="squared",plot=TRUE)

# Ljung-Box test of standardized residuals | AUTOCORRELATION TEST 1
Ljung_Box_PFBCOLOM<-Box.test(STDresiduals_bestfit_ARMA_GARCH_PFBCOLOM, lag = 500, type="Ljung");Ljung_Box_PFBCOLOM
Ljung_Box_ECOPETROL<-Box.test(STDresiduals_bestfit_ARMA_GARCH_ECOPETROL, lag = 500, type="Ljung");Ljung_Box_ECOPETROL
Ljung_Box_ISA<-Box.test(STDresiduals_bestfit_ARMA_GARCH_ISA, lag = 500, type="Ljung");Ljung_Box_ISA
Ljung_Box_ISAGEN<-Box.test(STDresiduals_bestfit_ARMA_GARCH_ISAGEN, lag = 500, type="Ljung");Ljung_Box_ISAGEN
Ljung_Box_EXITO<-Box.test(STDresiduals_bestfit_ARMA_GARCH_EXITO, lag = 500, type="Ljung");Ljung_Box_EXITO
Ljung_Box_BOGOTA<-Box.test(STDresiduals_bestfit_ARMA_GARCH_BOGOTA, lag = 500, type="Ljung");Ljung_Box_BOGOTA
Ljung_Box_BCOLOMBIA<-Box.test(STDresiduals_bestfit_ARMA_GARCH_BCOLOMBIA, lag = 500, type="Ljung");Ljung_Box_BCOLOMBIA
Ljung_Box_BVC<-Box.test(STDresiduals_bestfit_ARMA_GARCH_BVC, lag = 500, type="Ljung");Ljung_Box_BVC
Ljung_Box_CORFICOLCF<-Box.test(STDresiduals_bestfit_ARMA_GARCH_CORFICOLCF, lag = 500, type="Ljung");Ljung_Box_CORFICOLCF
Ljung_Box_GRUPOSURA<-Box.test(STDresiduals_bestfit_ARMA_GARCH_GRUPOSURA, lag = 500, type="Ljung");Ljung_Box_GRUPOSURA
Ljung_Box_NUTRESA<-Box.test(STDresiduals_bestfit_ARMA_GARCH_NUTRESA, lag = 500, type="Ljung");Ljung_Box_NUTRESA
Ljung_Box_GRUPOARGOS<-Box.test(STDresiduals_bestfit_ARMA_GARCH_GRUPOARGOS, lag = 500, type="Ljung");Ljung_Box_GRUPOARGOS
Ljung_Box_CELSIA<-Box.test(STDresiduals_bestfit_ARMA_GARCH_CELSIA, lag = 500, type="Ljung");Ljung_Box_CELSIA

Ljung_Box_STDresiduals_PFBCOLOM <- NULL
for(i in 1:500){
Ljung_Box2 <- Box.test(STDresiduals_bestfit_ARMA_GARCH_PFBCOLOM, lag = i, type="Ljung")$p.value
Ljung_Box_STDresiduals_PFBCOLOM <- rbind(Ljung_Box_STDresiduals_PFBCOLOM, cbind(i,Ljung_Box2))
}

Ljung_Box_STDresiduals_ECOPETROL <- NULL
for(i in 1:500){
  Ljung_Box2 <- Box.test(STDresiduals_bestfit_ARMA_GARCH_ECOPETROL, lag = i, type="Ljung")$p.value
  Ljung_Box_STDresiduals_ECOPETROL <- rbind(Ljung_Box_STDresiduals_ECOPETROL, cbind(i,Ljung_Box2))
}

Ljung_Box_STDresiduals_ISA <- NULL
for(i in 1:500){
  Ljung_Box2 <- Box.test(STDresiduals_bestfit_ARMA_GARCH_ISA, lag = i, type="Ljung")$p.value
  Ljung_Box_STDresiduals_ISA <- rbind(Ljung_Box_STDresiduals_ISA, cbind(i,Ljung_Box2))
}

Ljung_Box_STDresiduals_ISAGEN <- NULL
for(i in 1:500){
  Ljung_Box2 <- Box.test(STDresiduals_bestfit_ARMA_GARCH_ISAGEN, lag = i, type="Ljung")$p.value
  Ljung_Box_STDresiduals_ISAGEN <- rbind(Ljung_Box_STDresiduals_ISAGEN, cbind(i,Ljung_Box2))
}

Ljung_Box_STDresiduals_EXITO <- NULL
for(i in 1:500){
  Ljung_Box2 <- Box.test(STDresiduals_bestfit_ARMA_GARCH_EXITO, lag = i, type="Ljung")$p.value
  Ljung_Box_STDresiduals_EXITO <- rbind(Ljung_Box_STDresiduals_EXITO, cbind(i,Ljung_Box2))
}

Ljung_Box_STDresiduals_BOGOTA <- NULL
for(i in 1:500){
  Ljung_Box2 <- Box.test(STDresiduals_bestfit_ARMA_GARCH_BOGOTA, lag = i, type="Ljung")$p.value
  Ljung_Box_STDresiduals_BOGOTA <- rbind(Ljung_Box_STDresiduals_BOGOTA, cbind(i,Ljung_Box2))
}

Ljung_Box_STDresiduals_BCOLOMBIA <- NULL
for(i in 1:500){
  Ljung_Box2 <- Box.test(STDresiduals_bestfit_ARMA_GARCH_BCOLOMBIA, lag = i, type="Ljung")$p.value
  Ljung_Box_STDresiduals_BCOLOMBIA <- rbind(Ljung_Box_STDresiduals_BCOLOMBIA, cbind(i,Ljung_Box2))
}

Ljung_Box_STDresiduals_BVC <- NULL
for(i in 1:500){
  Ljung_Box2 <- Box.test(STDresiduals_bestfit_ARMA_GARCH_BVC, lag = i, type="Ljung")$p.value
  Ljung_Box_STDresiduals_BVC <- rbind(Ljung_Box_STDresiduals_BVC, cbind(i,Ljung_Box2))
}

Ljung_Box_STDresiduals_CORFICOLCF <- NULL
for(i in 1:500){
  Ljung_Box2 <- Box.test(STDresiduals_bestfit_ARMA_GARCH_CORFICOLCF, lag = i, type="Ljung")$p.value
  Ljung_Box_STDresiduals_CORFICOLCF <- rbind(Ljung_Box_STDresiduals_CORFICOLCF, cbind(i,Ljung_Box2))
}

Ljung_Box_STDresiduals_GRUPOSURA <- NULL
for(i in 1:500){
  Ljung_Box2 <- Box.test(STDresiduals_bestfit_ARMA_GARCH_GRUPOSURA, lag = i, type="Ljung")$p.value
  Ljung_Box_STDresiduals_GRUPOSURA <- rbind(Ljung_Box_STDresiduals_GRUPOSURA, cbind(i,Ljung_Box2))
}

Ljung_Box_STDresiduals_NUTRESA <- NULL
for(i in 1:500){
  Ljung_Box2 <- Box.test(STDresiduals_bestfit_ARMA_GARCH_NUTRESA, lag = i, type="Ljung")$p.value
  Ljung_Box_STDresiduals_NUTRESA <- rbind(Ljung_Box_STDresiduals_NUTRESA, cbind(i,Ljung_Box2))
}

Ljung_Box_STDresiduals_GRUPOARGOS <- NULL
for(i in 1:500){
  Ljung_Box2 <- Box.test(STDresiduals_bestfit_ARMA_GARCH_GRUPOARGOS, lag = i, type="Ljung")$p.value
  Ljung_Box_STDresiduals_GRUPOARGOS <- rbind(Ljung_Box_STDresiduals_GRUPOARGOS, cbind(i,Ljung_Box2))
}

Ljung_Box_STDresiduals_CELSIA <- NULL
for(i in 1:500){
  Ljung_Box2 <- Box.test(STDresiduals_bestfit_ARMA_GARCH_CELSIA, lag = i, type="Ljung")$p.value
  Ljung_Box_STDresiduals_CELSIA <- rbind(Ljung_Box_STDresiduals_CELSIA, cbind(i,Ljung_Box2))
}

windows()
par(mfrow=c(4,4))

colnames(Ljung_Box_STDresiduals_PFBCOLOM) <- c("Rezago","p-valor");
plot(Ljung_Box_STDresiduals_PFBCOLOM,ylim=c(0,1), main="PFBCOLOM")
abline(h=0.05,col="red")

colnames(Ljung_Box_STDresiduals_ECOPETROL) <- c("Rezago","p-valor");
plot(Ljung_Box_STDresiduals_ECOPETROL,ylim=c(0,1), main="ECOPETROL")
abline(h=0.05,col="red")

colnames(Ljung_Box_STDresiduals_ISA) <- c("Rezago","p-valor");
plot(Ljung_Box_STDresiduals_ISA,ylim=c(0,1), main="ISA")
abline(h=0.05,col="red")

colnames(Ljung_Box_STDresiduals_ISAGEN) <- c("Rezago","p-valor");
plot(Ljung_Box_STDresiduals_ISAGEN,ylim=c(0,1), main="ISAGEN")
abline(h=0.05,col="red")

colnames(Ljung_Box_STDresiduals_EXITO) <- c("Rezago","p-valor");
plot(Ljung_Box_STDresiduals_EXITO,ylim=c(0,1), main="EXITO")
abline(h=0.05,col="red")

colnames(Ljung_Box_STDresiduals_BOGOTA) <- c("Rezago","p-valor");
plot(Ljung_Box_STDresiduals_BOGOTA,ylim=c(0,1), main="BOGOTA")
abline(h=0.05,col="red")

colnames(Ljung_Box_STDresiduals_BCOLOMBIA) <- c("Rezago","p-valor");
plot(Ljung_Box_STDresiduals_BCOLOMBIA,ylim=c(0,1), main="BCOLOMBIA")
abline(h=0.05,col="red")

colnames(Ljung_Box_STDresiduals_BVC) <- c("Rezago","p-valor");
plot(Ljung_Box_STDresiduals_BVC,ylim=c(0,1), main="BVC")
abline(h=0.05,col="red")

colnames(Ljung_Box_STDresiduals_CORFICOLCF) <- c("Rezago","p-valor");
plot(Ljung_Box_STDresiduals_CORFICOLCF,ylim=c(0,1), main="CORFICOLCF")
abline(h=0.05,col="red")

colnames(Ljung_Box_STDresiduals_GRUPOSURA) <- c("Rezago","p-valor");
plot(Ljung_Box_STDresiduals_GRUPOSURA,ylim=c(0,1), main="GRUPOSURA")
abline(h=0.05,col="red")

colnames(Ljung_Box_STDresiduals_NUTRESA) <- c("Rezago","p-valor");
plot(Ljung_Box_STDresiduals_NUTRESA,ylim=c(0,1), main="NUTRESA")
abline(h=0.05,col="red")

colnames(Ljung_Box_STDresiduals_GRUPOARGOS) <- c("Rezago","p-valor");
plot(Ljung_Box_STDresiduals_GRUPOARGOS,ylim=c(0,1), main="GRUPOARGOS")
abline(h=0.05,col="red")

colnames(Ljung_Box_STDresiduals_CELSIA) <- c("Rezago","p-valor");
plot(Ljung_Box_STDresiduals_CELSIA,ylim=c(0,1), main="CELSIA")
abline(h=0.05,col="red")

# autocorTest for standardized residuals | AUTOCORRELATION TEST 2
AutoCorr_bestfit_ARMA_GARCH_PFBCOLOM<-AutocorTest(STDresiduals_bestfit_ARMA_GARCH_PFBCOLOM,lag=500);AutoCorr_bestfit_ARMA_GARCH_PFBCOLOM
AutoCorr_bestfit_ARMA_GARCH_ECOPETROL<-AutocorTest(STDresiduals_bestfit_ARMA_GARCH_ECOPETROL,lag=500);AutoCorr_bestfit_ARMA_GARCH_ECOPETROL
AutoCorr_bestfit_ARMA_GARCH_ISA<-AutocorTest(STDresiduals_bestfit_ARMA_GARCH_ISA,lag=500);AutoCorr_bestfit_ARMA_GARCH_ISA
AutoCorr_bestfit_ARMA_GARCH_ISAGEN<-AutocorTest(STDresiduals_bestfit_ARMA_GARCH_ISAGEN,lag=500);AutoCorr_bestfit_ARMA_GARCH_ISAGEN
AutoCorr_bestfit_ARMA_GARCH_EXITO<-AutocorTest(STDresiduals_bestfit_ARMA_GARCH_EXITO,lag=500);AutoCorr_bestfit_ARMA_GARCH_EXITO
AutoCorr_bestfit_ARMA_GARCH_BOGOTA<-AutocorTest(STDresiduals_bestfit_ARMA_GARCH_BOGOTA,lag=500);AutoCorr_bestfit_ARMA_GARCH_BOGOTA
AutoCorr_bestfit_ARMA_GARCH_BCOLOMBIA<-AutocorTest(STDresiduals_bestfit_ARMA_GARCH_BCOLOMBIA,lag=500);AutoCorr_bestfit_ARMA_GARCH_BCOLOMBIA
AutoCorr_bestfit_ARMA_GARCH_BVC<-AutocorTest(STDresiduals_bestfit_ARMA_GARCH_BVC,lag=500);AutoCorr_bestfit_ARMA_GARCH_BVC
AutoCorr_bestfit_ARMA_GARCH_CORFICOLCF<-AutocorTest(STDresiduals_bestfit_ARMA_GARCH_CORFICOLCF,lag=500);AutoCorr_bestfit_ARMA_GARCH_CORFICOLCF
AutoCorr_bestfit_ARMA_GARCH_GRUPOSURA<-AutocorTest(STDresiduals_bestfit_ARMA_GARCH_GRUPOSURA,lag=500);AutoCorr_bestfit_ARMA_GARCH_GRUPOSURA
AutoCorr_bestfit_ARMA_GARCH_NUTRESA<-AutocorTest(STDresiduals_bestfit_ARMA_GARCH_NUTRESA,lag=500);AutoCorr_bestfit_ARMA_GARCH_NUTRESA
AutoCorr_bestfit_ARMA_GARCH_GRUPOARGOS<-AutocorTest(STDresiduals_bestfit_ARMA_GARCH_GRUPOARGOS,lag=500);AutoCorr_bestfit_ARMA_GARCH_GRUPOARGOS
AutoCorr_bestfit_ARMA_GARCH_CELSIA<-AutocorTest(STDresiduals_bestfit_ARMA_GARCH_CELSIA,lag=500);AutoCorr_bestfit_ARMA_GARCH_CELSIA

### PRUEBA DE MULTIPLICADORES DE LAGRANGE - HOMOCEDASTICIDAD
MLG_sqr_STDresiduals_PFBCOLOM<-ArchTest(Sqr_STDresiduals_bestfit_ARMA_GARCH_PFBCOLOM,lag=100);MLG_sqr_STDresiduals_PFBCOLOM
MLG_sqr_STDresiduals_ECOPETROL<-ArchTest(Sqr_STDresiduals_bestfit_ARMA_GARCH_ECOPETROL,lag=100);MLG_sqr_STDresiduals_ECOPETROL
MLG_sqr_STDresiduals_ISA<-ArchTest(Sqr_STDresiduals_bestfit_ARMA_GARCH_ISA,lag=100);MLG_sqr_STDresiduals_ISA
MLG_sqr_STDresiduals_ISAGEN<-ArchTest(Sqr_STDresiduals_bestfit_ARMA_GARCH_ISAGEN,lag=100);MLG_sqr_STDresiduals_ISAGEN
MLG_sqr_STDresiduals_EXITO<-ArchTest(Sqr_STDresiduals_bestfit_ARMA_GARCH_EXITO,lag=100);MLG_sqr_STDresiduals_EXITO
MLG_sqr_STDresiduals_BOGOTA<-ArchTest(Sqr_STDresiduals_bestfit_ARMA_GARCH_BOGOTA,lag=100);MLG_sqr_STDresiduals_BOGOTA
MLG_sqr_STDresiduals_BCOLOMBIA<-ArchTest(Sqr_STDresiduals_bestfit_ARMA_GARCH_BCOLOMBIA,lag=100);MLG_sqr_STDresiduals_BCOLOMBIA
MLG_sqr_STDresiduals_BVC<-ArchTest(Sqr_STDresiduals_bestfit_ARMA_GARCH_BVC,lag=100);MLG_sqr_STDresiduals_BVC
MLG_sqr_STDresiduals_CORFICOLCF<-ArchTest(Sqr_STDresiduals_bestfit_ARMA_GARCH_CORFICOLCF,lag=100);MLG_sqr_STDresiduals_CORFICOLCF
MLG_sqr_STDresiduals_GRUPOSURA<-ArchTest(Sqr_STDresiduals_bestfit_ARMA_GARCH_GRUPOSURA,lag=100);MLG_sqr_STDresiduals_GRUPOSURA
MLG_sqr_STDresiduals_NUTRESA<-ArchTest(Sqr_STDresiduals_bestfit_ARMA_GARCH_NUTRESA,lag=100);MLG_sqr_STDresiduals_NUTRESA
MLG_sqr_STDresiduals_GRUPOARGOS<-ArchTest(Sqr_STDresiduals_bestfit_ARMA_GARCH_GRUPOARGOS,lag=100);MLG_sqr_STDresiduals_GRUPOARGOS
MLG_sqr_STDresiduals_CELSIA<-ArchTest(Sqr_STDresiduals_bestfit_ARMA_GARCH_CELSIA,lag=100);MLG_sqr_STDresiduals_CELSIA

### PRUEBA DE MCLEOD-LI - HOMOCEDASTICIDAD
windows()
par(mfrow=c(4,4),las=1)
McLeod_Li_bestfit_AG_PFBCOLOM<-McLeod.Li.test(y=STDresiduals_bestfit_ARMA_GARCH_PFBCOLOM,gof.lag=100,main="PFBCOLOM")
McLeod_Li_bestfit_AG_ECOPETROL<-McLeod.Li.test(y=STDresiduals_bestfit_ARMA_GARCH_ECOPETROL,gof.lag=100,main="ECOPETROL")
McLeod_Li_bestfit_AG_ISA<-McLeod.Li.test(y=STDresiduals_bestfit_ARMA_GARCH_ISA,gof.lag=100,main="ISA")
McLeod_Li_bestfit_AG_ISAGEN<-McLeod.Li.test(y=STDresiduals_bestfit_ARMA_GARCH_ISAGEN,gof.lag=100,main="ISAGEN")
McLeod_Li_bestfit_AG_EXITO<-McLeod.Li.test(y=STDresiduals_bestfit_ARMA_GARCH_EXITO,gof.lag=100,main="EXITO")
McLeod_Li_bestfit_AG_BOGOTA<-McLeod.Li.test(y=STDresiduals_bestfit_ARMA_GARCH_BOGOTA,gof.lag=100,main="BOGOTA")
McLeod_Li_bestfit_AG_BCOLOMBIA<-McLeod.Li.test(y=STDresiduals_bestfit_ARMA_GARCH_BCOLOMBIA,gof.lag=100,main="BCOLOMBIA")
McLeod_Li_bestfit_AG_BVC<-McLeod.Li.test(y=STDresiduals_bestfit_ARMA_GARCH_BVC,gof.lag=100,main="BVC")
McLeod_Li_bestfit_AG_CORFICOLCF<-McLeod.Li.test(y=STDresiduals_bestfit_ARMA_GARCH_CORFICOLCF,gof.lag=100,main="CORFICOLCF")
McLeod_Li_bestfit_AG_GRUPOSURA<-McLeod.Li.test(y=STDresiduals_bestfit_ARMA_GARCH_GRUPOSURA,gof.lag=100,main="GRUPOSURA")
McLeod_Li_bestfit_AG_NUTRESA<-McLeod.Li.test(y=STDresiduals_bestfit_ARMA_GARCH_NUTRESA,gof.lag=100,main="NUTRESA")
McLeod_Li_bestfit_AG_GRUPOARGOS<-McLeod.Li.test(y=STDresiduals_bestfit_ARMA_GARCH_GRUPOARGOS,gof.lag=100,main="GRUPOARGOS")
McLeod_Li_bestfit_AG_CELSIA<-McLeod.Li.test(y=STDresiduals_bestfit_ARMA_GARCH_CELSIA,gof.lag=100,main="CELSIA")

#=====================================================================================================================================================

### EXTRACTING STANDARDIZED RESIDUALS

STDr=data.frame(PFBCOLOM=STDresiduals_bestfit_ARMA_GARCH_PFBCOLOM,
ECOPETROL=STDresiduals_bestfit_ARMA_GARCH_ECOPETROL,ISA=STDresiduals_bestfit_ARMA_GARCH_ISA,
ISAGEN=STDresiduals_bestfit_ARMA_GARCH_ISAGEN,EXITO=STDresiduals_bestfit_ARMA_GARCH_EXITO,
BOGOTA=STDresiduals_bestfit_ARMA_GARCH_BOGOTA,BCOLOMBIA=STDresiduals_bestfit_ARMA_GARCH_BCOLOMBIA,
BVC=STDresiduals_bestfit_ARMA_GARCH_BVC,CORFICOLCF=STDresiduals_bestfit_ARMA_GARCH_CORFICOLCF,
GRUPOSURA=STDresiduals_bestfit_ARMA_GARCH_GRUPOSURA,NUTRESA=STDresiduals_bestfit_ARMA_GARCH_NUTRESA,
GRUPOARGOS=STDresiduals_bestfit_ARMA_GARCH_GRUPOARGOS,CELSIA=STDresiduals_bestfit_ARMA_GARCH_CELSIA,
row.names=NULL)

### PLOTTING STANDARDIZED RESIDUALS
windows()
par(mfrow=c(4,4),las=1)
plot(STD[,1],col="blue",ylab="STD residuales", xlab="Días", main="PFBCOLOM",type="l",las=1)
plot(STD[,2],col="blue",ylab="STD residuales", xlab="Días", main="ECOPETROL",type="l",las=1)
plot(STD[,3],col="blue",ylab="STD residuales", xlab="Días", main="ISA",type="l",las=1)
plot(STD[,4],col="blue",ylab="STD residuales", xlab="Días", main="ISAGEN",type="l",las=1)
plot(STD[,5],col="blue",ylab="STD residuales", xlab="Días", main="EXITO",type="l",las=1)
plot(STD[,6],col="blue",ylab="STD residuales", xlab="Días", main="BOGOTA",type="l",las=1)
plot(STD[,7],col="blue",ylab="STD residuales", xlab="Días", main="BCOLOMBIA",type="l",las=1)
plot(STD[,8],col="blue",ylab="STD residuales", xlab="Días", main="BVC",type="l",las=1)
plot(STD[,9],col="blue",ylab="STD residuales", xlab="Días", main="CORFICOLCF",type="l",las=1)
plot(STD[,10],col="blue",ylab="STD residuales", xlab="Días", main="GRUPOSURA",type="l",las=1)
plot(STD[,11],col="blue",ylab="STD residuales", xlab="Días", main="NUTRESA",type="l",las=1)
plot(STD[,12],col="blue",ylab="STD residuales", xlab="Días", main="GRUPOARGOS",type="l",las=1)
plot(STD[,13],col="blue",ylab="STD residuales", xlab="Días", main="CELSIA",type="l",las=1)

### EXPORTING DATA FRAME TO A TEXT FILE
write.table(STDr, "C:/Users/Rodrigo/Documents/MAESTRÍA/Tesis/R/STDr_3.txt",sep="\t")

#=====================================================================================================================================================

### ETAPA DE PRONÓSTICO POR MEDIO DEL MODELO ESPECIFICADO

PFBCOLOM4CC<-ugarchforecast(bestfit_ARMA_GARCH_PFBCOLOM, n.ahead=1, n.roll=122)
ECOPETROL4CC<-ugarchforecast(bestfit_ARMA_GARCH_ECOPETROL, n.ahead=1, n.roll=122)
ISA4CC<-ugarchforecast(bestfit_ARMA_GARCH_ISA, n.ahead=1, n.roll=122)
ISAGEN4CC<-ugarchforecast(bestfit_ARMA_GARCH_ISAGEN, n.ahead=1, n.roll=122)
EXITO4CC<-ugarchforecast(bestfit_ARMA_GARCH_EXITO, n.ahead=1, n.roll=122)
BOGOTA4CC<-ugarchforecast(bestfit_ARMA_GARCH_BOGOTA, n.ahead=1, n.roll=122)
BCOLOMBIA4CC<-ugarchforecast(bestfit_ARMA_GARCH_BCOLOMBIA, n.ahead=1, n.roll=122)
BVC4CC<-ugarchforecast(bestfit_ARMA_GARCH_BVC, n.ahead=1, n.roll=122)
CORFICOLCF4CC<-ugarchforecast(bestfit_ARMA_GARCH_CORFICOLCF, n.ahead=1, n.roll=122)
GRUPOSURA4CC<-ugarchforecast(bestfit_ARMA_GARCH_GRUPOSURA, n.ahead=1, n.roll=122)
NUTRESA4CC<-ugarchforecast(bestfit_ARMA_GARCH_NUTRESA, n.ahead=1, n.roll=122)
GRUPOARGOS4CC<-ugarchforecast(bestfit_ARMA_GARCH_GRUPOARGOS, n.ahead=1, n.roll=122)
CELSIA4CC<-ugarchforecast(bestfit_ARMA_GARCH_CELSIA, n.ahead=1, n.roll=122)


PFBSigma4C<-t(sigma(PFBCOLOM4CC))
ECOSigma4C<-t(sigma(ECOPETROL4CC))
ISASigma4C<-t(sigma(ISA4CC))
GENSigma4C<-t(sigma(ISAGEN4CC))
EXISigma4C<-t(sigma(EXITO4CC))
BOGSigma4C<-t(sigma(BOGOTA4CC))
BCOSigma4C<-t(sigma(BCOLOMBIA4CC))
BVCSigma4C<-t(sigma(BVC4CC))
CORSigma4C<-t(sigma(CORFICOLCF4CC))
GRSSigma4C<-t(sigma(GRUPOSURA4CC))
NUTSigma4C<-t(sigma(NUTRESA4CC))
GRASigma4C<-t(sigma(GRUPOARGOS4CC))
CELSigma4C<-t(sigma(CELSIA4CC))

Sigma4C<-cbind(PFB=PFBSigma4C[,1],ECO=ECOSigma4C[,1],ISA=ISASigma4C[,1],GEN=GENSigma4C[,1],EXI=EXISigma4C[,1],
               BOG=BOGSigma4C[,1],BCO=BCOSigma4C[,1],BVC=BVCSigma4C[,1],COR=CORSigma4C[,1],GRS=GRSSigma4C[,1],
               NUT=NUTSigma4C[,1],GRA=GRASigma4C[,1],CEL=CELSigma4C[,1])


PFBCMean4C<-t(fitted(PFBCOLOM4CC))
ECOCMean4C<-t(fitted(ECOPETROL4CC))
ISACMean4C<-t(fitted(ISA4CC))
GENCMean4C<-t(fitted(ISAGEN4CC))
EXICMean4C<-t(fitted(EXITO4CC))
BOGCMean4C<-t(fitted(BOGOTA4CC))
BCOCMean4C<-t(fitted(BCOLOMBIA4CC))
BVCCMean4C<-t(fitted(BVC4CC))
CORCMean4C<-t(fitted(CORFICOLCF4CC))
GRSCMean4C<-t(fitted(GRUPOSURA4CC))
NUTCMean4C<-t(fitted(NUTRESA4CC))
GRACMean4C<-t(fitted(GRUPOARGOS4CC))
CELCMean4C<-t(fitted(CELSIA4CC))


CMean4C<-cbind(PFB=PFBCMean4C[,1],ECO=ECOCMean4C[,1],ISA=ISACMean4C[,1],GEN=GENCMean4C[,1],EXI=EXICMean4C[,1],
               BOG=BOGCMean4C[,1],BCO=BCOCMean4C[,1],BVC=BVCCMean4C[,1],COR=CORCMean4C[,1],GRS=GRSCMean4C[,1],
               NUT=NUTCMean4C[,1],GRA=GRACMean4C[,1],CEL=CELCMean4C[,1])