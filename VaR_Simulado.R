
library(quantmod)
library(PortfolioAnalytics)
library(moments)
library(ggplot2)

A1<-getSymbols('AAPL',src='yahoo',from='2015-11-24',to='2020-11-27', 
                auto.assign = FALSE )[,c(4)]

chartSeries(A1) #Hacemos el gr�fico de los precios
A1<-na.omit(A1) #Omitimos NA
names(A1)<-'AAPL' #Renombramos los precios
logret<-diff(log(A1$AAPL)) #Calculamos los retornos diarios
# Retorno diarios
logret<-diff(log(A1$AAPL))[-1] # Quitamos el �ltimo valor
mu<-round(mean(logret),8);mu #Calculamos el rendimiento 
sig<-round(sd(logret),8);sig #Calculamos la riesgo-volatilidad

rvec <- as.vector(logret)
hist(rvec)
plot(rvec, lty=2, col='red')




# Modelo 1  considerando mu y sigma
var<-round(qnorm(0.01,mu,sig),6);var
ES<-mu-sig*dnorm(qnorm(0.01,0,1),0,1)/0.01;ES
#Valor en riesgo en un d�a 
1000*(exp(var)-1)
# Especting Shortfall
#M�xima perdida en un d�a 
1000*(exp(ES)-1)



#Prueba de Asimetr�a
S<-(sum((rvec-mean(rvec))^{3}))/(length(rvec)*sd(rvec)^{3});S
round(skewness(rvec),2)

#Prueba de Kurtosis en busca peso de las colas
K<-(sum((rvec-mean(rvec))^{4}))/(length(rvec)*sd(rvec)^{4});K
round(kurtosis(rvec),2)

#Prueba de normalidad asimetr�a=0 y Kurtosis=3 en los errores
JB<-(length(rvec)/6)*(S^{2}+(K-3)^{2}/4);JB
jarque.test(rvec)

#Consideremos que queremos invertir 1000 millones de pesos en las acciones de AAPL
#Calcula la m�nima perdida esperada en un d�a 
#Calcula la m�xima perdida en un d�a

# Modelo 1  considerando mu y sigma
var<-round(qnorm(0.01,mu,sig),6);var
ES<-mu-sig*dnorm(qnorm(0.01,0,1),0,1)/0.01;ES
#Valor en riesgo en un d�a 
1000*(exp(var)-1)
# Especting Shortfall
#M�xima perdida en un d�a 
1000*(exp(ES)-1)

## Modelo 2 Simulaci�n considerando normalidad de retornos 1
alpha<-0.01
set.seed(123789)
rvec<-rnorm(100000,mu, sig)
hist(rvec)
VaR<-quantile(rvec,alpha)
ES<-mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)
#M�nima perdida en un d�a 
1000*(exp(VaR)-1)
#M�xima perdida en un d�a 
1000*(exp(ES)-1)


## Modelo 3 Simulaci�n de datos actuales retornos 2
rvec<-sample(as.vector(logret),100000,replace=TRUE)
hist(rvec)
VaR<-quantile(rvec,alpha)
ES<-mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)
#La m�nima perdida en un d�a 
1000*(exp(VaR)-1)
#La m�xima perdida en un d�a
1000*(exp(ES)-1)




precios<-NULL
tickers<-c('AMZN','AAPL','NFLX','FB')
for (nom in tickers) {
  precios<-cbind(precios, getSymbols(nom, src='yahoo',
                from='2019-11-24',to='2020-11-23',auto.assign = FALSE))[,c(4)]
}


plot(precios)



options(scipen=999)
retornos<-na.omit(ROC(precios))
ret<-c('AMZN','AAPL','NFLX','FB')
ret<-colnames(retornos)

sharpe_idx<-SharpeRatio(R=retornos, rf=0, FUN = 'StdDev')
porf_ini<-portfolio.spec(assets = colnames(retornos))


# https://www.youtube.com/watch?v=5gmhZEl0kI8&t=47s

