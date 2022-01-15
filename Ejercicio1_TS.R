########################################################
####                                                ####
####                   Ejemplo 1                    ####
####                                                ####
########################################################

#### A partir de la siguiente secuencia de números aleatorios

rm(list = ls())
x<-c(0.06843,0.17149,0.69199,0.94022,0.99665,0.05561,
     0.77891,0.39030,0.01555,0.88060,0.23928,0.54564,
     0.13275,0.79979,0.34237,0.32037,0.34880,0.45604,
     0.13332,0.48576,0.98376,0.38324,0.21548,0.68331,
     0.33395,0.71230,0.77144,0.24613,0.98590,0.21735)
#### calculo de m=sqrt(x)
m<-floor(sqrt(length(x)))
#### a) Prueba de bondad de ajuste chi^{2} #### 
h<-hist(x)
FO<-h$counts
#### Se propone una distribución uniforme ####
#### Con F(x)=(y-a)/(b-a)
int<-c('0.0-0.2','0.2-0.4','0.4-0.6','0.6-0.8','0.8-1.0')
a<-0
b<-1
x1<-0.2
n<-length(x)
Fx<-(x1-a)/(b-a)
FE<-Fx*n
FE<-sum(FE)
C<-(FE-FO)^2/FE
pchi<-cbind(int,FO,Fx, FE, C)
C<-sum(C)
chi2<-qchisq(0.95,29)
if(C<=chi2){cat("Se Acepta la Hipótesis")} else{cat("Se Rechaza la Hipótesis")}

#### b) Prueba de bondad de ajuste Kolmogorov-Smirnov ####
rm(list = ls())
x<-c(0.06843,0.17149,0.69199,0.94022,0.99665,0.05561,
     0.77891,0.39030,0.01555,0.88060,0.23928,0.54564,
     0.13275,0.79979,0.34237,0.32037,0.34880,0.45604,
     0.13332,0.48576,0.98376,0.38324,0.21548,0.68331,
     0.33395,0.71230,0.77144,0.24613,0.98590,0.21735)
#### calculo de m=sqrt(x)
m<-floor(sqrt(length(x)))
bin<-seq(0.2,1, by=0.2)
int<-c('0.0-0.2','0.2-0.4','0.4-0.6','0.6-0.8','0.8-1.0')
#### Se realiza un histograma de frecuencias
h<-hist(x)
#### Se calcula la frecuencia observada
FO<-h$counts
#### Se calcula la Frecuencia observada acumulada
FOA<-cumsum(FO)
#### Se calcula la probabilidad observada
PO<-FO/length(x)
#### Se calcula la probabilidad observada acumulada
PAO<-cumsum(PO)
#### Se calcula la probabilidad esperada
#### Con F(x)=(x-a)/(b-a)# Se propone una distribución uniforme
a<-0
b<-1
Fx<-(bin-a)/(b-a)
#### Se Calcula la probabilidad acumulada y observada ####
PAE<-Fx*bin
#### Ahora obtenemos la diferencia de D<-|PAE-POA| 
D<-abs(PAE-PAO)
#### Se encuentra el máximo de la tabla 
DM<-max(D)
#### AHora se hace la comparación de los valores encontrados
#### con el nivel de confianza solicitado 95% con d=30
#### d_{30}<-0.2182
d<-0.2182
if(DM<=d){cat("Se Acepta la Hipótesis")} else{cat("Se Rechaza la Hipótesis")}
#### c) Prueba de medias ####

#### d) Prueba de Varianza ####
#### e) Prueba de uniformidad ####
#### f) Prueba de independencia póker #### 
#### g) Prueba de independencia de corridas arriba y abajo ####
#### h) Prueba de independencia arriba y abajo de la media ####
#### i) Prueba de independencia de huecos ####






