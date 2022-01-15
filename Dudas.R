#####################################
####            Ejercicio        ####
#####################################
rm(list=ls())

datos<-c(1,8,12,20,10,3,1)
seq(0,17, by=1)
h<-hist(datos, breaks = 10)
POA<-cumsum(datos/sum(datos));POA
Z<-(datos-mean(datos))/sd(datos)
Z
PEA<-c(0.2327,0.3265,0.4362,0.5511,0.6691,0.7601,0.8403,0.9006,0.9423)
KS<-max(abs(POA-PEA))
d<-0.183

if(KS<=d){cat("Se Acepta la Hipótesis")} else{cat("Se Rechaza la Hipótesis")}



#### Ejercicio 5 ####

x<-c(3,3,3,3,3,4,3,2,1,2,4,3,2,3,1,2,2,3,4,3,3,5,2,2,4,
     2,5,2,2,3,3,1,3,0,3,2,5,4,3,2,2,3,0,4,4,5,3,2,3,4)
h<-hist(x)
h<-hist(x, breaks = sqrt(length(x))) #
FO<-h$counts;FO
h$breaks

hist(x, freq=F, col = "lightblue", border = "green")
xf<-rbinom(length(x),5,.2);xf #Binomial 
xf<-rnorm(length(x), mean(x), sd(x)) #Normal
xf<-rpois(length(x), mean(x)) # Poison
xf<-rweibull(length(x), shape=3, scale = 3) #Weibull
xf<-rtri(length(x), min = 0, max = 5, mode = 2.5) #Triangular cargando el paquete 
xf<-runif(length(x), min(x), max(x)) # Uniforme
xf<-rlnorm(length(x), meanlog = log(mean(x)), sdlog = log(sd(x))) #Log-Normal
lines(density(xf, bw=1))


FX<-((x-min(x))^{2})/((max(x)-min(x))*(mean(x)-min(x)))
FX1<-1-((max(x)-x)^{2})/((max(x)-min(x))*(max(x)-mean(x)))

FX<-vector()
FX1<-vector()
clasif <- numeric(length(x)) # Acá voy a guardar el resultado
for(i in 1:50) {
  if(x <= 2.5) { ## Se si se cumple, entonces es "grande"
    texto <- paste(i, 'grande')
    print(texto)
    clasif[i] <- texto ## *1
    FX[i]<-((x[i]-min(x))^{2})/((max(x)-min(x))*(mean(x)-min(x)))
  } else {    ## Si no se cumple, entonces es "chico"
    texto <- paste(i, 'chico')
    print(texto)
    clasif[i] <- texto ## *2
      }
  # clasif[i] <- texto ## Este comando puede sustituir las lineas *1 y *2
}
clasif
FX

hist(x, xlim = c(min(x), max(x)), probability = TRUE,
     nclass = max(x) - min(x) + 1, col = 'lightblue',
     main = 'Distribución Binomial N=5, p=0.2')
lines(density(xf, bw=1), col = 'red', lwd = 3)

lambda <- 2 #
n <- 10^5   # Número de Simulaciones
set.seed(1)
ri <- runif(n) #Números aleatorios
X <- -log(ri)/lambda # Variables aleatorias
hist(X,breaks='FD', freq = FALSE, col='blueviolet', border="coral2",
     main = "", xlim = c(0,5), ylim = c(0, 2.5))
curve(dexp(x, lambda), lwd = 2, add = TRUE)


 