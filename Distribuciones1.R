rm(list = ls())

########## Distribución Uniforme Funciones ##########
mi<-0
ma<-1
#png("Uniforme.png")
x <- seq(mi-1, ma+1 , 0.01)
pdf_x <- dunif(x, mi, ma) #Función de Distribución Acumulada (CFD)
cdf_x <- punif(x, mi, ma)  #Función de Densidad (PDF)
prob <- seq(0, 1, 0.01)
inv_cdf_x <- qunif(prob, mi, ma, lower.tail = TRUE, log.p = FALSE) # Quantiles-Inverso de CFD
plot(x, pdf_x, type = "l", main = "PDF de x",  col="green",lwd=2)
lines(x, cdf_x, type = "l", main = "CDF de  x", col='red', lwd=2)
lines(prob, inv_cdf_x, type = "l", main = "CDF Inversa  de x", col='blue', lwd=2)
abline(h=(a+b)/2, lwd=2)
abline(v=(b-a)^2/12, col='yellow',lwd=2)
legend(x = "right", legend = c("PDF de x", "CDF de  x", 'CDF Inversa  de x',
                               'Media', 'Varianza'),
       fill = c("green", "red", 'blue', 'black','yellow'), 
       title = "Distribuciones")
#dev.off()

########## Distribución Uniforme Simulada ##########
a<-0
b<-1 
#png("suniforme.png")
x<-runif(100, min=a, max=b) 
hist(x, main="Histograma Distribución Uniforme", 
     xlab="Evento", 
     border="blue", 
     col="green", 
     xlim=c(-1,2), 
     prob = TRUE)
lines(x, dunif(x), col='red', lwd = 1)
lines(x, punif(x), col='blue', lwd=3)
abline(h=(a+b)/2, lwd=2)
abline(v=(b-a)^2/12, col='yellow',lwd=2)
#legend(x = "right", legend = c("PDF de x", "CDF de  x", 'CDF Inversa  de x',
 #                              'Media', 'Varianza'),
  #     fill = c("green", "red", 'blue','black','yellow'), 
   #    title = "Distribuciones")
#dev.off()




########## Distribución Exponencial ##########
x <- seq(0, 10 , 0.01)
#png("expo.png")
de<-dexp(x, rate =1)
pe<-pexp(x, rate = 1)
qe<-qexp(x, rate = 1)
plot(x, de, type = "l", main = "PDF de x",  col="green",lwd=2) 
lines(x, pe, type = "l", main = "CDF de  x", col='red', lwd=2)
prob <- seq(0, 10, 0.01)
lines(prob, qe, type = "l", main = "CDF Inversa  de x", col='blue', lwd=2)
abline(h=1, lwd=2)
abline(v=1, col='yellow',lwd=2)
legend(x = "right", legend = c("PDF de x", "CDF de  x", 'CDF Inversa  de x',
                               'Media', 'Varianza'),
       fill = c("green", "red", 'blue', 'black','yellow'), 
       title = "Distribuciones")
#dev.off()



x<-rexp(1000, rate = 0.5)
hist(x, main="Histograma Distribución Uniforme", 
     xlab="Evento", 
     border="blue", 
     col="green", 
     xlim=c(0,max(x)), 
     prob = TRUE)
lines(x, pexp(x, 0.5), col='red', lwd = 4, type = "l")
lines(x, punif(x), col='blue', lwd=3)




