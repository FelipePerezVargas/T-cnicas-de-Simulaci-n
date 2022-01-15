# Distribuci?n uniforme
a<-1
b<-5 
# Funci?n de densidad
fd<-1/(b-a);fd
plot(a,fd, xlim=c(0,6))
points(b,fd)
#Distribuci?n acumulada
#x<-runif(10, min=a-1, max=b+1) 
x <- seq(a, b , 0.01)
fp<-(x-a)/(b-a)
plot(x,fp, type = "l")


# Distribucipon exponencial
l<-0.5
x<-runif(10000000, min=a, max=b) 
hist(x)
x<-rexp(10000000, min=a, max=b) 
plot(x)
fd<-(1/l)*exp(-x/l)
plot(fd,x,  col='red')

#Distribuci?n Acumulada
da<-1-exp(-x/l)
plot(da,x)

# Distribuci?n de densidad Weibull
A<-3
B<-1
a<-0
b<-4
x<-runif(100, min=a, max=b);x 
dw<-A*B^(-A)*x^(A-1)*exp(-x/B)^(A);dw
#dw<-(A/B)*(x/B)*x^(A-1)*exp(-x/B)^(A);dw
plot(x,dw, col='blue')
# Distribuci?n Acumulada

daw<-1-exp(-x/B)^(A)
plot(x,daw)

#Distribuci?n triangular
a<-
b<-
c<-  
x<-  




NB <- function(N){
  x <- 2 *runif(N)-1
  y <- 2 *runif (N)-1
  h <- sum(x**2+y**2<=1)
  return(4*h/N)
}
NB(100000000)




####################################################################
##                                                                ##
##                    DISTRIBUCIONES CONTINUAS                    ##
##                                                                ##
####################################################################

# Distribuci?n Uniforme
x<-runif(100)
dunif(x, min = 0, max = 1, log = FALSE)
punif(q, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)
qunif(p, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE)
runif(n, min = 0, max = 1)

x<-runif(40, min = -1, max = 1)
dunif(x, min=0, max=1, log = F)

x <- seq(0, 100, by = 1)   
y <- dunif(x, min = 10, max = 50) 
plot(y, type = "o")  

ya<-unif(4, min = 10, max = 50)


# Example of parameter estimation for a distribution with
# unknown parameters currently been sought after.




# Distribuci?n beta 
# Distribuci?n binomial 
# Distribuci?n Cauchy
# Distribuci?n Chi-Square
# Distriuci?n Exponencial
# Distribuci?n F
# Distribuci?n Gamma
# Distribuci?n Geom?trica
# Distribuci?n hipergeom?trica
# Distribuci?n Log-Normal
# Distribuci?n Logistica
# Distribuci?n Normal
# Distribuci?n Poisson
# Distribuci?n Student
# Distribuci?n Uniforme
# Distribuci?n Weibull





