#########################################################
###                                                   ###
###              Números Aleatorios                   ###
###                                                   ###
#########################################################

#r=random number generation
#d=density or probability mass function
#p=cumulative distribution
#q=quantiles


# Distribución Uniforme
r<-runif(5000, min=0, max=1)
hist(r)





#Distribución Exponencial 
r<-rexp(1000000, rate = 1)
#d<-dexp(r)
hist(r)
##lines(r,d)

#Distribución Weibull
x<-rweibull(10000, shape=1, scale=1)
hist(x)

#Distribución Triangular 
rm(list = ls())
library(extraDistr)
x <- rtriang(100, 5, 7, 6)
hist(x)

#Distribución Normal
x<-rnorm(50000, mean=0, sd=1)
hist(x)


#Distribución Lognormal
x<-rlnorm(1000, meanlog=0, sdlog=1)
hist(x)

#Distribución logistica 
x<-rlogis(1000, location=0, scale=1)
hist(x)

# Distribución chi cuadrada
x<-rchisq(500, df=15)
hist(x)

#Distribución beta 

x<-rbeta(500000, shape1=1, shape2=0.5)
hist(x)

#Distribución de Couchy
  x<-rcauchy(5, location=0, scale=100)
  hist(x)

#Distribución Binomial
x<-rbinom(1000, size=10, prob=0.5)
hist(x)

#Distribución Poisson
x<-rpois(100, lambda=2)
hist(x)

#Distribución Geométrica 
x<-rgeom(10000, prob=0.6)
hist(x)
