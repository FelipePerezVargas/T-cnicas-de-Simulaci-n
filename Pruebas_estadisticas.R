########################################################
####                                                ####
####           Pruebas Estad?sticas                 ####
####                                                ####
########################################################
rm(list = ls())
#### Clase 24/09/2020 ####
## Pruebas estad?sticas en la generaci?n  de 
## n?meros aleatorios

#### Prueba de Medias ####
#Realice la prueba de medias a los primeros 30 n?meros aleatorios entre 0 y 1
#de un generador congruencial, con un nivel de confianza del 95%, loa n?meros siguientes:
x<-c(0.03991,0.10461,0.93716,0.16894,0.98953,0.73231,
     0.25593,0.34565,0.02345,0.67347,0.10987,0.25678,
     0.71890,0.61234,0.86322,0.94134,0.99872,0.27657,
     0.82324,0.12387,0.05389,0.82474,0.59289,0.36782,
     0.72484,0.48999,0.50502,0.39528,0.36782,0.90234)
#### Media ####
mean(x)

#### L?mite Inferior ####
n<-length(x) # N?mero de observaciones
lsx<-1/2+1.96*(1/(12*sqrt(n)));lsx           #con z_{alpha/2}=1.96 
#### L?mite Superior ####
lix<-1/2-1.96*(1/(12*sqrt(n)));lix
#### Resultado ####
pm<-c(lix,mean(x), lsx)
if(lix<=mean(x) & lsx>=mean(x)){cat("Se Acepta la Hip?tesis")}else{cat("Se Rechaza la Hip?tesis")}
#### Dado el valor medio se encuentra dentro de los l?mites de aceptaci?n.
#### Se acepta la hip?tesis H_{o}

#### Prueba de Varianza ####
#Realice la prueba de varianza de los siguientes 30 n?meros con un nivel de confianza
# de 97.5%
rm(list = ls())
x<-c(0.72484,0.48999,0.50502,0.39528,0.36782,0.90234,
     0.71890,0.61234,0.86322,0.94134,0.99872,0.27657,
     0.34565,0.02345,0.67347,0.10987,0.25678,0.25593,
     0.82345,0.12387,0.05389,0.82474,0.59289,0.36782,
     0.03991,0.10461,0.93716,0.16894,0.98953,0.73231)

#### Varianza 
n<-length(x)
var(x)

#### L?mite Inferior ####
lsv<- 45.7/(12*(n-1));lsv   #chi cuadrada con 0.025  y gl=29
#### L?mite Superior ####
liv<- 16.4/(12*(n-1));liv   #chi cuadrada con 0.975 y gl=29
#### Resultado ####
pv<-c(liv, var(x), lsv);pv
if(liv<=var(x) & lsv>=var(x)){cat("Se Acepta la Hip?tesis")} else{cat("Se Rechaza la Hip?tesis")}
#### Dado el valor medio se encuentra dentro de los l?mites de aceptaci?n.
#### Se acepta la hip?tesis H_{o}

#########################
#### Prueba de Forma ####
#########################

## Tomando los n?meros del ejemplo anterior, determine con un nivel de confianza
## del 95% si pertenecen a una poblaci?n uniforme. Dividiendo el rango de 0 a 1 
## en 10 intervalos y clasificando los 30 n?meros seg?n su valor.

### Hacer un histograma de frecuencias
rm(list = ls())
x<-c(0.72484,0.48999,0.50502,0.39528,0.36782,0.90234,
     0.71890,0.61234,0.86322,0.94134,0.99872,0.27657,
     0.34565,0.02345,0.67347,0.10987,0.25678,0.25593,
     0.82345,0.12387,0.05389,0.82474,0.59289,0.36782,
     0.03991,0.10461,0.93716,0.16894,0.98953,0.73231)
h<-hist(x, breaks = 10) #
FO<-h$counts;FO
h$breaks
bin<-c('0.0-0.1','0.1,0.2','0.2-0.3','0.3-0.4','0.4-0.5','0.5-0.6',
       '0.6-0.7','0.7,0.8','0.8,0.9','0.9-1.0')

FO<-as.numeric(FO)
n<-length(x)
a<-0
b<-10
FE<-(n-a)/(b-a)   

C<-c()
for(i in 1:10){
        C[i]<-(FE-FO[i])^2/FE
}
tabla<-cbind(bin,FO, FE, C)
C<-sum(C)
chi2<-qchisq(0.95,29)
if(C<=chi2){cat("Se Acepta la Hip?tesis")} else{cat("Se Rechaza la Hip?tesis")}


### Considerando 9 grados de libertad y nivel de confidencia del 95% tenemos que el 
### valor de chi cuadrado es de 16.9 se acepta la hip?tesis de uniformidad






#################################
#### Prueba de Independencia ####
#################################

#La prueba de independencia consiste en demostrar que los n?meros generados 
#son estad?sticamente independiente entre s?, esto es, que no depende 
#uno de otro 
#
# Ho:ri~Independiente
# Hi:ri~Dependiente

#  Para realizar esta prueba de hip?tesis existen los siguientes m?todos
#\item Poker 
#\item Corridas abajo y arriba
#\item Abajo y arriba de la media
#\item Longitud de las corridas
#\item Distancia
#\item Series
#\item Huecos
#

#####################
## Prueba de P?ker ##
#####################

#### Realiza la prueba de hip?tesis para saber si existe independencia de los 
#### siguientes n?meros aleatorios
rm(list = ls())
x<-c(0.72484,0.48999,0.50502,0.39528,0.36782,0.90234,
     0.71890,0.61234,0.86322,0.94134,0.99872,0.27657,
     0.34565,0.02345,0.67347,0.10987,0.25678,0.25593,
     0.82345,0.12387,0.05389,0.82474,0.59289,0.36782,
     0.03991,0.10461,0.93716,0.16894,0.98953,0.73231)

#### Paso 1 
#### Calcular la probabilidad esperadas para un juego de p?ker con 5 cartas 
#### numeradas del 0 al 9 con reemplazo, se tiene 7 eventos o intervalos
#### con las siguientes probabilidades

dif<-0.3024 #Todos los n?meros diferentes
par<-0.504 # Un par de n?meros iguales
dospar<-0.108 # Dos  n?meros iguales
ter<-0.072  # Salga tres n?meros iguales
full<-0.009  # Tres de un mismo n?mero y un par de otros
poker<-0.0045 # Cuatro n?meros iguales
qui<-0.0001 #Todos los n?meros sean iguales

#### Paso 2
#### Calcular las frecuencias esperadas de cada uno de los eventos (FEi) multiplicando 
#### la probabilidad de cada evento por el n?mero de  n?meros aleatorios generados
Probabilidades<-c("Diferente","Par","Dospares","Tercia","Full","P?ker","Quintilla")
PE<-c(dif,par,dospar,ter,full,poker,qui)
n<-length(x)
FE<-PE*n
#tabla<-cbind(Probabilidades,FE)
#View(tabla)
#### Paso 3
#### Para cada n?mero aleatorio generado verificar y clasificar seg?n las 
#### probabilidades indicadas anteriormente clasificandose como sigue
#### 0.03408 Es par
#### 0.44343 Es full por ejemplo
#### Con las clasificaci?n se genera una tabla de frecuencia observada (FOi)
####
#### Calcular el estad?stico C con m=7
#### Falta hacer el algoritmo para que compare los n?meros y clasifique en un histograma 
FO<-c(14,15,1,1,0,0,0)
#C<-c(length(FO))
for(i in 1:7){
    C[i]<-(FE[i]-FO[i])^2/FE[i]
    }
tablas<-cbind(Probabilidades,FO,FE, C)
View(tablas)
C<-sum(C)
#### Si el valor de C es menor o igual al estad?sticos de la table chi cuadrada
#### con 6 grados de libertad y una probabilidad de rechazo alfa
####
chi2<-qchisq(0.95,6)
if(C<=chi2){cat("Se Acepta la Hip?tesis")} else{cat("Se Rechaza la Hip?tesis")}

############################
#### Prueba de Corridas #### (Arriba-Abajo)
############################

#### Determine si la siguiente secuencia de 20 n?meros puede ser aceptada como
#### independiente con un nivel de confianza del 95% usando la prueba de corridas
rm(list = ls())
x<-c(0.43,0.28,0.33,0.27,0.12,0.31,0.42,0.01,0.32,0.45,
     0.98,0.79,0.99,0.55,0.67,0.74,0.16,0.20,0.12,0.58)
#### Paso 1
#### Clasificar cada n?mero aleatorio con respecto al anterior, de acuerdo con:
#### si ri<=r_{i-1} ri=-
#### si ri>r_{i-1} ri=+
#### Ejemplo 
####  ri=0.28 r_{i-1}=0.43  0.28<=43='-'   
r<-c(length(x))
y<-as.matrix(x, ncol=1);y
for (i in 1:length(y)-1){
    if(y[i+1,1]<=y[i,1])
    {
        r[i]<-'1'    
    }     
    else{
        r[i]<-'0'
    }
    
}
r

#### Paso 2
#### Calcular el n?mero de corridas observadas h, una corrida se forma por un 
#### conjunto de n?meros aleatorios del mismo signo
plot(r, col='blue')
# h=14 corridas
#### PAso 3 
#### Calcular E(h) y V(h) de acuerdo con las siguientes relaciones
#### E(h)<-(2*n-1)/3
Eh<-(2*length(x)-1)/3
#### V(h)<-(16*n-29)/90
Vh<-(16*length(x)-29)/90
#### Donde n=30 es el n?mero de datos generados
#### Paso 4
#### Calcular el estad?stico Z<-(h-E(h))/(sqrt(V(h))), si es menor que el valor 
#### cr?tico de Z_{alpha/2} se acepta la hip?tesis de independencia
h<-14
z<-(h-Eh)/sqrt(Vh)
z1=qnorm(0.975)
if(z<=z1){cat("Se Acepta la Hip?tesis")} else{cat("Se Rechaza la Hip?tesis")}

##########################
#### Prueba de Series ####
##########################

#### Realice la prueba de series a los siguientes 30 n?meros con un nivel de confianza
#### del 95%
rm(list = ls())
x<-c(0.72484,0.48999,0.50502,0.39528,0.36782,0.90234,
     0.71890,0.61234,0.86322,0.94134,0.99872,0.27657,
     0.34565,0.02345,0.67347,0.10987,0.25678,0.25593,
     0.82345,0.12387,0.05389,0.82474,0.59289,0.36782,
     0.03991,0.10461,0.93716,0.16894,0.98953,0.73231)
#### Paso 1 ####
#### Crear un histograma de dos dimensiones con m intervalos, clasificando
#### cada pareja de n?meros consecutivos (r_{i}, r_{i+1}) dentro de las casillas 
#### de dicho histograma de frecuencias. El n?mero total de pares ordenados 
#### en cada casilla formar? la frecuencia observada (FO_{i})
a<-c(length(x))
b<-c(length(x))
mq<-matrix(x,nrow=length(x),ncol=1)
for (i in 1:length(x)-1) {
    a[i]<- mq[i]
    b[i]<-mq[i+1]
}
pares<-cbind(a,b)
View(pares)
plot(a,b, ylim = c(0,1), xlim = c(0,1))
grid(4, 4,lty = 6, col = "cornsilk2")

##### Del grafico se puede obtener lo siguiente 
#-----------------
#|   |   |   |   |
#| 3 | 2 | 1 | 2 |
#|   |   |   |   |
#-----------------
#|   |   |   |   |
#| 1 | 1 | 1 | 3 |
#|   |   |   |   |
#-----------------
#|   |   |   |   |
#| 1 | 3 | 3 | 1 |
#|   |   |   |   |
#-----------------
#|   |   |   |   |
#| 2 | 2 | 1 | 2 |
#|   |   |   |   |
#----------------- 
conteo<-c(3,2,1,2,1,1,1,3,1,3,3,1,2,2,1,2)
#### Paso 2 ####
#### Calcular la frecuencia esperada en cada casilla de FE_{i} de acuerdo con 
#### FE_{i}=n?m/m donde n?m es el n?mero total de parejas ordenadas 
#### m es el n?mero de cuadrantes 4x4=16
FE<-length(x)/16
#### De la gr?fica podemos definir como 
#tabla_serie<-cbind(conteo, FO, c(FE, FE, FE))
#View(tabla_serie)
#### Paso 3 
#### Calculamos el valor de C con la formula de chi^{2} y si  
#### C<=chi^{2} aceptamos o rechazamos la hip?tesis de series
C<-c()
for(i in 1:length(conteo)){
C[i]<-sum(FE-conteo[i])^{2}/FE
}
C<-sum(C)
#El valor de chi2 con un 95% de confianza y con 15 grados de libertad
chi2<-qchisq(0.95,15)
if(C<=chi2){cat("Se Acepta la Hip?tesis")} else{cat("Se Rechaza la Hip?tesis")}




##########################
#### Prueba de Huecos #### 
##########################

#### Haga la prueba de huecos con los siguientes n?meros aleatorios
rm(list = ls())
x<-c(0.72484,0.48999,0.50502,0.39528,0.36782,0.90234,
     0.71890,0.61234,0.86322,0.94134,0.99872,0.27657,
     0.34565,0.02345,0.67347,0.10987,0.25678,0.25593,
     0.82345,0.12387,0.05389,0.82474,0.59289,0.36782,
     0.03991,0.10461,0.93716,0.16894,0.98953,0.73231)
#### Paso 1

#### Seleccionar un alpha y beta para poder realizar por 
####

alpha<-0.3 # min()
beta<-0.8   #max()  0.5
#### Paso 2 se clasifican los n?meros seg?n el intervalos seleccionado
#### los que est?n dentro del intervalo seleccionado se le pone un
#### 1 y los que este por abajo o arriba se les pone un 0 ####

d<-c(length(x))
for(i in 1:length(x)){
    if(x[i]>alpha& x[i]<beta){
        d[i]<-'1'
    }
        else{ 
        d[i]<-'0'
    }
}
d
#### Paso 3 ####
#### Ahora debemos clasificar el orden o tama?o de hueco
#### 11 es un hueco de orden 0
#### 101  es un hueco de orden 1
#### 1001 es un hueco de orden 2 
#### Calcular la frecuencia observada de los huecos
hueco<-c(0,1,2,3,4,5,6,7)
FO<-c(6,2,0,0,1,1,0,1)
tabla<-cbind(hueco, FO)
View(tabla)
#### Paso 4 ####
#### Calcular el n?mero de corridas observadas h, una corrida se forma por ceros 
#### y unos consecutivos y se clasifican seg?n el n?mero de ceros  
 h<-sum(FO)
#### El n?mero h=11

#### Paso 5  
#### Se calcula la frecuencia esperada (FE_{i})
####
FE<-c(length(hueco))
 for(i in 0:length(hueco)){
FE[i]<-h*(beta-alpha)*(1-(beta-alpha))^{i-1}
}
FE<-sum(FE)

C<-sum((FE-FO)^{2}/FE)
chi2<-qchisq(0.95,6)
if(C<=chi2){cat("Se Acepta la Hip?tesis")} else{cat("Se Rechaza la Hip?tesis")}



####################################################
#### Prueba de Corrida Arriba-Abajo de la media ####
####################################################

#### Determine si la siguiente secuencia de n?meros puede ser 
#### aceptada como independiente con un nivel de confianza de
#### 95%, usando la prueba de corridas Arriba-Abajo de la media

rm(list = ls())
x<-c(0.43,0.28,0.33,0.27,0.12,0.31,0.42,0.01,0.32,0.45,
     0.98,0.79,0.99,0.55,0.67,0.74,0.16,0.20,0.12,0.58)
med<-0.5  ### Por prueba de medias
#med<-mean(x) ### Realizar la prueba tomando en cuenta la media de 
# los datos 
#### Paso 1
#### Calcular los los valores de de la manera siguiente
#### Si r_{i}<=0.5  -> 0
#### Si r_{i}>0.5  -> 1
r<-c(length(x))
for (i in 1:length(x)) {
    if(x[i]<=0.5)
    {
        r[i]<-'0'    
    }     
    else{
        r[i]<-'1'
    }
    
}
r
#### Paso 2 ####
#### Calculamos el n?mero de corridas h
#### Una corrida son las sucesiones de cero y de uno existentes
#### en la serie
plot(r) # Esto es m?s f?cil que contar
####
####O se puede hacer el conteo de ceros las series de manera manual
h<-4
#### Paso 3
#### Calcular  E(h) y V(h) con las siguientes expresiones
#### Con n0= al n?mero de ceros existentes en la serie
#### Con n1= al n?mero de unos existentes en la serie
#### Con n= al n?mero de datos existentes en la serie
#### Del grafico o conteo podemos saber que
n0<-7
n1<-13
n<-n0+n1
Eh<-(2*n0*n1/n)+1/2
Vh<-(2*n0*n1*(2*n0*n1-n))/(n^{2}*(n-1))


#### Paso 4 
#### Una vez calculados E(h), V(h) y h se calcula el z observado
#### con la siguiente expresi?n 

zo<-abs((h-Eh)/(sqrt(Vh)))

####Paso 5 
#### Ahora se hace el comparativo para saber si la hip?tesis 
#### es aceptada 
z1=qnorm(0.95)
if(zo<=z1){cat("Se Acepta la Hip?tesis")} else{cat("Se Rechaza la Hip?tesis")}










#### Tarea Pendiente ####
x<-c(0.234,0.907,0.800,0.456,0.002,0.963,0.678,0.345,0.255,0.789,
     0.607,0.982,0.897,0.045,0.123,0.951,0.783,0.345,0.234,0.405,
     0.456,0.380,0.899,0.479,0.404,0.277,0.895,0.678,0.341)
a<-substr(x, star=3, stop=3);a
b<-substr(x, star=4, stop=4);b
a<-as.numeric(a);a
a[is.na(a)] <- 0 
b<-as.numeric(b);b
b[is.na(b)] <- 0
r<-c(length(x))
for(i in 1:length(x)){
    if(a[i]<=b[i])
    {
        r[i]<-'+'    
    }     
    else{
        r[i]<-'-'
    }
}
r

#### Selecci?n de posici?n de un n?mero ####

rm(list = ls())
x<-c(0.43,0.28,0.33,0.27,0.12,0.31,0.42,0.01,0.32,0.45,
     0.98,0.79,0.99,0.55,0.67,0.74,0.16,0.20,0.12,0.58)
a<-substr(x, star=3, stop=3);a
b<-substr(x, star=4, stop=4);b
a<-as.numeric(a);a
a[is.na(a)] <- 0 
b<-as.numeric(b);b
b[is.na(b)] <- 0 
r<-c(length(x))
for(i in 1:length(x)){
    if(a[i]<=b[i])
    {
        r[i]<-'-'    
    }     
    else{
        r[i]<-'+'
    }
}
r

