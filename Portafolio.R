## 
library(ggplot2)
library(plotly)

# Calcula el riego y el rendimiento de cada uno de los activos ####
# de la siguiente tabla


Stock<-c('Sta','Stb','Stc','Std','Ste','Stf')
prob<-c(15,15,20,30,10,10)
ra<-c(10,5,8,13,15,3)
rb<-c(7,8,-4,8,-5,12)
rc<-c(4,-3,8,12,10,17)
rd<-c(8,-8,14,15,12,-5)
re<-c(13,4,8,12,15,-4)
rf<-c(12,10,-6,4,10,15)  

tabla<-cbind(prob,ra,rb,rc,rd,re,rf);tabla

# Retornos esperados ####

Ea<-sum(prob*ra)/100
Eb<-sum(prob*rb)/100
Ec<-sum(prob*rc)/100
Ed<-sum(prob*rd)/100
Ee<-sum(prob*re)/100
Ef<-sum(prob*rf)/100  

RE<-c(Ea,Eb,Ec,Ed,Ee,Ef);round(RE,3)

# Calculamos la volatilidad ####
sa<-sqrt(sum((ra-Ea)^{2}*(prob/100)));sa
sb<-sqrt(sum((rb-Eb)^{2}*(prob/100)));sb
sc<-sqrt(sum((rc-Ec)^{2}*(prob/100)));sc
sd<-sqrt(sum((rd-Ed)^{2}*(prob/100)));sd
se<-sqrt(sum((re-Ee)^{2}*(prob/100)));se
sf<-sqrt(sum((rf-Ef)^{2}*(prob/100)));sf

DE<-c(sa,sb,sc,sd,se,sf);round(DE,3)

vr<-data.frame(Stocks=c('Sta','Stb','Stc','Std','Ste','Stf'),DE,RE)
# Realizamos un gráfico Riesgo-Rendimiento ####
p <- ggplot(vr,aes(DE,RE,label=Stocks,colour=DE))+geom_point()+
ggtitle("Riesgo vs Rendimiento")+xlab("Riesgo")+ ylab("Rendimiento")  
p + geom_text(check_overlap = TRUE, hjust = 0, nudge_x = 0.05)
p<-ggplotly(p);p

# Portafolio de inversión ####
w1<-seq(0,1,by=0.1)
w2<-1-w1
#Erp<-w1*Ea+w2*Eb;Erp
require(gtools)
require(MASS)
cov(ra/100,rb/100)


Stock<-c('Sta','Stb','Stc','Std','Ste','Stf')
Stock<-RE
a<-combinations(n=6, r=2, v=Stock, repeats=FALSE);a

Erp<-w1*Ea+w2*Eb;Erp
s<-sum(prob/100*(ra-Ea)*(rb-Eb));s
w<-cbind(w1,w2);w
cov_ab<-sum(p1*(sa-Ea)*(sb-Eb));s
cor(t1)
for(i in 1:length(a)){
 print(w*a[i])  
}


Stock<-c('Sta','Stb','Stc','Std','Ste','Stf')
prob<-c(15,15,20,30,10,10)
ra<-c(10,5,8,13,15,3)
rb<-c(7,8,-4,8,-5,12)
rc<-c(4,-3,8,12,10,17)
rd<-c(8,-8,14,15,12,-5)
re<-c(13,4,8,12,15,-4)
rf<-c(12,10,-6,4,10,15)  

tabla1<-cbind(ra,rb,rc,rd,re,rf);tabla1




p1<-c(0.3,0.4,0.3)
sa<-c(-20,5,40)
sb<-c(-5,10,15)
sc<-c(5,3,2)
t1<-cbind(sa,sb,sc)
Ea<-sum(p1*sa);Ea #Retornos esperados
Eb<-sum(p1*sb);Eb
Ec<-sum(p1*sc);Ec
xa<-sqrt(sum((sa-Ea)^{2}*(prob/100)));xa #DesviaciÃ³n estÃ¡ndar
xb<-sqrt(sum((sb-Eb)^{2}*(prob/100)));xb
xc<-sqrt(sum((sc-Ec)^{2}*(prob/100)));xc
s<-sum(p1*(sa-Ea)*(sb-Eb));s #Covarianza
cor(t1)


names1<-c('US','UK','France','Germany','Japan')
cm<-matrix(c(1.0000,0.5003,0.4389,0.3681,0.2663,
             0.5003,1.0000,0.5420,0.4265,0.3581,
             0.4389,0.5420,1.0000,0.6032,0.3923,
             0.3681,0.4265,0.6032,1.0000,0.3663,
             0.2663,0.3581,0.3923,0.3663,1.0000), 
ncol = 5, nrow = 5, byrow=TRUE);cm #Matriz de correlaciÃ³n


names1<-c('US','UK','France','Germany','Japan')
ret<-c(0.1355,0.1589,0.1519,0.1435,0.1497) #Retornos 
ds<-c(0.1535,0.2430,0.2324,0.2038,0.2298) # Volatilidad
rus<-0.1355
rja<-0.1497
dus<-0.1535
dja<-0.2298
rouj<-0.2663

wm<-seq(-1,2,by=0.1)
wus<-wm
wja<-(1-wus)  
Erp<-wus*rus+wja*rja;round(Erp,3)
varuj<-wus^{2}*dus^{2}+wja^{2}*dja^{2}+2*wus*wja*rouj*dus*dja;round(varuj,3)
stdusja<-sqrt(varuj);round(stdusja,3)

plot(varuj, Erp, xlab = 'Volatilidad', ylab ='Rendimiento', type = 'o')



# https://www.youtube.com/watch?v=3nplZHpF5dE
# https://www.youtube.com/watch?v=aveAPz5Cv7w

# Supuestos del modelo 
# Supuestos de la teoría de Markowitz: 
#   
#   Los inversores son racionales y se comportan de manera que maximicen su 
# utilidad con un determinado nivel de ingresos o dinero. 
# Los inversores tienen acceso gratuito a información justa y correcta sobre los rendimientos
# y el riesgo. 
# Los mercados son eficientes y absorben la información de manera rápida y perfecta. 
# Los inversores son reacios al riesgo y tratan de minimizar el riesgo y maximizar el rendimiento. 
# Los inversores basan sus decisiones en los rendimientos esperados y la varianza o desviación estándar de estos rendimientos de la media. 
# Los inversores eligen rendimientos más altos a rendimientos más bajos para un determinado nivel de riesgo. 
# Supuestos del modelo:
#   Los retornos siguen una distribución normal. 
# Una función de utilidad cuadrática representa las decisiones de los inversores. 
# 
# Diversificación de la teoría de Markowitz :
#   Markowitz postuló que la diversificación no solo debe apuntar a reducir el riesgo de un valor reduciendo su variabilidad o desviación estándar, sino también reduciendo la covarianza o el riesgo interactivo de dos o más valores en una cartera. Como por combinación de diferentes valores, es teóricamente posible tener un rango de riesgo que varía de cero a infinito.
# Frontera eficiente:
#   La frontera eficiente es el conjunto de carteras óptimas que ofrecen el rendimiento esperado más alto para un nivel de riesgo definido o el riesgo más bajo para un nivel dado de rendimiento esperado.