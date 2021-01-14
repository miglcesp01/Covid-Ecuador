setwd("C:/Users/Lenovo/OneDrive - Escuela Superior Politécnica del Litoral/Estadística/Proyecto/Covid-Ecuador")
#Cargando datos
library(corrplot)
library(modeest)
library(COVID19)
data <- covid19(country="Ecuador")

#Creando funciones
curtosis<-function(data){
  data=na.omit(data)
  P75 = quantile(data,prob=0.75)
  P25 = quantile(data,prob=0.25)
  P90 = quantile(data,prob=0.90)
  P10 = quantile(data,prob=0.10)
  numerador = P75-P25
  denominador = 2*(P90-P10)
  numerador/denominador
}
sesgo<-function(data){
  data=na.omit(data)
  media = mean(data)
  moda = mlv(data,method="mfv")
  sd = sd(data)
  numerador = 3*(media-moda)
  numerador/sd
}


#Escogiendo variables cuantitativas
confirmed = na.omit(data$confirmed)
deaths = na.omit(data$deaths)
recovered = na.omit(data$recovered)
tests = na.omit(data$tests)

#Escogiendo variables cualitativas
school_closing = na.omit(data$school_closing)
stay_home_restrictions = na.omit(data$stay_home_restrictions)
international_movement_restrictions = na.omit(data$internal_movement_restrictions)

#1.	(30 Puntos) Estadística descriptiva univariante: 
#Para las variables cuantitativas:

#Histogramas, ojivas (5 puntos): Falta las Ojivas
#Histogramas
hist_confirmed = hist(confirmed,breaks="Sturges",freq=FALSE,main="Histograma de Casos Confirmados")
hist_deaths = hist(deaths,breaks="Sturges",freq=FALSE,main="Histograma de casos Fallecidos")
hist_recovered = hist(recovered,breaks="Sturges",freq=FALSE,main="Histograma de Casos Recuperados")
hist_tests = hist(tests,breaks="Sturges",freq=FALSE,main="Histograma de Casos Testeados")

#Ojivas
p = c(0.05, 0.25, 0.5, 0.75, 0.95, 1)
valores_confirmed = quantile(confirmed,probs = p)
ojiva_confirmed = plot(valores_confirmed,p,main="Ojiva de Casos Confirmados",xlab="Casos Confirmados",ylab="Frecuencia Relativa Acumulada")
lines(valores_confirmed,p)
valores_deaths = quantile(deaths,probs=p)
ojiva_deaths = plot(valores_deaths,p,main="Ojiva de Casos Fallecidos",xlab="Casos Fallecidos",ylab="Frecuencia Relativa Acumulada")
lines(valores_deaths,p)
valores_recovered = quantile(recovered,probs=p)
ojiva_recovered = plot(valores_recovered,p,main="Ojiva de casos Recuperados",xlab="Casos Recuperados",ylab="Frecuencia Relativa Acumulada")
lines(valores_recovered,p)
valores_tests = quantile(tests,probs=p)
ojiva_tests = plot(valores_tests,p,main="Ojiva de Casos Testeados",xlab="Casos Testeados",ylab="Frecuencia Relativa Acumulada")
lines(valores_tests,p)
#Diagramas de cajas (5 puntos)
box_confirmed = boxplot(confirmed,xlab="Casos Confirmados",main="Diagrama de Cajas de Casos Confirmados")
box_deaths = boxplot(deaths,xlab="Casos Fallecidos",main="Diagrama de Cajas de Casos Fallecidos")
box_recovered = boxplot(recovered,xlab="Casos Recuperados",main="Diagrama de Cajas de Casos Recuperados")
box_tests = boxplot(tests,xlab="Tests realizados",main="Diagrama de Cajas de Casos Testeados")
#Medidas estadísticas (media, mediana, desviación estándar, cuartiles, curtosis, sesgo) (5 puntos)
#Variable Confirmed
media_confirmed = mean(confirmed)
mediana_confirmed = median(confirmed)
sd_confirmed = sd(confirmed)
cuartiles_confirmed = quantile(confirmed,prob=c(0,0.25,0.5,0.75,1))
curtosis_confirmed = curtosis(data$confirmed)
sesgo_confirmed = sesgo(data$confirmed)
#variable deaths
media_deaths = mean(deaths)
mediana_deaths = median(deaths)
sd_deaths = sd(deaths)
cuartiles_deaths = quantile(deaths,prob=c(0,0.25,0.50,0.75,1))
curtosis_deaths = curtosis(data$deaths)
sesgo_deaths = sesgo(data$deaths)
#variable recovered
media_recovered = mean(recovered)
mediana_recovered = median(recovered)
sd_recovered = sd(recovered)
cuartiles_recovered = quantile(recovered,prob=c(0,0.25,0.50,0.75,1))
curtosis_recovered = curtosis(data$recovered)
sesgp_recovered = sesgo(data$recovered)
#variable tests
media_tests = mean(tests)
mediana_tests = median(tests)
sd_tests = sd(tests)
cuartiles_tests = quantile(tests,prob=c(0,0.25,0.50,0.75,1))
curtosis_tests = curtosis(data$confirmed)
sesgo_tests = sesgo(data$tests)

#Tablas de datos agrupados (5 puntos)
agrup_confirmed = summary(confirmed)
agrup_deaths = summary(deaths)
agrup_recovered = summary(recovered)
agrup_tests = summary(tests)

#Para las variables cualitativas:
#Diagrama de barras (5 puntos)
dgr_school_closing = barplot(table(school_closing),main="Diagrama de barras School Closing",
                             xlab="Categorias")
dgr_stay_home = barplot(table(stay_home_restrictions),main="Diagrama de barras Stay Home Restrictions",
                        xlab="Categorias")
dgr_international_movement = barplot(table(international_movement_restrictions),main="Diagrama de barras International Movement Restrictions",
                                     xlab="Categorias")
#Diagramas circulares (5 puntos):FAltan diagramas circulares
#School closing
cantidades_school_closing = c(sum(school_closing==0),sum(school_closing==1),
                              sum(school_closing==2),sum(school_closing==3))
piepercent_school_closing = round(100*cantidades_school_closing/sum(cantidades_school_closing),2)
etiquetas = c("Nivel 0","Nivel 1","Nivel 2","Nivel 3")
pie(cantidades_school_closing,piepercent_school_closing,
    main="%School Closing Measures Pie Chart",col=rainbow(4))
legend("topleft",etiquetas,fill=rainbow(4))
#Stay home restrictions
cantidades_stay_home = c(sum(stay_home_restrictions==0),sum(stay_home_restrictions==1),
                         sum(stay_home_restrictions==2),sum(stay_home_restrictions==3))
piepercent_stay_home = round(100*cantidades_stay_home/sum(cantidades_stay_home),2)
pie(cantidades_stay_home,piepercent_stay_home,
    main="%Stay Home Restrictions Measures",col=rainbow(4))
legend("topleft",etiquetas,fill=rainbow(4))
#international movement restrictions
etiquetas2 = c("Nivel0","Nivel1","Nivel2","Nivel3","Nivel4")
cantidades_international = c(sum(international_movement_restrictions==0),sum(international_movement_restrictions==1),
                             sum(international_movement_restrictions==2),sum(international_movement_restrictions==3))
piepercent_international = round(100*cantidades_international/sum(cantidades_international),2)
pie(cantidades_international,piepercent_international,
    main="%International Restrictions Measures Pie Chart%",col=rainbow(4))
legend("topleft",etiquetas2,fill=rainbow(4))

#2.	(40 Puntos) Estadística descriptiva bivariante o multivariante: 
#Para las variables cualitativas (10 puntos): 

#Para cada variable cuantitativa presente un diagrama de cajas segmentado por al menos una variable cualitativa. Ej: Diagrama de caja de Edad segmentado por Sexo
#confirmed vs stay home restrictions
confirmed_V_stayHome = boxplot(data$confirmed ~ data$stay_home_restrictions,horizontal=T,main="Casos Confirmado vs Stay Home Measures",xlab="Solo hay valores para el 3 de school closing")

#deaths vs international movement restrictions
deaths_V_international = boxplot(data$deaths ~ data$internal_movement_restrictions,horizontal=T,main="Casos Fallecidos vs International Movement Restrictions")
#recovered vs school closing
recovered_V_schoolC = boxplot(data$recovered ~ data$school_closing,horizontal=T,main="Casos Recuperados vs School Closing Measures")
#tests vs international movement
tests_V_international = boxplot(data$tests ~ data$internal_movement_restrictions,horizontal=T,main="Casos Testeados vs International Movement Restrictions")

#Para las variables cuantitativas:
#Matriz de correlación y matriz de covarianzas (5 puntos)
datos_cuantitativos = data.frame("Casos Confirmados" = na.omit(data$confirmed)[0:288],
                                 "Casos Recuperados" = na.omit(data$recovered)[0:288],
                                 "Casos Fallecidos" = na.omit(data$deaths)[0:288],
                                 "Casos Testeados" = na.omit(data$tests)[0:288])

matriz_correlacion = cor(datos_cuantitativos)
matriz_covarianza = cov(datos_cuantitativos)
print("MATRIZ DE CORRELACION:")
print(matriz_correlacion)
print("MATRIZ DE COVARIANZA")
print(matriz_covarianza)
#Matriz gráfica de correlación (5 puntos)
pairs(datos_cuantitativos,
      labels=c("confirmed","recovered","deaths","tests"),
      pch=21,
      main="MATRIZ GRÁFICA DE CORRELACIÓN",
      font.labels = 1)
#Intervalo de confianza para la media de una de las variables cuantitativas. (10 puntos)
print("INTERVALO DE CONFIANZA")
intervalos =t.test(x=confirmed,conf.level=0.95)$conf.int
print(intervalos)
#Prueba de hipótesis para establecer si existe diferencia entre la proporción (media) de una de las variables cuantitativas según una de las variables cualitativas. (10 puntos)
#H1: La proporción de casos confirmados de coronavirus durante un período
#    con medidas de school closing igual a 3 es mayor a 0.8
#H0_ La proporción de casos confirmados de coronavirus durante un período
#    con medidas de school closing igual a 3 es igual a 0.8
#H0: p<=0.8
#H1: p>0.8
cantidad = length(na.omit(confirmed[school_closing==3]))
total = length(na.omit(confirmed))
z = prop.test(cantidad,total,p=0.8,alternative = "greater",conf.level = 0.95)
print(z)
#p es <= 0.05 por lo que rechazamos la hipotesis nula



