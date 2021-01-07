#Cargando datos
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
hist_confirmed = hist(confirmed,breaks="Sturges",freq=FALSE)
hist_deaths = hist(deaths,breaks="Sturges",freq=FALSE)
hist_recovered = hist(recovered,breaks="Sturges",freq=FALSE)
hist_tests = hist(tests,breaks="Sturges",freq=FALSE)

#Ojivas


#Diagramas de cajas (5 puntos)
box_confirmed = boxplot(confirmed,xlab="Casos Confirmados")
box_deaths = boxplot(deaths,xlab="Muertes")
box_recovered = boxplot(recovered,xlab="Recuperados")
box_tests = boxplot(tests,xlab="Tests realizados")
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
#Diagramas circulares (5 puntos)

#2.	(40 Puntos) Estadística descriptiva bivariante o multivariante: 
#Para las variables cualitativas (10 puntos): 

#Para cada variable cuantitativa presente un diagrama de cajas segmentado por al menos una variable cualitativa. Ej: Diagrama de caja de Edad segmentado por Sexo
#confirmed vs stay home restrictions
confirmed_V_stayHome = boxplot(data$confirmed ~ data$stay_home_restrictions,horizontal=T)
#deaths vs international movement restrictions
deaths_V_international = boxplot(data$deaths ~ data$internal_movement_restrictions,horizontal=T)
#recovered vs school closing
recovered_V_schoolC = boxplot(data$recovered ~ data$school_closing,horizontal=T)
#tests vs international movement
tests_V_international = boxplot(data$tests ~ data$internal_movement_restrictions,horizontal=T)

#Para las variables cuantitativas:
#Matriz de correlación y matriz de covarianzas (5 puntos)
#Matriz gráfica de correlación (5 puntos)
datos_cuantitativos = data.frame(data$confirmed,data$recovered,data$deaths,data$tests)
matriz_correlacion = cor(datos_cuantitativos)
print(matriz_correlacion)
#Intervalo de confianza para la media de una de las variables cuantitativas. (10 puntos)
#Prueba de hipótesis para establecer si existe diferencia entre la proporción (media) de una de las variables cuantitativas según una de las variables cualitativas. (10 puntos)




