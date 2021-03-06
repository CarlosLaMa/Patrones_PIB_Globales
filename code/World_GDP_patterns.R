---
title: "Práctica2 TyCdV de los Datos"
author: "Carlos Lavado Mahia, Dionisio González Jiménez"
date: "06/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

Comenzamos cargando y preparando los datos:

```{r carga y preparación}
# Cargamos el dataset.
# NOTA: Si se copia este código, es necesario indicar el directorio del CSV.
country_profiles_og<-read.csv("country_profile_variables.csv", sep=",", check.names=FALSE)

# Seleccionamos las variables de interés.
country_profiles<-country_profiles_og[,c(1,2,4,5,7,8,9,10,11,12,13,14,15,24,25,35)]

# Renombramos las columnas para facilitar el trabajo con ellas.
names(country_profiles)[names(country_profiles) == "Population in thousands (2017)"] <- "population"
names(country_profiles)[names(country_profiles) == "Population density (per km2, 2017)"] <- "pop_dens"
names(country_profiles)[names(country_profiles) == "Population growth rate (average annual %)"] <- "pop_growth"
names(country_profiles)[names(country_profiles) == "Urban population (% of total population)"] <- "urban_pop"
names(country_profiles)[names(country_profiles) == "GDP: Gross domestic product (million current US$)"] <- "gdp"
names(country_profiles)[names(country_profiles) == "GDP growth rate (annual %, const. 2005 prices)"] <- "gdp_growth"
names(country_profiles)[names(country_profiles) == "GDP per capita (current US$)"] <- "gdp_xcap"
names(country_profiles)[names(country_profiles) == "Economy: Agriculture (% of GVA)"] <- "eco_agri"
names(country_profiles)[names(country_profiles) == "Economy: Industry (% of GVA)"] <- "eco_industry"
names(country_profiles)[names(country_profiles) == "Economy: Services and other activity (% of GVA)"] <- "eco_services"
names(country_profiles)[names(country_profiles) == "Employment: Agriculture (% of employed)"] <- "empl_agri"
names(country_profiles)[names(country_profiles) == "Employment: Industry (% of employed)"] <- "empl_industry"
names(country_profiles)[names(country_profiles) == "Employment: Services (% of employed)"] <- "empl_services"
names(country_profiles)[names(country_profiles) == "Education: Government expenditure (% of GDP)"] <- "educ_exp"

# Imprimimos un resumen inicial.
summary(country_profiles)

# Algunas columnas se han interpretado erróneamente.
# Convertimos las columnas que no se han interpretado con el tipo deseada.
country_profiles$Region<-as.factor(country_profiles$Region)
country_profiles$pop_growth<-as.numeric(country_profiles$pop_growth)
country_profiles$gdp_growth<-as.numeric(country_profiles$gdp_growth)
country_profiles$eco_agri<-as.numeric(country_profiles$eco_agri)
country_profiles$empl_agri<-as.numeric(country_profiles$empl_agri)
country_profiles$empl_industry<-as.numeric(country_profiles$empl_industry)
country_profiles$empl_services<-as.numeric(country_profiles$empl_services)
country_profiles$educ_exp<-as.numeric(country_profiles$educ_exp)

# Imprimimos de nuevo un resumen.
summary(country_profiles)
```

Realizamos un análisis para ver si tenemos valores iguales a 0:

```{r valores iguales a 0}

# Analizamos si tenemos valores cero en población
which(country_profiles$population == 0.0)
# Analizamos si tenemos valores cero en densidad de población
which(country_profiles$pop_dens == 0.0)
# Analizamos si tenemos valores cero en crecimiento de población
which(country_profiles$pop_growth == 0.0)
# Analizamos si tenemos valores cero en población urbana
which(country_profiles$urban_pop == 0.0)
# Analizamos si tenemos valores cero en el PIB
which(country_profiles$gdp == 0.0)
# Analizamos si tenemos valores cero en el crecimiento del PIB
which(country_profiles$gdp_growth == 0.0)
# Analizamos si tenemos valores cero en el PIB per cápita
which(country_profiles$gdp_xcap == 0.0)
# Analizamos si tenemos valores cero en el sector agricultura
which(country_profiles$eco_agri == 0.0)
# Analizamos si tenemos valores cero en el sector industria
which(country_profiles$eco_industry == 0.0)
# Analizamos si tenemos valores cero en el sector servicios
which(country_profiles$eco_services == 0.0)
# Analizamos si tenemos valores cero en empleo agricultura
which(country_profiles$empl_agri == 0.0)
# Analizamos si tenemos valores cero en empleo industria
which(country_profiles$empl_industry == 0.0)
# Analizamos si tenemos valores cero en empleo servicios
which(country_profiles$empl_services == 0.0)
# Analizamos si tenemos valores cero en educación
which(country_profiles$educ_exp == 0.0)
```

Observamos si tenemos valores vacíos:

```{r valores vacíos}

# Detectamos valores vacíos con is.na().
paste("Valores vacíos en población:")
which(is.na(country_profiles$population))
paste("Valores vacíos en densidad población:")
which(is.na(country_profiles$pop_dens))
paste("Valores vacíos en crecimiento de población:")
which(is.na(country_profiles$pop_growth))
paste("Valores vacíos en población urbana:")
which(is.na(country_profiles$urban_pop))
paste("Valores vacíos en PIB:")
which(is.na(country_profiles$gdp))
paste("Valores vacíos en crecimiento del PIB:")
which(is.na(country_profiles$gdp_growth))
paste("Valores vacíos en PIB per capita:")
which(is.na(country_profiles$gdp_xcap))
paste("Valores vacíos en sector agricultura:")
which(is.na(country_profiles$eco_agri))
paste("Valores vacíos en sector industria:")
which(is.na(country_profiles$eco_industry))
paste("Valores vacíos en sector servicios:")
which(is.na(country_profiles$eco_services))
paste("Valores vacíos en empleo agricultura:")
which(is.na(country_profiles$empl_agri))
paste("Valores vacíos en empleo industria:")
which(is.na(country_profiles$empl_industry))
paste("Valores vacíos en empleo servicios:")
which(is.na(country_profiles$empl_services))
paste("Valores vacíos en educación:")
which(is.na(country_profiles$educ_exp))
```

Observamos la presencia de valores extremos, para ver si son razonables:

```{r valores extremos}
# Comprobamos los valores extremos con boxplot_stats$out.
# Esta función devuelve los valores que están a más de 1.5 de distancia del IQR.
paste("Valores extremos de la población:")
boxplot.stats(country_profiles$population)$out
paste("Valores extremos de la densidad población:")
boxplot.stats(country_profiles$pop_dens)$out
paste("Valores extremos del crecimiento población:")
boxplot.stats(country_profiles$pop_growth)$out
paste("Valores extremos de la población urbana:")
boxplot.stats(country_profiles$urban_pop)$out
paste("Valores extremos del PIB:")
boxplot.stats(country_profiles$gdp)$out
paste("Valores extremos del PIB per capita:")
boxplot.stats(country_profiles$gdp_xcap)$out
paste("Valores extremos del crecimiento del PIB:")
boxplot.stats(country_profiles$gdp_growth)$out
paste("Valores extremos del sector agricultura:")
boxplot.stats(country_profiles$eco_agri)$out
paste("Valores extremos del sector industria::")
boxplot.stats(country_profiles$eco_industry)$out
paste("Valores extremos del sector servicios:")
boxplot.stats(country_profiles$eco_services)$out
paste("Valores extremos de empleabilidad en agricultura:")
boxplot.stats(country_profiles$empl_agri)$out
paste("Valores extremos de empleabilidad en industria:")
boxplot.stats(country_profiles$empl_industry)$out
paste("Valores extremos de empleabilidad en servicios:")
boxplot.stats(country_profiles$empl_services)$out
paste("Valores extremos de gasto en educacion:")
boxplot.stats(country_profiles$educ_exp)$out

# Tras una inspección visual, los valores faltantes parecen haberse marcado con -99.
# Procedemos a sustituirlos por NA.
country_profiles[country_profiles == -99.0] <- NA

# Para acabar de preparar el dataset antes de aplicar el algoritmo de imputación de NAs,
# extraemos temporalmente la columna country (más de 53 categorías).
country_profiles <- subset(country_profiles, select = -c(country))
```

Ahora, procedamos a aplicar un algoritmo de imputación para los valores faltantes:

```{r missForest}
# Cargamos librería.
library(missForest)

# Aplicamos algoritmo a los datos para limpiarlos.
country_profiles_clean <- missForest(country_profiles)
country_profiles <- country_profiles_clean$ximp

# Añadimos de nuevo la columna country.
country_profiles <- data.frame(country_profiles_og$country, country_profiles)

# Imprimimos resumen
summary(country_profiles)
```

Una vez realizado toda la limpieza se exportan dichos datos de interés a un nuevo archivo csv.

```{r fichero csv tras limpieza}

# Creamos el nuevo csv con los datos de interés (es necesario indicar la dirección deseada).
write.csv(country_profiles, "/country_profile_variables_modified.csv", row.names = FALSE)
```

A continuación, estudiaremos la normalidad de los datos:

```{r normalidad}
# Computamos el test de Shapiro-Wilk para estudiar la normalidad.
shapiro.test(country_profiles$population)
shapiro.test(country_profiles$pop_dens)
shapiro.test(country_profiles$pop_growth)
shapiro.test(country_profiles$urban_pop)
shapiro.test(country_profiles$gdp)
shapiro.test(country_profiles$gdp_growth)
shapiro.test(country_profiles$gdp_xcap)
shapiro.test(country_profiles$eco_agri)
shapiro.test(country_profiles$eco_industry)
shapiro.test(country_profiles$eco_services)
shapiro.test(country_profiles$empl_agri)
shapiro.test(country_profiles$empl_industry)
shapiro.test(country_profiles$empl_services)
shapiro.test(country_profiles$educ_exp)

# Pruebas adicionales con Kolmogorov-Smirnov.
ks.test(country_profiles$population, pnorm, mean(country_profiles$population), sd(country_profiles$population))
ks.test(country_profiles$pop_dens, pnorm, mean(country_profiles$pop_dens), sd(country_profiles$pop_dens))
ks.test(country_profiles$pop_growth, pnorm, mean(country_profiles$pop_growth), sd(country_profiles$pop_growth))
ks.test(country_profiles$urban_pop, pnorm, mean(country_profiles$urban_pop), sd(country_profiles$urban_pop))
ks.test(country_profiles$gdp, pnorm, mean(country_profiles$gdp), sd(country_profiles$gdp))
ks.test(country_profiles$gdp_growth, pnorm, mean(country_profiles$gdp_growth), sd(country_profiles$gdp_growth))
ks.test(country_profiles$gdp_xcap, pnorm, mean(country_profiles$gdp_xcap), sd(country_profiles$gdp_xcap))
ks.test(country_profiles$eco_agri, pnorm, mean(country_profiles$eco_agri), sd(country_profiles$eco_agri))
ks.test(country_profiles$eco_industry, pnorm, mean(country_profiles$eco_industry), sd(country_profiles$eco_industry))
ks.test(country_profiles$eco_services, pnorm, mean(country_profiles$eco_services), sd(country_profiles$eco_services))
ks.test(country_profiles$empl_agri, pnorm, mean(country_profiles$eco_agri), sd(country_profiles$eco_agri))
ks.test(country_profiles$empl_industry, pnorm, mean(country_profiles$empl_industry), sd(country_profiles$empl_industry))
ks.test(country_profiles$empl_services, pnorm, mean(country_profiles$empl_services), sd(country_profiles$empl_services))
ks.test(country_profiles$educ_exp, pnorm, mean(country_profiles$educ_exp), sd(country_profiles$educ_exp))

# Comprobemos el gráfico Q-Q de cada variable.
# Con estos, podremos observar si tan solo los outliers se salen de los patrones.
layout(matrix(c(1,2,3,4,5,
                6,7,8,9,10,
                11,12,13,14,15), nrow = 3, ncol = 5, byrow = TRUE))
qqnorm(country_profiles$population, main = "Population")
qqline(country_profiles$population, col = "red")
qqnorm(country_profiles$pop_dens, main = "Pop_Density")
qqline(country_profiles$pop_dens, col = "red")
qqnorm(country_profiles$pop_growth, main = "Pop_Growth")
qqline(country_profiles$pop_growth, col = "red")
qqnorm(country_profiles$urban_pop, main = "Urban_Pop")
qqline(country_profiles$urban_pop, col = "red")
qqnorm(country_profiles$gdp, main = "GDP")
qqline(country_profiles$gdp, col = "red")
qqnorm(country_profiles$gdp_growth, main = "GDP_Growth")
qqline(country_profiles$gdp_growth, col = "red")
qqnorm(country_profiles$gdp_xcap, main = "GDP x cap")
qqline(country_profiles$gdp_xcap, col = "red")
qqnorm(country_profiles$eco_agri, main = "Eco_Agri")
qqline(country_profiles$eco_agri, col = "red")
qqnorm(country_profiles$eco_industry, main = "Eco_Industry")
qqline(country_profiles$eco_industry, col = "red")
qqnorm(country_profiles$eco_services, main = "Eco_Services")
qqline(country_profiles$eco_services, col = "red")
qqnorm(country_profiles$empl_agri, main = "Empl_Agri")
qqline(country_profiles$empl_agri, col = "red")
qqnorm(country_profiles$empl_industry, main = "Empl_Industry")
qqline(country_profiles$empl_industry, col = "red")
qqnorm(country_profiles$empl_services, main = "Empl_Services")
qqline(country_profiles$empl_services, col = "red")
qqnorm(country_profiles$educ_exp, main = "Educ_Exp")
qqline(country_profiles$educ_exp, col = "red")
```

Realizamos el primer análisis, una regresión lineal múltiple en la cual intentaremos predecir PIB por cápita a través de las
métricas relacionadas con los tres sectores económicos principales de un país (agricultura, industria, servicios).

```{r lm test}
# Cargamos librerías necesarias
library(rminer)

# Dividimos datos en entrenamiento y validación, con un ratio de 3/4.
# Mantendremos los datos estratificados por región.
h<-holdout(country_profiles$Region, ratio = 3/4, mode="order")
data_train<-country_profiles[h$tr,]
data_test<-country_profiles[h$ts,]

# Creamos el modelo, y realizamos un resumen del mismo.
regressor <- lm(gdp_xcap ~ eco_agri + eco_industry + eco_services + empl_agri + empl_industry + empl_services, data = data_train)
summary(regressor)

# Aplicamos el modelo sobre los datos de entrenamiento, y estudiamos correlación entre datos reales y predichos.
train_predict<-predict(regressor, data=data_train)
cor(train_predict, data_train$gdp_xcap)

# Aplicamos el modelo sobre los datos de test, y estudiamos correlación entre datos reales y predichos.
test_predict<-predict(regressor, newdata=data_test)
cor(test_predict, data_test$gdp_xcap)

# Comprobamos normalidad a través del gráfico de residuos.
plot(regressor, which=2)

# Comprobamos homocedasticidad de los residuos.
plot(regressor, which=3)

# Gráfico de representación del modelo:
library(car)
avPlots(regressor)
```

Segundo análisis, Test coeficiente de correlación de Spearman entre PIB y variables demográficas:

```{r spearman}
# Almacenamos las correlaciones de Spearman en una matriz.
corr.res <- cor(country_profiles[, c("gdp", "gdp_xcap", "gdp_growth", "urban_pop", "pop_dens", "educ_exp", "population", "pop_growth")], method="spearman")
corr.res

# Elaboramos el gráfico de correlaciones.
library(corrplot)
corrplot.mixed(corr.res, upper="ellipse", tl.cex = 0.6)
```

A continuación presentamos el tercer análisis, un contraste de medianas. Aquí, comprobaremos si la mediana de crecimiento de PIB de dos regiones coincide, con un 95% de confianza.

```{r mann-whitney}
# Comprobamos igualdad de varianzas.
fligner.test(gdp_growth ~ Region, data=country_profiles)

# No hay diferencias de vaianza significativas entre regiones para la variable gdp_growth.
# Separamos datos por regiones.
country_profiles_europe <- country_profiles[which(country_profiles$Region=="WesternEurope" | country_profiles$Region=="EasternEurope" | country_profiles$Region=="NorthernEurope" | country_profiles$Region=="SouthernEurope"), ]
country_profiles_asia <- country_profiles[which(country_profiles$Region=="WesternAsia" | country_profiles$Region=="EasternAsia" | country_profiles$Region=="CentralAsia" | country_profiles$Region=="SouthernAsia" | country_profiles$Region=="South-easternAsia"), ]

# Primero, aplicamos un test Chi-cuadrado para saber si las dos variables son dependientes entre ellas.
# Dependiendo del resultado, emplearemos Wilcoxon o Mann-Whitney.
chisq.test(table(country_profiles_europe, country_profiles_asia))
                 
# Obtenemos p-value muy grande, de entre 20% y 30%.
# Por tanto, no podemos rechazar hipótesis nula (independencia).

# Mann-Whitney se aplica con la misma función wilcox.test(), pasando como parámetros dos grupos de datos
# independientes, y aplicando el parámetro paired=FALSE.
wilcox.test(country_profiles_europe$gdp_growth, country_profiles_asia$gdp_growth, paired=FALSE)

# p-value muy poco superior a 0.05. Por tanto, no podemos rechazar la hipótesis nula, y no podemos asegurar que Europa o Asia no estén experimentando crecimientos similares,
# si bien por muy poco.

# Comprobemos visualmente la diferencia de medianas:

country_profiles_europe$Region <- "Europe"
country_profiles_asia$Region <- "Asia"
country_profiles_eurasia = rbind(country_profiles_europe, country_profiles_asia)
country_profiles_eurasia$Region <- factor(country_profiles_eurasia$Region)
plot(country_profiles_eurasia$Region, country_profiles_eurasia$gdp_growth, main = "Comparativa de medianas", xlab = "Región", ylab = "gdp_growth")
```



