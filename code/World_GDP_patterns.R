# CÓDIGO TRAS FEEDBACK DE DIEGO

---
title: "Práctica2 TyCdV de los Datos"
author: "Carlos Lavado Mahia, Dionisio González Jiménez"
date: "06/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

En este primer apartado, cargaremos y prepararemos los datos, poniendo especial enfásis en el formato de las columnas, así como en la detección de outliers y de valores faltantes:

```{r carga y preparación}
# Cargamos el dataset.
country_profiles_og<-read.csv("C:/Users/carlo/OneDrive/Escritorio/country_profile_variables.csv", sep=",", check.names=FALSE)

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

```{r valores vacios}
# FALTA CAMBIAR LOS VALORES VACIOS DE LAS 3 DE EMPLEO Y EDUCACION A NA.
paste("Valores vacios en población:", which(is.na(country_profiles$population)))
paste("Valores vacios en densidad población:", which(is.na(country_profiles$pop_dens)))
paste("Valores vacios en crecimiento de población:", which(is.na(country_profiles$pop_growth)))
paste("Valores vacios en población urbana:", which(is.na(country_profiles$urban_pop)))
paste("Valores vacios en PIB", which(is.na(country_profiles$gdp)))
paste("Valores vacios en crecimiento del PIB:", which(is.na(country_profiles$gdp_growth)))
paste("Valores vacios en PIB per capita:", which(is.na(country_profiles$gdp_xcap)))
paste("Valores vacios en sector agricultura:", which(is.na(country_profiles$eco_agri)))
paste("Valores vacios en sector industria:", which(is.na(country_profiles$eco_industry)))
paste("Valores vacios en sector servicios:", which(is.na(country_profiles$eco_services)))

paste("Valores vacios en empleo agricultura:")
valores1<-which(is.na(country_profiles$empl_agri))
valores1
paste("Valores vacios en empleo industria:")
valores2<-which(is.na(country_profiles$empl_industry))
valores2
paste("Valores vacios en empleo servicios:")
valores3<-which(is.na(country_profiles$empl_services))
valores3
paste("Valores vacios en educación:")
valores4<-which(is.na(country_profiles$educ_exp))
valores4

# Sustituimos los valores vacíos por NA.

# Comprobamos los valores extremos con boxplots:
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
paste("Valores extremos de educacion:")
boxplot.stats(country_profiles$educ_exp)$out

# Tras una inspección visual, los valores faltantes parecen haberse marcado con -99.
# Procedemos a sustituirlos por NA.
country_profiles[country_profiles == -99.0] <- NA

# PENDIENTE: PASAR PARTES DE BOX-PLOT$OUT Y COMENTARIOS

# Para acabar de preparar el dataset antes de aplicar el algoritmo de imputación de NAs,
# extraemos temporalmente la columna country (más de 53 categorías).
country_profiles <- subset(country_profiles, select = -c(country))
```

Ahora, procedamos a aplicar un algoritmo de imputación para los valores faltantes:

```{r missForest}
library(missForest)

country_profiles_clean <- missForest(country_profiles)

country_profiles <- country_profiles_clean$ximp

country_profiles <- data.frame(country_profiles_og$country, country_profiles)

summary(country_profiles)
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

Para comprobar la igualdad de varianzas utilizamos el test fligner-killen
```{r test fligner-killen}
# Se comprueba la homogeneidad de las varianzas entre las variables PIB per cápita y sector agricultura
fligner.test(gdp_xcap~eco_agri, data=country_profiles)
# Aceptamos la hipótesis nula y confirmamos la igualdad de las varianzas entre las variables.

# Se comprueba la homogeneidad de las varianzas entre las variables PIB per cápita y sectro industria

```
