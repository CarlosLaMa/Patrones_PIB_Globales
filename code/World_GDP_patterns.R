```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r import}
dataset<-read.csv("C:/Users/carlo/OneDrive/Escritorio/country_profile_variables.csv", sep=",", check.names=FALSE)
colnames(dataset)
```

```{r selección}
new_dataset<-dataset[,c(1,2,4,5,7,8,9,10,11,12,13,14,15,24,25,35)]
sapply(new_dataset, typeof)
```
```{r resumen inicial}
summary(new_dataset)
```

```{r head}
head(new_dataset)
```

```{r vble converting}
new_dataset$Region<-as.factor(new_dataset$Region)
new_dataset$`GDP growth rate (annual %, const. 2005 prices)`<-as.numeric(new_dataset$`GDP growth rate (annual %, const. 2005 prices)`)
new_dataset$`Economy: Agriculture (% of GVA)`<-as.numeric(new_dataset$`Economy: Agriculture (% of GVA)`)
new_dataset$`Employment: Agriculture (% of employed)`<-as.numeric(new_dataset$`Employment: Agriculture (% of employed)`)
new_dataset$`Employment: Industry (% of employed)`<-as.numeric(new_dataset$`Employment: Industry (% of employed)`)
new_dataset$`Employment: Services (% of employed)`<-as.numeric(new_dataset$`Employment: Services (% of employed)`)
new_dataset$`Population growth rate (average annual %)`<-as.numeric(new_dataset$`Population growth rate (average annual %)`)
new_dataset$`Education: Government expenditure (% of GDP)`<-as.numeric(new_dataset$`Education: Government expenditure (% of GDP)`)
```
```{r probando}
summary(new_dataset)
```

```{r outliers y valores perdidos}

# Con la función boxplot() de R, se puede observar si existen observaciones distanciadas a más de 1,5 de proporción del rango intercuartil.
# Con boxplot.stats$out, observamos de qué observaciones se trata para saber si son razonables.

# De momento, realizamos este análisis para variables en las que observamos una gran disparidad entre media y mediana, ya que esto puede implicar
# que hay valores significativamente grandes distorsionando el valor de la media aritmética.

paste("Valores extremos de población (en miles):")
sort(boxplot.stats(new_dataset$`Population in thousands (2017)`)$out)

paste("Valores extremos de densidad de población en km2:")  
sort(boxplot.stats(new_dataset$`Population density (per km2, 2017)`)$out)

paste("Valores extremos de GDP:")  
sort(boxplot.stats(new_dataset$`GDP: Gross domestic product (million current US$)`)$out)

# Comprobemos que los números más altos corresponden a grandes potencias.
paste("Los valores de GDP más grandes corresponden a:")
new_dataset$country[new_dataset$`GDP: Gross domestic product (million current US$)` == 11158457]
new_dataset$country[new_dataset$`GDP: Gross domestic product (million current US$)` == 18036648]

paste("Valores extremos de crecimiento anual del GDP:")
sort(boxplot.stats(new_dataset$`GDP growth rate (annual %, const. 2005 prices)`)$out)

# En muchas ocasiones, se ha imputado -99. Esto indica tratamiento de valores perdidos!
new_dataset$country[new_dataset$`GDP growth rate (annual %, const. 2005 prices)` == -99.000000]
new_dataset$Region[new_dataset$`GDP growth rate (annual %, const. 2005 prices)` == -99.000000]

# Hemos podido comprobar, que algunos países ni siquiera se encuentran identificados (string NA).
# Aprovechamos para extraer estas filas de la muestra.
new_dataset <- new_dataset[!is.na(new_dataset$country), ]

# PARECE QUE CÓDIGO NO FUNCIONA, SIGUE HABIENDO EL PAÍS NA. COMPROBAR PROBLEMA
new_dataset$country[new_dataset$`GDP growth rate (annual %, const. 2005 prices)` == -99.000000]

# Transformamos estos valores a NA
new_dataset$`GDP growth rate (annual %, const. 2005 prices)`[new_dataset$`GDP growth rate (annual %, const. 2005 prices)` == -99.000000] <- NA

# Ahora, los reemplazamos por la media de crecimiento de GDP de su región respectiva:
new_dataset$`GDP growth rate (annual %, const. 2005 prices)`[is.na(new_dataset$`GDP growth rate (annual %, const. 2005 prices)`) & new_dataset$Region == "Polynesia"] <- mean(!is.na(new_dataset$`GDP growth rate (annual %, const. 2005 prices)`) & new_dataset$Region == "Polynesia")

new_dataset$`GDP growth rate (annual %, const. 2005 prices)`[is.na(new_dataset$`GDP growth rate (annual %, const. 2005 prices)`) & new_dataset$Region == "Caribbean"] <- mean(!is.na(new_dataset$`GDP growth rate (annual %, const. 2005 prices)`) & new_dataset$Region == "Caribbean")

new_dataset$`GDP growth rate (annual %, const. 2005 prices)`[is.na(new_dataset$`GDP growth rate (annual %, const. 2005 prices)`) & new_dataset$Region == "SouthernEurope"] <- mean(!is.na(new_dataset$`GDP growth rate (annual %, const. 2005 prices)`) & new_dataset$Region == "SouthernEurope")

new_dataset$`GDP growth rate (annual %, const. 2005 prices)`[is.na(new_dataset$`GDP growth rate (annual %, const. 2005 prices)`) & new_dataset$Region == "NorthernEurope"] <- mean(!is.na(new_dataset$`GDP growth rate (annual %, const. 2005 prices)`) & new_dataset$Region == "NorthernEurope")

new_dataset$`GDP growth rate (annual %, const. 2005 prices)`[is.na(new_dataset$`GDP growth rate (annual %, const. 2005 prices)`) & new_dataset$Region == "SouthAmerica"] <- mean(!is.na(new_dataset$`GDP growth rate (annual %, const. 2005 prices)`) & new_dataset$Region == "SouthAmerica")

new_dataset$`GDP growth rate (annual %, const. 2005 prices)`[is.na(new_dataset$`GDP growth rate (annual %, const. 2005 prices)`) & new_dataset$Region == "NorthernAmerica"] <- mean(!is.na(new_dataset$`GDP growth rate (annual %, const. 2005 prices)`) & new_dataset$Region == "NorthernAmerica")

new_dataset$`GDP growth rate (annual %, const. 2005 prices)`[is.na(new_dataset$`GDP growth rate (annual %, const. 2005 prices)`) & new_dataset$Region == "Micronesia"] <- mean(!is.na(new_dataset$`GDP growth rate (annual %, const. 2005 prices)`) & new_dataset$Region == "Micronesia")

new_dataset$`GDP growth rate (annual %, const. 2005 prices)`[is.na(new_dataset$`GDP growth rate (annual %, const. 2005 prices)`) & new_dataset$Region == "EasternAfrica"] <- mean(!is.na(new_dataset$`GDP growth rate (annual %, const. 2005 prices)`) & new_dataset$Region == "EasternAfrica")

new_dataset$`GDP growth rate (annual %, const. 2005 prices)`[is.na(new_dataset$`GDP growth rate (annual %, const. 2005 prices)`) & new_dataset$Region == "WesternAfrica"] <- mean(!is.na(new_dataset$`GDP growth rate (annual %, const. 2005 prices)`) & new_dataset$Region == "WesternAfrica")

new_dataset$`GDP growth rate (annual %, const. 2005 prices)`[is.na(new_dataset$`GDP growth rate (annual %, const. 2005 prices)`) & new_dataset$Region == "NorthernAfrica"] <- mean(!is.na(new_dataset$`GDP growth rate (annual %, const. 2005 prices)`) & new_dataset$Region == "NorthernAfrica")

# Comprobamos si queda alguna observación por transformar.
paste(new_dataset$country[is.na(new_dataset$`GDP growth rate (annual %, const. 2005 prices)`)])

# Transformamos los valores -99 a NA de la columna GDP.
new_dataset$`GDP: Gross domestic product (million current US$)`[new_dataset$`GDP: Gross domestic product (million current US$)` == -99.000000] <- NA

# Ahora, los reemplazamos por la media de GDP de su región respectiva:
new_dataset$`GDP: Gross domestic product (million current US$)`[is.na(new_dataset$`GDP: Gross domestic product (million current US$)`) & new_dataset$Region == "Polynesia"] <- mean(!is.na(new_dataset$`GDP: Gross domestic product (million current US$)`) & new_dataset$Region == "Polynesia")
new_dataset$`GDP: Gross domestic product (million current US$)`[is.na(new_dataset$`GDP: Gross domestic product (million current US$)`) & new_dataset$Region == "Caribbean"] <- mean(!is.na(new_dataset$`GDP: Gross domestic product (million current US$)`) & new_dataset$Region == "Caribbean")
new_dataset$`GDP: Gross domestic product (million current US$)`[is.na(new_dataset$`GDP: Gross domestic product (million current US$)`) & new_dataset$Region == "SouthernEurope"] <- mean(!is.na(new_dataset$`GDP: Gross domestic product (million current US$)`) & new_dataset$Region == "SouthernEurope")
new_dataset$`GDP: Gross domestic product (million current US$)`[is.na(new_dataset$`GDP: Gross domestic product (million current US$)`) & new_dataset$Region == "NorthernEurope"] <- mean(!is.na(new_dataset$`GDP: Gross domestic product (million current US$)`) & new_dataset$Region == "NorthernEurope")
new_dataset$`GDP: Gross domestic product (million current US$)`[is.na(new_dataset$`GDP: Gross domestic product (million current US$)`) & new_dataset$Region == "SouthAmerica"] <- mean(!is.na(new_dataset$`GDP: Gross domestic product (million current US$)`) & new_dataset$Region == "SouthAmerica")
new_dataset$`GDP: Gross domestic product (million current US$)`[is.na(new_dataset$`GDP: Gross domestic product (million current US$)`) & new_dataset$Region == "NorthernAmerica"] <- mean(!is.na(new_dataset$`GDP: Gross domestic product (million current US$)`) & new_dataset$Region == "NorthernAmerica")
new_dataset$`GDP: Gross domestic product (million current US$)`[is.na(new_dataset$`GDP: Gross domestic product (million current US$)`) & new_dataset$Region == "Micronesia"] <- mean(!is.na(new_dataset$`GDP: Gross domestic product (million current US$)`) & new_dataset$Region == "Micronesia")
new_dataset$`GDP: Gross domestic product (million current US$)`[is.na(new_dataset$`GDP: Gross domestic product (million current US$)`) & new_dataset$Region == "EasternAfrica"] <- mean(!is.na(new_dataset$`GDP: Gross domestic product (million current US$)`) & new_dataset$Region == "EasternAfrica")
new_dataset$`GDP: Gross domestic product (million current US$)`[is.na(new_dataset$`GDP: Gross domestic product (million current US$)`) & new_dataset$Region == "WesternAfrica"] <- mean(!is.na(new_dataset$`GDP: Gross domestic product (million current US$)`) & new_dataset$Region == "WesternAfrica")
new_dataset$`GDP: Gross domestic product (million current US$)`[is.na(new_dataset$`GDP: Gross domestic product (million current US$)`) & new_dataset$Region == "NorthernAfrica"] <- mean(!is.na(new_dataset$`GDP: Gross domestic product (million current US$)`) & new_dataset$Region == "NorthernAfrica")

# Comprobamos que no queda ningún valor NA en la columna GDP
new_dataset$country[is.na(new_dataset$`GDP: Gross domestic product (million current US$)`)]

# Transformamos los valores -99 a NA de la columna GDP per capita
new_dataset$`GDP per capita (current US$)`[new_dataset$`GDP per capita (current US$)` == -99.000000] <- NA

# Ahora, los reemplazamos por la media de GDP per capita de su región respectiva:
new_dataset$`GDP per capita (current US$)`[is.na(new_dataset$`GDP per capita (current US$)`) & new_dataset$Region == "Polynesia"] <- mean(!is.na(new_dataset$`GDP per capita (current US$)`) & new_dataset$Region == "Polynesia")
new_dataset$`GDP per capita (current US$)`[is.na(new_dataset$`GDP per capita (current US$)`) & new_dataset$Region == "Caribbean"] <- mean(!is.na(new_dataset$`GDP per capita (current US$)`) & new_dataset$Region == "Caribbean")
new_dataset$`GDP per capita (current US$)`[is.na(new_dataset$`GDP per capita (current US$)`) & new_dataset$Region == "SouthernEurope"] <- mean(!is.na(new_dataset$`GDP per capita (current US$)`) & new_dataset$Region == "SouthernEurope")
new_dataset$`GDP per capita (current US$)`[is.na(new_dataset$`GDP per capita (current US$)`) & new_dataset$Region == "NorthernEurope"] <- mean(!is.na(new_dataset$`GDP per capita (current US$)`) & new_dataset$Region == "NorthernEurope")
new_dataset$`GDP per capita (current US$)`[is.na(new_dataset$`GDP per capita (current US$)`) & new_dataset$Region == "SouthAmerica"] <- mean(!is.na(new_dataset$`GDP per capita (current US$)`) & new_dataset$Region == "SouthAmerica")
new_dataset$`GDP per capita (current US$)`[is.na(new_dataset$`GDP per capita (current US$)`) & new_dataset$Region == "NorthernAmerica"] <- mean(!is.na(new_dataset$`GDP per capita (current US$)`) & new_dataset$Region == "NorthernAmerica")
new_dataset$`GDP per capita (current US$)`[is.na(new_dataset$`GDP per capita (current US$)`) & new_dataset$Region == "Micronesia"] <- mean(!is.na(new_dataset$`GDP per capita (current US$)`) & new_dataset$Region == "Micronesia")
new_dataset$`GDP per capita (current US$)`[is.na(new_dataset$`GDP per capita (current US$)`) & new_dataset$Region == "EasternAfrica"] <- mean(!is.na(new_dataset$`GDP per capita (current US$)`) & new_dataset$Region == "EasternAfrica")
new_dataset$`GDP per capita (current US$)`[is.na(new_dataset$`GDP per capita (current US$)`) & new_dataset$Region == "WesternAfrica"] <- mean(!is.na(new_dataset$`GDP per capita (current US$)`) & new_dataset$Region == "WesternAfrica")
new_dataset$`GDP per capita (current US$)`[is.na(new_dataset$`GDP per capita (current US$)`) & new_dataset$Region == "NorthernAfrica"] <- mean(!is.na(new_dataset$`GDP per capita (current US$)`) & new_dataset$Region == "NorthernAfrica")

# Comprobamos que no queda ningún valor NA en la columna GDP per capita
new_dataset$country[is.na(new_dataset$`GDP per capita (current US$)`)]

# Transformamos los valores -99 a NA de la columna Economy: Agriculture
new_dataset$`Economy: Agriculture (% of GVA)`[new_dataset$`Economy: Agriculture (% of GVA)` == -99.000000] <- NA

# Ahora, los reemplazamos por la media de Economy: Agriculture de su región respectiva:
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "Polynesia"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "Polynesia")
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "Caribbean"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "Caribbean")
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "SouthernEurope"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "SouthernEurope")
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "NorthernEurope"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "NorthernEurope")
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "SouthAmerica"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "SouthAmerica")
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "NorthernAmerica"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "NorthernAmerica")
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "Micronesia"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "Micronesia")
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "EasternAfrica"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "EasternAfrica")
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "EasternAsia"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "EasternAsia")
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "WesternAfrica"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "WesternAfrica")
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "NorthernAfrica"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "NorthernAfrica")
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "WesternEurope"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "WesternEurope")
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "South-easternAsia"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "South-easternAsia")

# Comprobamos que no queda ningún valor NA en la columna Economy: Agriculture
new_dataset$country[is.na(new_dataset$`Economy: Agriculture (% of GVA)`)]

# Transformamos los valores -99 a NA de la columna Economy: Industry
new_dataset$`Economy: Industry (% of GVA)`[new_dataset$`Economy: Industry (% of GVA)` == -99.000000] <- NA

# Ahora, los reemplazamos por la media de Economy: Industry de su región respectiva:
new_dataset$`Economy: Industry (% of GVA)`[is.na(new_dataset$`Economy: Industry (% of GVA)`) & new_dataset$Region == "Polynesia"] <- mean(!is.na(new_dataset$`Economy: Industry (% of GVA)`) & new_dataset$Region == "Polynesia")
new_dataset$`Economy: Industry (% of GVA)`[is.na(new_dataset$`Economy: Industry (% of GVA)`) & new_dataset$Region == "Caribbean"] <- mean(!is.na(new_dataset$`Economy: Industry (% of GVA)`) & new_dataset$Region == "Caribbean")
new_dataset$`Economy: Industry (% of GVA)`[is.na(new_dataset$`Economy: Industry (% of GVA)`) & new_dataset$Region == "SouthernEurope"] <- mean(!is.na(new_dataset$`Economy: Industry (% of GVA)`) & new_dataset$Region == "SouthernEurope")
new_dataset$`Economy: Industry (% of GVA)`[is.na(new_dataset$`Economy: Industry (% of GVA)`) & new_dataset$Region == "NorthernEurope"] <- mean(!is.na(new_dataset$`Economy: Industry (% of GVA)`) & new_dataset$Region == "NorthernEurope")
new_dataset$`Economy: Industry (% of GVA)`[is.na(new_dataset$`Economy: Industry (% of GVA)`) & new_dataset$Region == "SouthAmerica"] <- mean(!is.na(new_dataset$`Economy: Industry (% of GVA)`) & new_dataset$Region == "SouthAmerica")
new_dataset$`Economy: Industry (% of GVA)`[is.na(new_dataset$`Economy: Industry (% of GVA)`) & new_dataset$Region == "NorthernAmerica"] <- mean(!is.na(new_dataset$`Economy: Industry (% of GVA)`) & new_dataset$Region == "NorthernAmerica")
new_dataset$`Economy: Industry (% of GVA)`[is.na(new_dataset$`Economy: Industry (% of GVA)`) & new_dataset$Region == "Micronesia"] <- mean(!is.na(new_dataset$`Economy: Industry (% of GVA)`) & new_dataset$Region == "Micronesia")
new_dataset$`Economy: Industry (% of GVA)`[is.na(new_dataset$`Economy: Industry (% of GVA)`) & new_dataset$Region == "EasternAfrica"] <- mean(!is.na(new_dataset$`Economy: Industry (% of GVA)`) & new_dataset$Region == "EasternAfrica")
new_dataset$`Economy: Industry (% of GVA)`[is.na(new_dataset$`Economy: Industry (% of GVA)`) & new_dataset$Region == "WesternAfrica"] <- mean(!is.na(new_dataset$`Economy: Industry (% of GVA)`) & new_dataset$Region == "WesternAfrica")
new_dataset$`Economy: Industry (% of GVA)`[is.na(new_dataset$`Economy: Industry (% of GVA)`) & new_dataset$Region == "NorthernAfrica"] <- mean(!is.na(new_dataset$`Economy: Industry (% of GVA)`) & new_dataset$Region == "NorthernAfrica")



# Comprobamos que no queda ningún valor NA en la columna Economy: Industry
new_dataset$country[is.na(new_dataset$`Economy: Industry (% of GVA)`)]
# A partir de aquí no lo tengas en cuenta han sido pruebas, porque tiene un valor -99 pero lo he intentado mostrar y me indica que es factor, he intentado cambiar a numérico y tampoco puedo, por si se te ocurre algo.

# Transformamos los valores -99 a NA de la columna surface area.
#new_dataset$`Surface area (km2)`[new_dataset$`Surface area (km2)` == -99.000000] <- NA

#new_dataset$`Surface area (km2)`[is.na(new_dataset$`Surface area (km2)`) & new_dataset$Region == "NorthernAfrica"] <- mean(!is.na(new_dataset$`Surface area (km2)`) & new_dataset$Region == "NorthernAfrica")
```
