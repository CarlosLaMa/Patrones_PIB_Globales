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

# Ninguno de los outliers de estas dos primeras columnas, debería considerarse un valor extremo a suprimir.
paste("Valores extremos de GDP:")  
sort(boxplot.stats(new_dataset$`GDP: Gross domestic product (million current US$)`)$out)

# Comprobemos que los números más altos corresponden a grandes potencias.
paste("Los valores de GDP más grandes corresponden a:")
new_dataset$country[new_dataset$`GDP: Gross domestic product (million current US$)` == 11158457]
new_dataset$country[new_dataset$`GDP: Gross domestic product (million current US$)` == 18036648]

# Si bien no existen outliers en la columna GDP, sí se pueden observar campos marcados como -99.
# Tal y como veremos en la siguiente columna, esto se ha realizado para tratar valores perdidos.
# Nosotros lo cambiaremos, aplicando en cada campo la media de su respectiva región.
# Para ello, revisemos primero los países y regiones que no disponen de datos en esta columna:
new_dataset$country[new_dataset$`GDP: Gross domestic product (million current US$)` == -99]
new_dataset$Region[new_dataset$`GDP: Gross domestic product (million current US$)` == -99]

# Transformamos los valores -99 a NA de la columna GDP.
new_dataset$`GDP: Gross domestic product (million current US$)`[new_dataset$`GDP: Gross domestic product (million current US$)` == -99] <- NA

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

paste("Valores extremos de GDP per capita:")

# Comprobemos primero los outliers de esta columna
boxplot.stats(new_dataset$`GDP per capita (current US$)`)$out

# Todos estos valores son razonables desde un punto de vista económico. Por tanto, los mantenemos.
# Por otro lado, la columna dispone de campos -99 (valores perdidos). 
# Procedemos a comprobar de qué regiones se trata, y modificar los campos.
new_dataset$country[new_dataset$`GDP per capita (current US$)` == -99]
new_dataset$Region[new_dataset$`GDP per capita (current US$)` == -99]
new_dataset$`GDP per capita (current US$)`[new_dataset$`GDP per capita (current US$)` == -99] <- NA

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

paste("Valores extremos del peso de Agriculture sobre GVA:")

# Comprobemos primero los outliers de esta columna
boxplot.stats(new_dataset$`Economy: Agriculture (% of GVA)`)$out

# Excepto los valores -99, todos son razonables desde un punto de vista económico. Por tanto, los mantenemos.
# Por otro lado, la columna dispone de campos -99 (valores perdidos). 
# Procedemos a comprobar de qué regiones se trata, y modificar los campos.
new_dataset$country[new_dataset$`Economy: Agriculture (% of GVA)` == -99.000000]
new_dataset$Region[new_dataset$`Economy: Agriculture (% of GVA)` == -99.000000]
# NOTA: VUELVE A HABER UN PAÍS CON NA (FILA 20)
new_dataset$`Economy: Agriculture (% of GVA)`[new_dataset$`Economy: Agriculture (% of GVA)` == -99.000000] <- NA

# Ahora, los reemplazamos por la media de Economy: Agriculture de su región respectiva:
# NOTA: Algunas regiones, correspondientes a países que no cabían en la celda mostrada, han sido añadidas al final.
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "Polynesia"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "Polynesia")
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "Caribbean"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "Caribbean")
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "SouthernEurope"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "SouthernEurope")
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "NorthernEurope"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "NorthernEurope")
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "WesternEurope"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "WesternEurope")
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "SouthAmerica"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "SouthAmerica")
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "NorthernAmerica"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "NorthernAmerica")
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "Micronesia"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "Micronesia")
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "EasternAfrica"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "EasternAfrica")
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "WesternAfrica"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "WesternAfrica")
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "NorthernAfrica"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "NorthernAfrica")
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "EasternAsia"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "EasternAsia")
new_dataset$`Economy: Agriculture (% of GVA)`[is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "South-easternAsia"] <- mean(!is.na(new_dataset$`Economy: Agriculture (% of GVA)`) & new_dataset$Region == "South-easternAsia")

# Comprobamos que no queda ningún valor NA en la columna Economy: Agriculture
new_dataset$country[is.na(new_dataset$`Economy: Agriculture (% of GVA)`)]

paste("Valores extremos del peso de Industry sobre GVA:")

# Comprobemos primero los outliers de esta columna
boxplot.stats(new_dataset$`Economy: Industry (% of GVA)`)$out

# Los únicos outliers detectados son campos de valores perdidos (-99).
# Procedemos a comprobar de qué regiones se trata, y modificar los campos.
new_dataset$country[new_dataset$`Economy: Industry (% of GVA)` == -99.000000]
new_dataset$Region[new_dataset$`Economy: Industry (% of GVA)` == -99.000000]
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

paste("Valores extremos del peso de Services and other activities sobre GVA:")

# Comprobemos primero los outliers de esta columna
boxplot.stats(new_dataset$`Economy: Services and other activity (% of GVA)`)$out

# Excepto los valores -99, todos son razonables desde un punto de vista económico. Por tanto, los mantenemos.
# Por otro lado, la columna dispone de campos -99 (valores perdidos). 
# Procedemos a comprobar de qué regiones se trata, y modificar los campos.
new_dataset$country[new_dataset$`Economy: Services and other activity (% of GVA)` == -99.000000]
new_dataset$Region[new_dataset$`Economy: Services and other activity (% of GVA)` == -99.000000]
new_dataset$`Economy: Services and other activity (% of GVA)`[new_dataset$`Economy: Services and other activity (% of GVA)` == -99.000000] <- NA

# Ahora, los reemplazamos por la media de Economy: Services and other activity de su región respectiva:
new_dataset$`Economy: Services and other activity (% of GVA)`[is.na(new_dataset$`Economy: Services and other activity (% of GVA)`) & new_dataset$Region == "Polynesia"] <- mean(!is.na(new_dataset$`Economy: Services and other activity (% of GVA)`) & new_dataset$Region == "Polynesia")
new_dataset$`Economy: Services and other activity (% of GVA)`[is.na(new_dataset$`Economy: Services and other activity (% of GVA)`) & new_dataset$Region == "Caribbean"] <- mean(!is.na(new_dataset$`Economy: Services and other activity (% of GVA)`) & new_dataset$Region == "Caribbean")
new_dataset$`Economy: Services and other activity (% of GVA)`[is.na(new_dataset$`Economy: Services and other activity (% of GVA)`) & new_dataset$Region == "SouthernEurope"] <- mean(!is.na(new_dataset$`Economy: Services and other activity (% of GVA)`) & new_dataset$Region == "SouthernEurope")
new_dataset$`Economy: Services and other activity (% of GVA)`[is.na(new_dataset$`Economy: Services and other activity (% of GVA)`) & new_dataset$Region == "NorthernEurope"] <- mean(!is.na(new_dataset$`Economy: Services and other activity (% of GVA)`) & new_dataset$Region == "NorthernEurope")
new_dataset$`Economy: Services and other activity (% of GVA)`[is.na(new_dataset$`Economy: Services and other activity (% of GVA)`) & new_dataset$Region == "SouthAmerica"] <- mean(!is.na(new_dataset$`Economy: Services and other activity (% of GVA)`) & new_dataset$Region == "SouthAmerica")
new_dataset$`Economy: Services and other activity (% of GVA)`[is.na(new_dataset$`Economy: Services and other activity (% of GVA)`) & new_dataset$Region == "NorthernAmerica"] <- mean(!is.na(new_dataset$`Economy: Services and other activity (% of GVA)`) & new_dataset$Region == "NorthernAmerica")
new_dataset$`Economy: Services and other activity (% of GVA)`[is.na(new_dataset$`Economy: Services and other activity (% of GVA)`) & new_dataset$Region == "Micronesia"] <- mean(!is.na(new_dataset$`Economy: Services and other activity (% of GVA)`) & new_dataset$Region == "Micronesia")
new_dataset$`Economy: Services and other activity (% of GVA)`[is.na(new_dataset$`Economy: Services and other activity (% of GVA)`) & new_dataset$Region == "EasternAfrica"] <- mean(!is.na(new_dataset$`Economy: Services and other activity (% of GVA)`) & new_dataset$Region == "EasternAfrica")
new_dataset$`Economy: Services and other activity (% of GVA)`[is.na(new_dataset$`Economy: Services and other activity (% of GVA)`) & new_dataset$Region == "WesternAfrica"] <- mean(!is.na(new_dataset$`Economy: Services and other activity (% of GVA)`) & new_dataset$Region == "WesternAfrica")
new_dataset$`Economy: Services and other activity (% of GVA)`[is.na(new_dataset$`Economy: Services and other activity (% of GVA)`) & new_dataset$Region == "NorthernAfrica"] <- mean(!is.na(new_dataset$`Economy: Services and other activity (% of GVA)`) & new_dataset$Region == "NorthernAfrica")

# Comprobamos que no queda ningún valor NA en la columna Economy: Services and other activity
new_dataset$country[is.na(new_dataset$`Economy: Services and other activity (% of GVA)`)]

paste("Valores extremos del porcentaje de empleados en el sector Agricultura")

# Comprobemos primero los outliers de esta columna
boxplot.stats(new_dataset$`Employment: Agriculture (% of employed)`)$out

# Excepto los valores -99, todos son razonables desde un punto de vista económico. Por tanto, los mantenemos.
# Por otro lado, la columna dispone de campos -99 (valores perdidos). 
# Procedemos a comprobar de qué regiones se trata, y modificar los campos.
new_dataset$country[new_dataset$`Employment: Agriculture (% of employed)` == -99.000000]
new_dataset$Region[new_dataset$`Employment: Agriculture (% of employed)` == -99.000000]
new_dataset$`Employment: Agriculture (% of employed)`[new_dataset$`Employment: Agriculture (% of employed)` == -99.000000] <- NA

# Ahora, los reemplazamos por la media de Employed in Agriculture de su región respectiva:
new_dataset$`Employment: Agriculture (% of employed)`[is.na(new_dataset$`Employment: Agriculture (% of employed)`) & new_dataset$Region == "Polynesia"] <- mean(!is.na(new_dataset$`Employment: Agriculture (% of employed)`) & new_dataset$Region == "Polynesia")
new_dataset$`Employment: Agriculture (% of employed)`[is.na(new_dataset$`Employment: Agriculture (% of employed)`) & new_dataset$Region == "Caribbean"] <- mean(!is.na(new_dataset$`Employment: Agriculture (% of employed)`) & new_dataset$Region == "Caribbean")
new_dataset$`Employment: Agriculture (% of employed)`[is.na(new_dataset$`Employment: Agriculture (% of employed)`) & new_dataset$Region == "SouthernEurope"] <- mean(!is.na(new_dataset$`Employment: Agriculture (% of employed)`) & new_dataset$Region == "SouthernEurope")
new_dataset$`Employment: Agriculture (% of employed)`[is.na(new_dataset$`Employment: Agriculture (% of employed)`) & new_dataset$Region == "NorthernEurope"] <- mean(!is.na(new_dataset$`Employment: Agriculture (% of employed)`) & new_dataset$Region == "NorthernEurope")
new_dataset$`Employment: Agriculture (% of employed)`[is.na(new_dataset$`Employment: Agriculture (% of employed)`) & new_dataset$Region == "SouthAmerica"] <- mean(!is.na(new_dataset$`Employment: Agriculture (% of employed)`) & new_dataset$Region == "SouthAmerica")
new_dataset$`Employment: Agriculture (% of employed)`[is.na(new_dataset$`Employment: Agriculture (% of employed)`) & new_dataset$Region == "NorthernAmerica"] <- mean(!is.na(new_dataset$`Employment: Agriculture (% of employed)`) & new_dataset$Region == "NorthernAmerica")
new_dataset$`Employment: Agriculture (% of employed)`[is.na(new_dataset$`Employment: Agriculture (% of employed)`) & new_dataset$Region == "Micronesia"] <- mean(!is.na(new_dataset$`Employment: Agriculture (% of employed)`) & new_dataset$Region == "Micronesia")
new_dataset$`Employment: Agriculture (% of employed)`[is.na(new_dataset$`Employment: Agriculture (% of employed)`) & new_dataset$Region == "EasternAfrica"] <- mean(!is.na(new_dataset$`Employment: Agriculture (% of employed)`) & new_dataset$Region == "EasternAfrica")
new_dataset$`Employment: Agriculture (% of employed)`[is.na(new_dataset$`Employment: Agriculture (% of employed)`) & new_dataset$Region == "WesternAfrica"] <- mean(!is.na(new_dataset$`Employment: Agriculture (% of employed)`) & new_dataset$Region == "WesternAfrica")
new_dataset$`Employment: Agriculture (% of employed)`[is.na(new_dataset$`Employment: Agriculture (% of employed)`) & new_dataset$Region == "NorthernAfrica"] <- mean(!is.na(new_dataset$`Employment: Agriculture (% of employed)`) & new_dataset$Region == "NorthernAfrica")
new_dataset$`Employment: Agriculture (% of employed)`[is.na(new_dataset$`Employment: Agriculture (% of employed)`) & new_dataset$Region == "WesternAfrica"] <- mean(!is.na(new_dataset$`Employment: Agriculture (% of employed)`) & new_dataset$Region == "WesternAfrica")
new_dataset$`Employment: Agriculture (% of employed)`[is.na(new_dataset$`Employment: Agriculture (% of employed)`) & new_dataset$Region == "WesternEurope"] <- mean(!is.na(new_dataset$`Employment: Agriculture (% of employed)`) & new_dataset$Region == "WesternEurope")

# Comprobamos que no queda ningún valor NA en la columna Employment: Agriculture (% of employed)
new_dataset$country[is.na(new_dataset$`Employment: Agriculture (% of employed)`)]

paste("Valores extremos del porcentaje de empleados en el sector industria:")

# Comprobemos primero los outliers de esta columna
boxplot.stats(new_dataset$`Employment: Industry (% of employed)`)$out

# Los únicos outliers detectados son campos de valores perdidos (-99).
# Procedemos a comprobar de qué regiones se trata, y modificar los campos.
new_dataset$country[new_dataset$`Employment: Industry (% of employed)` == -99.000000]
new_dataset$Region[new_dataset$`Employment: Industry (% of employed)` == -99.000000]
new_dataset$`Employment: Industry (% of employed)`[new_dataset$`Employment: Industry (% of employed)` == -99.000000] <- NA

# Ahora, los reemplazamos por la media de Employed in Industry de su región respectiva:
new_dataset$`Employment: Industry (% of employed)`[is.na(new_dataset$`Employment: Industry (% of employed)`) & new_dataset$Region == "Polynesia"] <- mean(!is.na(new_dataset$`Employment: Industry (% of employed)`) & new_dataset$Region == "Polynesia")
new_dataset$`Employment: Industry (% of employed)`[is.na(new_dataset$`Employment: Industry (% of employed)`) & new_dataset$Region == "Caribbean"] <- mean(!is.na(new_dataset$`Employment: Industry (% of employed)`) & new_dataset$Region == "Caribbean")
new_dataset$`Employment: Industry (% of employed)`[is.na(new_dataset$`Employment: Industry (% of employed)`) & new_dataset$Region == "SouthernEurope"] <- mean(!is.na(new_dataset$`Employment: Industry (% of employed)`) & new_dataset$Region == "SouthernEurope")
new_dataset$`Employment: Industry (% of employed)`[is.na(new_dataset$`Employment: Industry (% of employed)`) & new_dataset$Region == "NorthernEurope"] <- mean(!is.na(new_dataset$`Employment: Industry (% of employed)`) & new_dataset$Region == "NorthernEurope")
new_dataset$`Employment: Industry (% of employed)`[is.na(new_dataset$`Employment: Industry (% of employed)`) & new_dataset$Region == "SouthAmerica"] <- mean(!is.na(new_dataset$`Employment: Industry (% of employed)`) & new_dataset$Region == "SouthAmerica")
new_dataset$`Employment: Industry (% of employed)`[is.na(new_dataset$`Employment: Industry (% of employed)`) & new_dataset$Region == "NorthernAmerica"] <- mean(!is.na(new_dataset$`Employment: Industry (% of employed)`) & new_dataset$Region == "NorthernAmerica")
new_dataset$`Employment: Industry (% of employed)`[is.na(new_dataset$`Employment: Industry (% of employed)`) & new_dataset$Region == "Micronesia"] <- mean(!is.na(new_dataset$`Employment: Industry (% of employed)`) & new_dataset$Region == "Micronesia")
new_dataset$`Employment: Industry (% of employed)`[is.na(new_dataset$`Employment: Industry (% of employed)`) & new_dataset$Region == "EasternAfrica"] <- mean(!is.na(new_dataset$`Employment: Industry (% of employed)`) & new_dataset$Region == "EasternAfrica")
new_dataset$`Employment: Industry (% of employed)`[is.na(new_dataset$`Employment: Industry (% of employed)`) & new_dataset$Region == "WesternAfrica"] <- mean(!is.na(new_dataset$`Employment: Industry (% of employed)`) & new_dataset$Region == "WesternAfrica")
new_dataset$`Employment: Industry (% of employed)`[is.na(new_dataset$`Employment: Industry (% of employed)`) & new_dataset$Region == "NorthernAfrica"] <- mean(!is.na(new_dataset$`Employment: Industry (% of employed)`) & new_dataset$Region == "NorthernAfrica")
new_dataset$`Employment: Industry (% of employed)`[is.na(new_dataset$`Employment: Industry (% of employed)`) & new_dataset$Region == "WesternEurope"] <- mean(!is.na(new_dataset$`Employment: Industry (% of employed)`) & new_dataset$Region == "WesternEurope")

# Comprobamos que no queda ningún valor NA en la columna Employment: Industry (% of employed)
new_dataset$country[is.na(new_dataset$`Employment: Industry (% of employed)`)]

paste("Valores extremos del porcentaje de empleados en sector servicios:")

# Comprobemos primero los outliers de esta columna
boxplot.stats(new_dataset$`Employment: Services (% of employed)`)$out

# Los únicos outliers detectados son campos de valores perdidos (-99).
# Procedemos a comprobar de qué regiones se trata, y modificar los campos.
new_dataset$country[new_dataset$`Employment: Services (% of employed)` == -99.000000]
new_dataset$Region[new_dataset$`Employment: Services (% of employed)` == -99.000000]
new_dataset$`Employment: Services (% of employed)`[new_dataset$`Employment: Services (% of employed)` == -99.000000] <- NA

# Ahora, los reemplazamos por la media de Employed in Services de su región respectiva:
new_dataset$`Employment: Services (% of employed)`[is.na(new_dataset$`Employment: Services (% of employed)`) & new_dataset$Region == "Polynesia"] <- mean(!is.na(new_dataset$`Employment: Services (% of employed)`) & new_dataset$Region == "Polynesia")
new_dataset$`Employment: Services (% of employed)`[is.na(new_dataset$`Employment: Services (% of employed)`) & new_dataset$Region == "Caribbean"] <- mean(!is.na(new_dataset$`Employment: Services (% of employed)`) & new_dataset$Region == "Caribbean")
new_dataset$`Employment: Services (% of employed)`[is.na(new_dataset$`Employment: Services (% of employed)`) & new_dataset$Region == "SouthernEurope"] <- mean(!is.na(new_dataset$`Employment: Services (% of employed)`) & new_dataset$Region == "SouthernEurope")
new_dataset$`Employment: Services (% of employed)`[is.na(new_dataset$`Employment: Services (% of employed)`) & new_dataset$Region == "NorthernEurope"] <- mean(!is.na(new_dataset$`Employment: Services (% of employed)`) & new_dataset$Region == "NorthernEurope")
new_dataset$`Employment: Services (% of employed)`[is.na(new_dataset$`Employment: Services (% of employed)`) & new_dataset$Region == "SouthAmerica"] <- mean(!is.na(new_dataset$`Employment: Services (% of employed)`) & new_dataset$Region == "SouthAmerica")
new_dataset$`Employment: Services (% of employed)`[is.na(new_dataset$`Employment: Services (% of employed)`) & new_dataset$Region == "NorthernAmerica"] <- mean(!is.na(new_dataset$`Employment: Services (% of employed)`) & new_dataset$Region == "NorthernAmerica")
new_dataset$`Employment: Services (% of employed)`[is.na(new_dataset$`Employment: Services (% of employed)`) & new_dataset$Region == "Micronesia"] <- mean(!is.na(new_dataset$`Employment: Services (% of employed)`) & new_dataset$Region == "Micronesia")
new_dataset$`Employment: Services (% of employed)`[is.na(new_dataset$`Employment: Services (% of employed)`) & new_dataset$Region == "EasternAfrica"] <- mean(!is.na(new_dataset$`Employment: Services (% of employed)`) & new_dataset$Region == "EasternAfrica")
new_dataset$`Employment: Services (% of employed)`[is.na(new_dataset$`Employment: Services (% of employed)`) & new_dataset$Region == "WesternAfrica"] <- mean(!is.na(new_dataset$`Employment: Services (% of employed)`) & new_dataset$Region == "WesternAfrica")
new_dataset$`Employment: Services (% of employed)`[is.na(new_dataset$`Employment: Services (% of employed)`) & new_dataset$Region == "NorthernAfrica"] <- mean(!is.na(new_dataset$`Employment: Services (% of employed)`) & new_dataset$Region == "NorthernAfrica")
new_dataset$`Employment: Services (% of employed)`[is.na(new_dataset$`Employment: Services (% of employed)`) & new_dataset$Region == "WesternEurope"] <- mean(!is.na(new_dataset$`Employment: Services (% of employed)`) & new_dataset$Region == "WesternEurope")

# Comprobamos que no queda ningún valor NA en la columna Employment: Services (% of employed)
new_dataset$country[is.na(new_dataset$`Employment: Services (% of employed)`)]

paste("Valores extremos de crecimiento de población:")

# En esta columna, no observamos valores extremos. Además, media y mediana son similares.
# Sin embargo, sí que disponemos de 7 observaciones NA.
# Procedemos a asignar la media de la región para estas observaciones.
new_dataset$country[is.na(new_dataset$`Population growth rate (average annual %)`)]
new_dataset$Region[is.na(new_dataset$`Population growth rate (average annual %)`)]

new_dataset$`Population growth rate (average annual %)`[is.na(new_dataset$`Population growth rate (average annual %)`) & new_dataset$Region == "Polynesia"] <- mean(!is.na(new_dataset$`Population growth rate (average annual %)`) & new_dataset$Region == "Polynesia")
new_dataset$`Population growth rate (average annual %)`[is.na(new_dataset$`Population growth rate (average annual %)`) & new_dataset$Region == "Caribbean"] <- mean(!is.na(new_dataset$`Population growth rate (average annual %)`) & new_dataset$Region == "Caribbean")
new_dataset$`Population growth rate (average annual %)`[is.na(new_dataset$`Population growth rate (average annual %)`) & new_dataset$Region == "SouthernEurope"] <- mean(!is.na(new_dataset$`Population growth rate (average annual %)`) & new_dataset$Region == "SouthernEurope")
new_dataset$`Population growth rate (average annual %)`[is.na(new_dataset$`Population growth rate (average annual %)`) & new_dataset$Region == "EasternEurope"] <- mean(!is.na(new_dataset$`Population growth rate (average annual %)`) & new_dataset$Region == "EasternEurope")
new_dataset$`Population growth rate (average annual %)`[is.na(new_dataset$`Population growth rate (average annual %)`) & new_dataset$Region == "NorthernAmerica"] <- mean(!is.na(new_dataset$`Population growth rate (average annual %)`) & new_dataset$Region == "NorthernAmerica")

# Comprobamos que no queda ningún valor NA en la columna de Population Growth Rate
new_dataset$country[is.na(new_dataset$`Population growth rate (average annual %)`)]

paste("Valores extremos de porcentaje de población urbana:")

# En la columna de Urban Population (%), no hay una diferencia significativa entre media y mediana.
# Además, no se encuentran valores NA.
# Sin embargo, el mínimo (0%) y el máximo (100%) parecen muy extremos. Comprobemos los países.
# Empezamos por aquellos con una proporción de población urbana del 0%:
new_dataset$country[new_dataset$`Urban population (% of total population)` == 0]
# Tras una comprobación manual, estos dos países se han introducido en el dataset correctamente.
# Pasemos ahora a los territorios con una población urbana del 100%:
new_dataset$country[new_dataset$`Urban population (% of total population)` == 100]
# De nuevo, los datos parecen estar en línea con los que se pueden estudiar en el data.worldbank.org.
# Por tanto, se concluye que esta columna no presenta outliers.

paste("Valores extremos de gasto en educación sobre GDP:")

# Comprobemos primero los outliers de esta columna
boxplot.stats(new_dataset$`Education: Government expenditure (% of GDP)`)$out

# Todos los outliers son valores perdidos (-99).
# Procedemos a comprobar de qué regiones se trata, y modificar los campos.
new_dataset$country[new_dataset$`Education: Government expenditure (% of GDP)` == -99]
# NOTA: Vuelve a haber valores NA extraños, comprobar para la entrega final.
new_dataset$Region[new_dataset$`Education: Government expenditure (% of GDP)` == -99]
new_dataset$`Education: Government expenditure (% of GDP)`[new_dataset$`Education: Government expenditure (% of GDP)` == -99] <- NA

new_dataset$`Education: Government expenditure (% of GDP)`[is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "Polynesia"] <- mean(!is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "Polynesia")
new_dataset$`Education: Government expenditure (% of GDP)`[is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "Caribbean"] <- mean(!is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "Caribbean")
new_dataset$`Education: Government expenditure (% of GDP)`[is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "SouthernEurope"] <- mean(!is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "SouthernEurope")
new_dataset$`Education: Government expenditure (% of GDP)`[is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "NorthernEurope"] <- mean(!is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "NorthernEurope")
new_dataset$`Education: Government expenditure (% of GDP)`[is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "SouthAmerica"] <- mean(!is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "SouthAmerica")
new_dataset$`Education: Government expenditure (% of GDP)`[is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "NorthernAmerica"] <- mean(!is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "NorthernAmerica")
new_dataset$`Education: Government expenditure (% of GDP)`[is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "CentralAmerica"] <- mean(!is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "CentralAmerica")
new_dataset$`Education: Government expenditure (% of GDP)`[is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "Micronesia"] <- mean(!is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "Micronesia")
new_dataset$`Education: Government expenditure (% of GDP)`[is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "Melanesia"] <- mean(!is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "Melanesia")
new_dataset$`Education: Government expenditure (% of GDP)`[is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "EasternAfrica"] <- mean(!is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "EasternAfrica")
new_dataset$`Education: Government expenditure (% of GDP)`[is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "WesternAfrica"] <- mean(!is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "WesternAfrica")
new_dataset$`Education: Government expenditure (% of GDP)`[is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "NorthernAfrica"] <- mean(!is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "NorthernAfrica")
new_dataset$`Education: Government expenditure (% of GDP)`[is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "SouthernAfrica"] <- mean(!is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "SouthernAfrica")
new_dataset$`Education: Government expenditure (% of GDP)`[is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "MiddleAfrica"] <- mean(!is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "MiddleAfrica")
new_dataset$`Education: Government expenditure (% of GDP)`[is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "South-easternAsia"] <- mean(!is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "South-easternAsia")
new_dataset$`Education: Government expenditure (% of GDP)`[is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "CentralAsia"] <- mean(!is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "CentralAsia")
new_dataset$`Education: Government expenditure (% of GDP)`[is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "WesternAsia"] <- mean(!is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "WesternAsia")
new_dataset$`Education: Government expenditure (% of GDP)`[is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "EasternAsia"] <- mean(!is.na(new_dataset$`Education: Government expenditure (% of GDP)`) & new_dataset$Region == "EasternAsia")

# Comprobamos que no queda ningún valor NA en la columna de gasto en educación.
new_dataset$country[is.na(new_dataset$`Education: Government expenditure (% of GDP)`)]
```

```{r analysis}

# TRAER HACIA ATRÁS PARA HACER EL CÓDIGO ANTERIOR MÁS LEGIBLE
new_dataset$population <- new_dataset$`Population in thousands (2017)`
new_dataset$pop_dens <- new_dataset$`Population density (per km2, 2017)`
new_dataset$pop_growth <- new_dataset$`Population growth rate (average annual %)`
new_dataset$urban_pop <- new_dataset$`Urban population (% of total population)`
new_dataset$gdp <- new_dataset$`GDP: Gross domestic product (million current US$)`
new_dataset$gdp_growth <- new_dataset$`GDP growth rate (annual %, const. 2005 prices)`
new_dataset$gdp_xcap <- new_dataset$`GDP per capita (current US$)`
new_dataset$eco_agri <- new_dataset$`Economy: Agriculture (% of GVA)`
new_dataset$eco_industry <- new_dataset$`Economy: Industry (% of GVA)`
new_dataset$eco_services <- new_dataset$`Economy: Services and other activity (% of GVA)`
new_dataset$empl_agri <- new_dataset$`Employment: Agriculture (% of employed)`
new_dataset$empl_industry <- new_dataset$`Employment: Industry (% of employed)`
new_dataset$empl_services <- new_dataset$`Employment: Services (% of employed)`
new_dataset$educ_exp <- new_dataset$`Education: Government expenditure (% of GDP)`

paste("Comprobación de normalidad:")

# Todas las columnas tienen más de 30 observaciones. Por tanto, según el TCL,
# estas deberían poder aproximarse a la distribución normal.
# Comprobemos los gráficos de la distribución de las columnas numéricas:
layout(matrix(c(1,2,3,4,5,
                6,7,8,9,10,
                11,12,13,14,14), nrow = 3, ncol = 5, byrow = TRUE))
plot(density(new_dataset$population), main = "Population")
plot(density(new_dataset$pop_dens), main = "Pop_Density")
plot(density(new_dataset$pop_growth), main = "Pop_Growth")
plot(density(new_dataset$urban_pop), main = "Urban_Pop")
plot(density(new_dataset$gdp), main = "GDP")
plot(density(new_dataset$gdp_growth), main = "GDP_Growth")
plot(density(new_dataset$gdp_xcap), main = "GDP x cap")
plot(density(new_dataset$eco_agri), main = "Eco_Agri")
plot(density(new_dataset$eco_industry), main = "Eco_Industry")
plot(density(new_dataset$eco_services), main = "Eco_Services")
plot(density(new_dataset$empl_agri), main = "Empl_Agri")
plot(density(new_dataset$empl_industry), main = "Empl_Industry")
plot(density(new_dataset$empl_services), main = "Empl_Services")
plot(density(new_dataset$educ_exp), main = "Educ_Exp")

# Algunas columnas son representadas con cierta asimetría,
# aunque esto parece consecuencia de los outliers que hemos dejado.

# Comprobemos el gráfico Q-Q de cada variable.
# Con estos, podremos observar si tan solo los outliers se salen de los patrones.
layout(matrix(c(1,2,3,4,5,
                6,7,8,9,10,
                11,12,13,14,14), nrow = 3, ncol = 5, byrow = TRUE))
qqnorm(new_dataset$population, main = "Population")
qqline(new_dataset$population, col = "red")
qqnorm(new_dataset$pop_dens, main = "Pop_Density")
qqline(new_dataset$pop_dens, col = "red")
qqnorm(new_dataset$pop_growth, main = "Pop_Growth")
qqline(new_dataset$pop_growth, col = "red")
qqnorm(new_dataset$urban_pop, main = "Urban_Pop")
qqline(new_dataset$urban_pop, col = "red")
qqnorm(new_dataset$gdp, main = "GDP")
qqline(new_dataset$gdp, col = "red")
qqnorm(new_dataset$gdp_growth, main = "GDP_Growth")
qqline(new_dataset$gdp_growth, col = "red")
qqnorm(new_dataset$gdp_xcap, main = "GDP x cap")
qqline(new_dataset$gdp_xcap, col = "red")
qqnorm(new_dataset$eco_agri, main = "Eco_Agri")
qqline(new_dataset$eco_agri, col = "red")
qqnorm(new_dataset$eco_industry, main = "Eco_Industry")
qqline(new_dataset$eco_industry, col = "red")
qqnorm(new_dataset$eco_services, main = "Eco_Services")
qqline(new_dataset$eco_services, col = "red")
qqnorm(new_dataset$empl_agri, main = "Empl_Agri")
qqline(new_dataset$empl_agri, col = "red")
qqnorm(new_dataset$empl_industry, main = "Empl_Industry")
qqline(new_dataset$empl_industry, col = "red")
qqnorm(new_dataset$empl_services, main = "Empl_Services")
qqline(new_dataset$empl_services, col = "red")
qqnorm(new_dataset$empl_services, main = "Educ_Exp")
qqline(new_dataset$empl_services, col = "red")

# Más o menos, los puntos de todas las columnas siguen la distribución
# marcada por la QQline.
# Preguntar a Diego si consideraría que, por ejemplo, GDP x cap no sigue
# dist normal. Bajo nuestra perspectiva, solo por el simple TCL ya debería.

paste("Regression model to explain GDP per capita:")

# Suavicemos primero la skewness de la variable dependiente.
new_dataset$gdp_xcap <- sqrt(new_dataset$gdp_xcap)

# Ahora, creamos el modelo de regresión.
lm_gdp_xcap <- lm(gdp_xcap ~ eco_agri + eco_industry + eco_services + empl_agri + empl_industry + empl_services, data = new_dataset)
summary(lm_gdp_xcap)

# Excepto por el peso de la agricultura en la economía,
# el resto de variables contribuyen a explicar GDP_xcap.
# Además, el modelo explica el 60% de variabilidad de la variable GDP x cap.

# Comprobación de homocedasticidad:
plot(lm_gdp_xcap)
```
