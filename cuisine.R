
library(magrittr)
library(plyr)
library(dplyr)
library(tidyverse)
library(modelr)
library(funModeling)
library(dplyr) 
library(ggplot2)
#library(GGally)
#library(ggcorrplot)
library(foreign)
#library(feather)
#library(data.table)
library(purrr)
library(tidyr)
#library(XML)
library(xml2)
library(jsonlite)
#library(rjson)
library(stringi)
#library(here)



#Levantar datos y guardar


chefmozcuisine_original <- read.csv ("Dataset/chefmozcuisine.csv", sep = ',')

str(chefmozcuisine_original)
head(chefmozcuisine_original)


# el dataset original tiene 59 niveles. Muchos de ellos similares por lo que se decide agruparlo
# para trabajar con menos niveles y obtener mejores resultados a la hora de realizar el cluster


chefmozcuisine_tabla_clasificacion <- read.csv ("Dataset/Cuisine_agrupado.csv", sep = ';')

# a continuacion se unen ambas tablas conservando todas las filas del dataset original y agregando 
# la columna con la nueva clasificacion.

chefmozcuisine_clasificado <- left_join( chefmozcuisine_original, chefmozcuisine_tabla_clasificacion, by="Rcuisine")

# adicionalmente se elimina la columna con la clasificacion anterior y todas las filas que quedaron 
# con igual clasificacion para el mismo ID

chefmozcuisine_clasificado <- unique(chefmozcuisine_clasificado[,-2])

# algunos placeID tienen mas de una propuesta. Considerando que ya se hizo el trabajo anterior de
# reagrupar los similares se decide dejar los que luego de esta modificacion mantienen mas de una propuesta

str(chefmozcuisine_clasificado)
head(chefmozcuisine_clasificado)


