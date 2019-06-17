
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


#Levantar datos de dias de atencion y guardar


chefmozhours4_mod <- read.csv ("Dataset/chefmozhours4_modificado.csv", sep = ';')
chefmozhours4_mod$Days1 <- ifelse(chefmozhours4_mod$Days1 == "Mon", 1, (chefmozhours4_mod$Days1))

# se eliminan las columnas con los restantes dias de la semana (tue-fri) y las filas repetidas

chefmozhours4_mod <- unique(chefmozhours4_mod[,-3:-6])


#Levantar datos de parking y guardar


chefmozparking <- read.csv ("Dataset/chefmozparking.csv", sep = ',')
chefmozparking$parking_lot [chefmozparking$parking_lot == "street"] <- "none"
chefmozparking$parking_lot <- (ifelse(chefmozparking$parking_lot == "none", 0, 1))


