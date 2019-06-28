library(magrittr)
library(plyr)
library(dplyr)
library(tidyverse)
library(modelr)
library(funModeling)
library(ggplot2)
library(ggcorrplot)
library(MASS)
library(GGally)
library(simstudy)
library(foreign)
library(feather)
library(data.table)
library(jtools)
library(purrr)
library(tidyr)
library(XML)
library(xml2)
library(jsonlite)
library(rjson)
library(stringi)
library(here)
library(PCAmixdata)
library(psych)
library(factoextra)

set.seed(123) 

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
# 
chefmozhours4_mod <- read.csv ("Dataset/chefmozhours4_modificado.csv", sep = ';')
chefmozhours4_mod$Days1 <- ifelse(chefmozhours4_mod$Days1 == "Mon", 1, (chefmozhours4_mod$Days1))

# se eliminan las columnas con los restantes dias de la semana (tue-fri) y las filas repetidas

chefmozhours4_mod <- unique(chefmozhours4_mod[,-3:-6])
chefmozhours4_mod1 <- tidyr::spread(data = chefmozhours4_mod, key = Days1, value = Days1)
chefmozhours4_mod1[,2:4] <- ifelse(chefmozhours4_mod1[,2:4] == "NA" , F , T)
names(chefmozhours4_mod1) <- c("placeID", "L-V", "S", "D")


#Levantar datos de parking y guardar

chefmozparking <- read.csv ("Dataset/chefmozparking.csv", sep = ',')
chefmozparking$parking_lot [chefmozparking$parking_lot == "street"] <- "none"
chefmozparking$parking_lot <- (ifelse(chefmozparking$parking_lot == "none", F, T))


#Levantar datos de medios de pagoo aceptados y guardar
chefmozaccepts <- read.csv ("Dataset/chefmozaccepts.csv", sep = ',')
chefmozaccepts$Rpayment <- toupper(chefmozaccepts$Rpayment)
chefmozaccepts <- unique(chefmozaccepts)
chefmozacceptsTipo <- chefmozaccepts
chefmozaccepts <- tidyr::spread(data = chefmozaccepts, key = Rpayment, value = Rpayment)
print(colnames(chefmozaccepts))
vars_to_keep <- c("placeID",  "AMERICAN_EXPRESS", "BANK_DEBIT_CARDS", "CASH", "MASTERCARD-EUROCARD", "VISA")
chefmozaccepts <- chefmozaccepts[,vars_to_keep]
chefmozaccepts[,2:6] <- ifelse(chefmozaccepts[,2:6] == "NA" , 0 , 1) 

# Se calcula la cantidad medios de pagos por cada restaurante
chefmozacceptsTipo <-chefmozacceptsTipo%>%
                      group_by(placeID)%>%
                      count(placeID)

#hefmozacceptsTipo2 <- tapply(chefmozacceptsTipo$placeID, chefmozacceptsTipo$placeID, count)


#Levantar ratings y guardar

ratings <- read.csv ("Dataset/rating_final.csv", sep = ',')
ratings <- ratings[,-1]
ratings <- aggregate(ratings[, 2:4], list(ratings$placeID), mean)
names(ratings) <- c("placeID", "rating", "food_rating", "service_rating")



#Levantar geoplaces y guardar

geoplaces <- read.csv ("Dataset/geoplaces2.csv", sep = ',')
geoplaces1 <- geoplaces[,-(2:11)]
geoplaces1 <- geoplaces1[,-7]


general <- left_join( chefmozhours4_mod1, chefmozcuisine_clasificado, by="placeID")
general <- left_join( general, chefmozparking, by="placeID")
general <- left_join( general, chefmozaccepts, by="placeID")
general <- left_join( general, ratings, by="placeID")
general <- left_join( geoplaces1, general, by="placeID")
general <- general[,-(16:20)]




general <- lapply(general, as.numeric)
general <- as.data.frame(general)





# calculamos la matriz de correlacion 

general  %>%
  na.omit() %>%
  select_if(is.numeric)%>%
  as.matrix() %>%
  rcorr


# graficamosla matriz de correlacion


is.na(general) <- sapply(general, is.infinite)
general[is.na(general)] <- 0
#general[is.nan(general)] <- 0

 general %>%
   na.omit() %>%
   select_if(is.numeric) %>%
   cor %>%
   ggcorrplot(type = "lower", ggtheme = theme_minimal, colors = c("#6D9EC1","white","#E46726"),
              show.diag = T,
              lab = T, lab_size = 3,
              title = "Correlation Matrix",
              legend.title = "Correlation Value",
              outline.color = "white",
              hc.order = T)



 cluster_kmeans <- kmeans(na.omit(general), 5, 1000, 50)
 print(cluster_kmeans)
 aggregate(general, by=list(cluster=cluster_kmeans$cluster), mean)
 
 calcular_totwithinss <- function(n_clusters, datos, iter.max=1000, nstart=50){
   # Esta función aplica el algoritmo kmeans y devuelve la suma total de
   # cuadrados internos.
   cluster_kmeans <- kmeans(centers = n_clusters, x = datos, iter.max = iter.max,
                            nstart = nstart)
   return(cluster_kmeans$tot.withinss)
 }
 
 
 #que es 1:15?
 
 total_withinss <- purrr::map_dbl(.x = 1:15,
                                  .f = calcular_totwithinss,
                                  datos = general)
 
 total_withinss
 data.frame(n_clusters = 1:15, suma_cuadrados_internos = total_withinss) %>%
   ggplot(aes(x = n_clusters, y = suma_cuadrados_internos)) +
   geom_line() +
   geom_point() +
   scale_x_continuous(breaks = 1:15) +
   labs(title = "Evolución de la suma total de cuadrados intra-cluster") +
   theme_bw()
 
 
 
 km_clusters <- kmeans(x = general, centers = 9, nstart = 80)
 
 # Las funciones del paquete factoextra emplean el nombre de las filas del
 # dataframe que contiene los datos como identificador de las observaciones.
 # Esto permite añadir labels a los gráficos.
 fviz_cluster(object = km_clusters, data = general, show.clust.cent = TRUE,
              ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
   labs(title = "Resultados clustering K-means") +
   theme_bw() +
   theme(legend.position = "none")
 
 
 
 #evaluacion
 
 eclust_km_clusters <- eclust(x = general, FUNcluster = "kmeans", k = 9, seed = 123,
                              hc_metric = "euclidean", nstart = 50, graph = FALSE)
 fviz_silhouette(sil.obj = eclust_km_clusters, print.summary = TRUE, palette = "jco",
                 ggtheme = theme_classic()) 
 
 