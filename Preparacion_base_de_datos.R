#Leer el archivo
#Subset de las fechas
#Cambiar el formato 
library(tidyverse)


StreetPrice <- read.csv(file = "Data/Street Prices CRE May 2018 - May 2020.csv", 
                        header = FALSE, col.names = Colnom, encoding = "UTF-8")
head(StreetPrice)
#vector con el nombre de las columnas, 12 variables
#LAtina o algo que permita acentos
Colnom <-c("Compañia","Permiso", "Nombre Estación", 
           "Dirección", "Tipo", "Tipo2",
           "Precio", "Fecha", "Estado", 
           "Municipio", "Fecha2", "N")
colnames(StreetPrice)
           
#Dos fechas que nos interesan
# 31 octubre 2018
# 31 marzo de 2019

DosF <-  StreetPrice %>% filter(Fecha2 == "2018-10-31" | Fecha2 == "2019-03-31")
#DosFB <- StreetPrice %>% filter(Fecha == "2018-10-31T00:00:00" | Fecha == "2019-03-31T00:00:00")# Esta es una fecha que el sistema utiliza para una carga, ignorar

head(DosF)
table(DosFB$Fecha2)

head(DosFB)


DosF <- DosF %>% select(-N, Fecha)

head(DosF)

write.csv(x = DosF, file = "DF20181031_20190331.csv")








table(DosF$Fecha2)
temp <- DosF %>% spread(key= "Tipo2", value = "N")
temp <-  spread(data = DosF, key = "Tipo2", value = "Precio")


res <- dcast(melt(DosF, id.vars = "Permiso")[ value != "" ], record_numb ~ variable)




head(temp)

library(reshape)

colnames(temp)
melt(temp, id.vars = "Permiso", measure.vars = c("Diesel","Premium", "Regular"))


x = data.frame(
  id   = c(1, 1, 2, 2),
  blue = c(1, 0, 1, 0),
  red  = c(0, 1, 0, 1)
)



melt(data = x, id.vars = "id", measure.vars = c("blue", "red"))



head(temp)

spread()


head(DosF)


table(DosF$Permiso)




table(StreetPrice$Compañia)
head(DosF)


           
table(StreetPrice$Fecha2)

#heatmaps de estados
#heatmaps de fechas
#heatmaps de mapa de la república, tal vez sólo de ciudad de México

