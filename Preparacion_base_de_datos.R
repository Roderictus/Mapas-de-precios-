library(tidyverse)
library(reshape)
library(lubridate)
library(ggthemes)
#library(ggfortify)# no está instalado aún, no queda claro que se vaya a ocupar
library(readxl)
library(stringr)
library(leaflet)

#######################################################################################################
#######################################################################################################
Colnom <-c("Compañia","Permiso", "Nombre Estación", 
           "Dirección", "Tipo", "Tipo2",
           "Precio", "Fecha", "Estado", 
           "Municipio", "Fecha2", "N")
StreetPrice <- read.csv(file = "Data/Street Prices CRE May 2018 - May 2020.csv", 
                        header = FALSE, col.names = Colnom, encoding = "UTF-8")
########################################################################################
#Subset del 20200604
PERM <- read_xlsx(path = "Data/Permisos CRE para BASE 2020 06 03 v1.xlsx", 
                  col_names = FALSE)

#16 oct 18, 12 mar 19, 10 mar 20
fechas <- c("2018-08-18","2019-03-12","2020-03-10")
fechas <- ymd(fechas)

StreetPrice$Fecha2 <- ymd(StreetPrice$Fecha2) #para cambiar esta columna a tipo fecha 

ISOBase <- StreetPrice %>% 
  filter(Permiso %in% temp ) %>%
  filter( Tipo2 %in% c("Premium", "Regular")) %>%
  filter( Fecha2 %in% fechas)

ISOBase <- ISOBase %>% select(-Fecha, -N,)

write.csv(x = ISOBase, file = "Subset_2020_06_04.csv")

#Otro subset del 20200604
PERM <- read_xlsx(path = "Entregas/Faltantes Leon y Toluca.xlsx", col_names = FALSE)
temp <- sapply(X = PERM, FUN = as.character)
temp <- as.character(temp)

ISOBase <- StreetPrice %>% 
  filter(Permiso %in% temp ) %>%
  filter( Tipo2 %in% c("Premium", "Regular"))

ISOBase <- ISOBase %>% select(-Fecha, -N,)

write.csv(x = ISOBase, file = "Subset_2020_06_04_Dos_Leon_Toluca.csv")



DosF <-  StreetPrice %>% filter(Fecha2 == "2018-10-19")
#DosFB <- StreetPrice %>% filter(Fecha == "2018-10-31T00:00:00" | Fecha == "2019-03-31T00:00:00") # Esta es una fecha que el sistema utiliza para una carga, ignorar
Perm <-"PL/3717/EXP/ES/2015"
Fech <- "2020-03-18"
PL3717 <- StreetPrice %>% filter(Permiso == Perm & Fecha2 == Fech)
Permisos <-unique(StreetPrice$Permiso)
write.csv(x = PL3717, file = "Permisos.csv")
DosF <- DosF %>% select(-N, Fecha)
write.csv(x = DosF, file = "DF20181031_20190331.csv")

#############################################################
##############      Loop para hacerlo legible     ###########
#############################################################
setwd("Data/Bases Manejables/")
j = 1   #recordar resetear j = 1
for (i in 1:21) {
  print(i)
  print(j)
  temp      <- StreetPrice[j:(j+1000000),]
  print(head(temp, 1))
  print(tail(temp, 1))
  j         <- j + 1000000
  print(j)
  Principio <- head(temp$Fecha2, n = 1)
  Fin       <- tail(temp$Fecha2, n = 1)
  fecha     <- str_c(Principio, Fin, sep = "_")
  name      <- str_c("Base_", fecha)
  print(name)
  write.csv(x = temp, file = name)
}

temp <-StreetPrice[21000001:nrow(StreetPrice), ]
Principio <- head(temp$Fecha2, n = 1)
Fin       <- tail(temp$Fecha2, n = 1)
fecha     <- str_c(Principio, Fin, sep = "_")
name      <- str_c("Base_", fecha)
print(name)
write.csv(x = temp, file = name)

##############################################################
#####       Base Costco     ################################
##############################################################

StreetPrice %>% filter()

G2018 <-  StreetPrice %>% filter(year(Fecha2) == "2018")
#empatarlo con las direcciones
head(GasDF) #CRE_Id
HEAD(G2018) #
#unificar el nombre de la variable por la que se unen
full_join(G2018, GasDf, by ="")

#Base sólo con los permisos de costco
VCostco <- c("PL/20238/EXP/ES/2017","PL/21005/EXP/ES/2018", "PL/21114/EXP/ES/2018","PL/21361/EXP/ES/2018",
             "PL/21361/EXP/ES/2018","PL/21441/EXP/ES/2018","PL/21879/EXP/ES/2018","PL/22362/EXP/ES/2019",
             "PL/22363/EXP/ES/2019","PL/22364/EXP/ES/2019","PL/23213/EXP/ES/2020")
BCostco <- StreetPrice %>% filter(Permiso %in% VCostco )
unique(BCostco$Permiso)# 5: PL/20238/EXP/ES/2017 PL/21005/EXP/ES/2018 PL/21114/EXP/ES/2018 PL/21441/EXP/ES/2018 PL/21879/EXP/ES/2018
unique(BCostco$Estado)#Sinaloa              Coahuila de Zaragoza Guanajuato           Baja California      Coahuila de Zaragoz  Puebla      
#unir con los datos georeferenciados
#Buscar estaciones que Tengan costco en el nombre, GasDF tiene 1279 estaciones

Loccostco <- GasDF %>% filter(CRE_Id %in% VCostco)# son 10 3 de ellas sin coordenadas, 3 permisos de 2019, un permiso de 2020

colnames(Loccostco)[2] <- "Permiso"
#juntar las bases
CLCostco <- full_join(x = Loccostco, y = BCostco, "Permiso")
head(CLCostco)

#Bases que sólo tienen una entrada
UnaE <- c("PL/21361/EXP/ES/2018", "PL/22362/EXP/ES/2019", "PL/22363/EXP/ES/2019",  "PL/22364/EXP/ES/2019", "PL/23213/EXP/ES/2020" )
StreetPrice$Fecha2 <- ymd(StreetPrice$Fecha2) #transformando de factor a fecha 
BCostco$Fecha2 <- ymd(BCostco$Fecha2)#cambiar a algo que funcione como fecha
write.csv(x = BCostco, file = "Data/Base_Costco.csv")
Regular <- BCostco %>% filter(Tipo2 == "Regular")

theme_set(theme_classic())
ggplot(mpg, aes(cty, hwy)) + geom_point() + facet_grid(year ~.)
#######################################################################################################
################          Nuevo Subset      ##################################

SS <- read_xlsx(path = "Data/Costco_EstacionesServicio subsetsubset.xlsx")
ISOID <- SS %>% select(ID) #son 1704
temp <- sapply(X = ISOID, FUN = as.character)
temp <- as.character(temp)


StreetPrice$Permiso
ISOID <-as.character(x = ISOID)

table(ISOBase$Tipo2)

ISOBase <- StreetPrice %>% filter(Permiso %in% temp & Tipo2 %in% c("Premium", "Regular"))

write.csv(x = ISOBase, file = "Data/Bases Manejables/Subset_20200601.csv")

head(temp)
#######################################################################################################
# Nuevo Subset Jaime  2020_06_03

EstF <- read_xlsx(path = "Data/est faltantes (Costco) 2020 06 03 v1.xlsx") #24 filas
ISOID <- EstF[1]
temp <- sapply(X = ISOID, FUN = as.character)
temp <- as.character(temp)

EstF <- StreetPrice %>% filter(Permiso %in% temp)

write.csv(x = EstF, file = "Estaciones Faltantes 2020 06 03.csv")


#######################################################################################################
#BCostco %>% filter(Fecha2 >= "2020-03-01" & Tipo2 == "Regular") %>% #reduzcamoslo a un periodo reciente 
BCostco  %>% filter(Fecha2 >= "2020-02-15") %>%
  ggplot(aes(x=Fecha2, y = Precio, col = Municipio)) + 
  geom_line () + facet_grid(Tipo2 ~.) 

#vector con los municipios de los que queremos obtener un promedio 
unique(BCostco$Municipio )
table(BCostco$Municipio )
VMun <- c("Celaya", "Culiacán", "Mexicali", "Puebla", "Saltillo")
PromMun <- StreetPrice %>% filter(Fecha2 >= "2020-02-15" & Municipio %in% VMun & Tipo2 == "Regular") %>% 
  group_by(Municipio, Fecha2) %>%
  summarise(PromMun = mean(Precio)) 

  
#graficar Promedios Municipales contra CostCo
#O diferencia del precio de costco respecto al promedio
#O un lag en el cambio de precio 
#Buscar incluir el precio de la TAR
#Pedir datos de TAR a la CRE

#######################################################################################################
#######################################################################################################
#Base de isocronas
StreetPrice <- StreetPrice %>% select(-N, -Fecha)

ISO <-read_xlsx(path = "Data/Intersect_IsocronasCostco_EstacionesServicio (1).xlsx")
ISOID <- ISO %>% select(ID) #son 1704
temp <- sapply(X = ISOID, FUN = as.character)
temp <- as.character(temp)

StreetPrice$Permiso
ISOID <-as.character(x = ISOID)
ISOBase <- StreetPrice %>% filter(Permiso %in% temp)
head(ISOBase)
write.csv(x = ISOBase, file = "Data/Bases Manejables/ISOBASE.csv")


#tal vez contra promedios municipales
#o contra promedios nacionales, análisis de la varianza nacional
#variaciones del precio como %
#respuesta de entrada a un competidor
#mapa con la primera entrada
#cambios en la utilidad promedio 
#como se ve el promedio municipal.
#comparar con los promedios municipales 



head(BCostco)
uniqueBCostco
colnames(CLCostco)
unique(CLCostco$Permiso)[2]
CLCostco %>% select(Permiso, Y_Coord, X_Coord) %>% unique()


leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng= -107.4252 , lat = 24.79775, popup = "Gasolinera de Costco") %>%
  addCircleMarkers(lng= -107.4252 , lat = 24.79775, radius = 200) 




#graficado contra tiempo
#variaciones respecto a la media 

#normalización básica 




ggplot(Re)



#alguna forma de normalizar el precio 






#Tienen la misma extraté, variaciones de precio a lo largo de los meses

#Obtener TAR
#Polígonos de los Sam
#subsetear para un mes o un año


#semestres para la base de datos
#cuatro escenarios de precios
#465 estaciones de servicio
#polígono de referencia alrededor de la tienda
#revisar de las estaciones alrededor del polígono
#9 de estaciones de servicio Soriana
#Donde están situadas
#heatmaps de estados
#heatmaps de fechas
#heatmaps de mapa de la república, tal vez sólo de ciudad de México
#Dos fechas que nos interesan
# 31 octubre 2018
# 31 marzo de 2019
#periodo de dos meses
#datos básicos en un notepad
#8 fechas
#estaciones de servicio de Costco
#Costco como empresa pública tal vez muestre cuanto obtiene por gasolinas en México
#walmart hace sus propios polígonos


#Melt de bases de datos

temp <- DosF %>% spread(key= "Tipo2", value = "N")
temp <-  spread(data = DosF, key = "Tipo2", value = "Precio")
res <- dcast(melt(DosF, id.vars = "Permiso")[ value != "" ], record_numb ~ variable)
melt(temp, id.vars = "Permiso", measure.vars = c("Diesel","Premium", "Regular"))
melt(data = x, id.vars = "id", measure.vars = c("blue", "red"))

#La apertura de las gasolinas ha disminuido los precios?

Es difícil determinar la baja de los precios ya que depende de múltiples factores 




