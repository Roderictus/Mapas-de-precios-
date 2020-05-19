# Paquetes
library(leaflet)
library(tidyverse)
library(readxl)
library(XML)
library(httr)
# Cargar datos de precios
getwd()
temp <- read_xlsx(path = "data/Muestra Precios Venta la Público vigentes 2020_05_16 (1).xlsx")

# Cargar datos de ubicación

temp_ubik <- xmlParse(file = "Data/places.xml")

head(xmlRoot(temp_ubik))

ubik_node <- xmlRoot(temp_ubik)

xpathSApply(ubik_node, "//place/name", xmlValue, encoding = "UTF-8")   #Nombre de la estación 
xpathSApply(ubik_node, "//place/cre_id", xmlValue, encoding = "UTF-8") #CRE_Id
xpathSApply(ubik_node, "//place/location/x", xmlValue, encoding = "UTF-8") #X
xpathSApply(ubik_node, "//place/location/y", xmlValue, encoding = "UTF-8") #Y

GasDF <- data.frame(Estacion = xpathSApply(ubik_node, "//place/name", xmlValue, encoding = "UTF-8"),
           CRE_Id   = xpathSApply(ubik_node, "//place/cre_id", xmlValue, encoding = "UTF-8"),
           X_Coord  =  xpathSApply(ubik_node, "//place/location/x", xmlValue, encoding = "UTF-8"),
           Y_Coord  = xpathSApply(ubik_node, "//place/location/y", xmlValue, encoding = "UTF-8"))

############      Precios     ##############
head(temp)
head(GasDF)
temp$Numero #31,194 , tres precios 10398
GasDF$CRE_Id #12,709
unique(GasDF$CRE_Id)

library(tidyr)
pivot_wider(data = temp, value = )


#mapeando precios de la gasolina
#mapas de calor por municipio estados
#cercania con las centrales de suministro





doc_ubik <- content(temp_ubik, as = "text", encoding = "UTF-8")

doc <- content(temp_ubik, as="text", encoding="UTF-8")




library(XML)
library(httr)

# found this XML with hebrew
tmp <- GET("https://tiktickets.googlecode.com/svn-history/r102/trunk/war/ShowHalls.xml")
doc <- content(tmp, as="text", encoding="UTF-8")
doc <- substr(doc, 2, nchar(doc)) # skip encoding bits at the beginning

doc_x <- xmlParse(doc, encoding="UTF-8")

# do data frame conversion by hand




require(XML)
data <- xmlParse("http://forecast.weather.gov/MapClick.php?lat=29.803&lon=-82.411&FcstType=digitalDWML")

xml_data <- xmlToList(data)
In the case of your example data, getting location and start time is fairly straightforward:
  
  location <- as.list(xml_data[["data"]][["location"]][["point"]])

start_time <- unlist(xml_data[["data"]][["time-layout"]][
  names(xml_data[["data"]][["time-layout"]]) == "start-valid-time"])

library("XML")
library("methods")

# Convert the input xml file to a data frame.
xmldataframe <- xmlToDataFrame("input.xml")
print(xmldataframe)


# Subir con leaflet
# Mapas regionales 