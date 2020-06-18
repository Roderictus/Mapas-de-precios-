#manejo de los archivos de pdf

library(quanteda)
library(pdfsearch)

temp <- readtext::readtext(file = "Data/Precios diarios TAR/20200615.pdf")

head(temp)
temp$text


#Precios de venta en TAR aplicables a las gasolinas y el diésel 

inicio <-"Precios de venta en TAR aplicables a las gasolinas y el diésel" 
fin <-   "NOM-016-CRE-2016"

inicio <- str_locate_all(string = temp, pattern = inicio ) #nos interesan dos páginas después de este punto
fin <- str_locate_all(string = temp, pattern = fin) #nos interesan dos páginas después de este punto

inicio <- inicio[[1]][2] #esto es donde termina la oración 14,583
fin    <- fin[[1]][2] #el principio de la segunda ocasión donde aparece el texto

#seleccionar este rango en el archivo
strsplit(x = temp, split = 10,fixed = TRUE)




stringr::str_extract(string = temp,
                     pattern ="Precios de venta.............................."  )



library(stringr)

str_length(temp)


#vector con nombres de los estados
#después devolver información, valores despues del nombre
#recortar para sólo TAR


#fecha
#primera página de datos
#segunda página de datos
