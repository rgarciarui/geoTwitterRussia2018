# Se cargan los paquetes necesarios: twitteR, ggmap
library(ggmap)
library(ggplot2)
library(tmap)
library(tmaptools)
library(stringr)
library(stringi)
library(rgdal)
library(sp)

library(rgeos) 
library(raster)
library(gstat)
library(spatstat)
library(readxl)

# ajustamos el directorio de trabajo a la carpeta 'contaminación'
old_dir = getwd()
setwd("./contaminacion")

#----------------------------------------------------------

# el conjunto de los 3 ficheros necesarios para el trabajo 
# se encuentra en el directorio ya indicado y ahora se procede 
# con la lectura de los datos

# Carga del fichero “Calidad del aire. Tiempo real csv”
medidas <- read.csv('horario.csv',header=TRUE,sep=';') 

# Se filtran en una nueva variable unicamente los datos de 
# dioxido de nitrogeno (valor = 8)
medidas.no2 <- medidas[medidas$MAGNITUD==8,] 

# Se añade una nueva variable y se inicializa sin valor
medidas.no2$maxno2 <-NA 


for (i in 1:nrow(medidas.no2)){ 
  x<-c() # Vector c inicializado vacío
  
  # añade todos los valores de esas columnas para cada fila al vector (x)
  for (j in 1:24){ 
    # sprintf() genera los nombres de las columnas “H01”,”H02”,…,”H24”.
    x[j]<-medidas.no2[i,paste(sprintf("H%02d", j)) ]
  }
  
  # calcula y añade a la variable 'maxno2' el maximo de cda estación
  medidas.no2$maxno2[i]<- max(x) 
}

#----------------------------------------------------------

# En segundo lugar realizamos el tratamiento del fichero con los datos 
# de “Calidad del aire: estaciones de control”.  
# Realizamos el cambio a coordenads decimales desde las sexagesimales 
# en las que se encuentran en el fichero.

estaciones <- read_excel('informacion_estaciones_red_calidad_aire.xls', 
                         col_names=TRUE, 
                         skip=4, 
                         n_max=25)

# bucle que convierte las coordenadas sexagesimales a decimales
for (i in 1:nrow(estaciones)){ 
  long <- unlist(strsplit(estaciones$LONGITUD[i],split=c(" "))) 
  long[3]<- gsub(c("\"O"),"",long[3]) 
  long[3]<- gsub(c("''O"),"",long[3]) 
  long[3]<- gsub(c("'O"),"",long[3]) 
  estaciones$long[i] <- -1 *( as.integer(gsub("º","",long[1])) +
                              (as.double(gsub("'","",long[2])))/60 +
                              as.double(gsub(",",".",long[3]))/1200) 
  lat <- unlist(strsplit(estaciones$LATITUD[i],split=c(" "))) 
  lat[3]<- gsub(c("''N"),"",long[3]) 
  estaciones$lat[i] <- as.integer(gsub("º","",lat[1])) +
  (as.double(gsub("'","",lat[2])))/60 +
  as.double(gsub(",",".",lat[3]))/1200 
  }


# En este punto procedemos a unir los 2 dataframes y del dataframe final 
# simplificamos las variables que nos van a ser útiles para cálculos 
# posteriores y realizamos una segunda selección y simplificación

# Creamos un nuevo dataframe con la fusión de los conjuntos de datos:
# - medidas.no2
# - estaciones
# Procedemos a su fusión por los atributos:
# - medidas.no2$ESTACION
# - estaciones$NÚMERO
#
medidas.geo <- merge(medidas.no2,estaciones, by.x="ESTACION", by.y="NÚMERO") 

# Procedemos a simplicar el dataframe, tomando solo los atributos necesarios
medidas.geo.simp <- medidas.geo[,c("ESTACION","maxno2","long","lat")]


# Carga del shapefile, creación del raster y del sistema poligonal

limites <-readOGR("./DISTRITOS_ETRS89/DISTRITOS_20151002.shp") 
limites.wgd84 <- spTransform(limites, CRS("+proj=longlat +datum=WGS84")) 
limite.madrid <- gUnaryUnion(limites.wgd84) 
limite.raster <- raster(limite.madrid, res=0.001)


coordinates(medidas.geo.simp) <- 3:4


medidas.geo.simp@proj4string <- limite.madrid@proj4string


# interpolación usando IDW
gstat.parametros <- gstat(formula = maxno2~1, 
                          locations=medidas.geo.simp,set = list(idp = 2)) 
no2.idw <- interpolate(limite.raster, gstat.parametros)
no2.idwr <- raster::mask(no2.idw,limite.madrid) 
plot(no2.idwr)


# interpolado por krigeaje
gstat.parametros <- gstat(formula=maxno2~1,
                          locations=medidas.geo.simp) 
variograma <- variogram(gstat.parametros, width=0.5) 
var.teorico <-fit.variogram(variograma, vgm(c("Exp", "Ste", "Sph","Mat","Gau","Spl"))) 
var.teorico
plot(variograma, var.teorico)


# mapa interpolado
no2.ordkg <- krige(formula=maxno2~1, 
                   locations=medidas.geo.simp,newdata<- as(limite.raster,"SpatialGrid"), 
                   model=var.teorico) 
no2.okgr <- raster::mask(raster(no2.ordkg),limite.madrid) 
plot(no2.okgr)


