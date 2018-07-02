# Se cargan los paquetes necesarios: twitteR, ggmap
library(ggmap)
library(ggplot2)
library(tmap)
library(tmaptools)
library(stringr)
library(stringi)
library(rgdal)
library(sp)

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


