# Se cargan los paquetes necesarios: twitteR, ggmap
library(twitteR)
library(ggmap)
library(ggplot2)
library(tmap)
library(tmaptools)
library(stringr)
library(stringi)
library(rgdal)
library(sp)

# variables de acceso a Twitter mediante R
consumer_key <- "xGVFIywgtmdVq6NoN2FhXybqQ"
consumer_secret <- "wqEpwvlJ3NLibbHoS1qOBBR5rPFd0JF56OvzqlLLQ7x4SSt0JY"
access_token <- "287839706-L6TUQwtBfyVGXmWUyc8PNALCEBNYemBEGBQ9nMgp"
access_secret <- "95O8S7QG1GO9aVv4dWZzzGnXMnNvkIc4Kv82YNuHBYxzv"

# Esta función ajusta las funciones de handshake de autenticación 
# OAuth del paquete httr para una sesión twitteR
setup_twitter_oauth(consumer_key,consumer_secret, 
                    access_token, access_secret)

# Hashtag definitivo
searchTerm <- "#Russia2018"
#searchTerm <- "#world cup+#Russia2018"

#___________________________________________________________

# Número máximo de twitts
searchResults <- searchTwitter(searchTerm, n = 15000)

# salvamos la lista obtenida en el sistema
save(searchResults, file = "searchResults.RData", 
     compress = "bzip2", compression_level = 9)

#___________________________________________________________

# convierte la lista de resultados searchResult en un dataframe
tweetFrame <- twListToDF(searchResults)

# summario del dataframe
summary(tweetFrame)

# salvamos la lista obtenida en el sistema
save(tweetFrame, file = "tweetFrame.RData", 
     compress = "bzip2", compression_level = 9)

# lista (userInfo) a partir del listado de usuarios asociados a los tweets anteriores

# como calcularla completa da un error persistente se calcula por
# tramos y luego se une en una sola lista
userInfo <- lookupUsers(tweetFrame$screenName[1:5000])
userInfo2 <- lookupUsers(tweetFrame$screenName[5001:10000])

# Da error:
# Error in twInterfaceObj$doAPICall(paste("users", "lookup", sep = "/"),  : 
#                                    Forbidden (HTTP 403).
#userInfo3 <- lookupUsers(tweetFrame$screenName[10001:15000])

userInfo3 <- lookupUsers(tweetFrame$screenName[10001:13900])
userInfo4 <- lookupUsers(tweetFrame$screenName[13450:15000])

# fusionamos los datos en una sola lista
userInfo = c(userInfo, userInfo2, userInfo3, userInfo4)

# borramos las variables no necesarias
rm(userInfo2, userInfo3, userInfo4)

# salvamos la lista obtenida en el sistema
save(userInfo, file = "userInfo.RData", 
     compress = "bzip2", compression_level = 9)

#___________________________________________________________

# Convertimos la lista en dataFrame
userFrame <- twListToDF(userInfo)

# summario del dataframe
summary(userFrame)

#___________________________________________________________

# Filtramos el dataFrame obteniendo el listado de todos aquellos usuarios 
# que introdujeron información relativa a su localización
addresses <- userFrame[userFrame$location !="",]

# summario del dataframe
summary(addresses)

# salvamos la lista obtenida en el sistema
save(addresses, file = "addresses.RData", 
     compress = "bzip2", compression_level = 9)

#___________________________________________________________

# Gestionamos la geo localización de longitud y latitud de los twitts que tienen
# informado correctamente (o al menos que se pueda utilizar en la localización)
# el pais/localidad en el dataframe 'addresses'.
# Para ello consultamos la dirección de internet http://nominatim.openstreetmap.org
# ya que otras alternativas eran menos fiables para consultar todos los 
# twitts de nuestra lista.
# De nuestro dataframe utilizamos el campo 'location' y utilizando la función
# geocode_OSM() nos da como retorno la longitud y latitud de cada twitt

# Creamos un dataframe vacio para guardar todos los datos finales obtenidos
geocoded_02 <- data.frame()

# Manejamos un loop en el que accederemos a la función geocode_OSM()
# suministrandoles uno a uno los parámetros
for( i in 1:nrow(addresses) ){
  
  # Creamos una dataframe estandar para los datos que necesitamos obtener
  answer <- data.frame(lat=NA, long=NA, screenName = NA)
  
  # llamamos a la función geocode_OSM() pero capturando los eventos de 
  # warnings que se puedan producir
  result = try(
    geocode_OSM(addresses$location[i], 
                projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs", 
                return.first.only = TRUE, 
                details = FALSE, 
                as.data.frame = NA, 
                as.SPDF = FALSE,
                server = "http://nominatim.openstreetmap.org")
  )
  
  # si no se ha producido un warning de tipo "try_error", si se produce se descarta
  # el usuario
  if(class(result) != "try-error"){
    
    # Si el valor obtenido en resultado fuera nulo se descarta
    if(!is.null(result)){
      
      answer$lat  = result$coords[2]    # latitud
      answer$long = result$coords[1]    # longitud
      answer$screenName = addresses$screenName[i]   # usuario
      
      # añadimos los datos a nuestro dataframe de salida
      geocoded_02 <- rbind(geocoded_02, answer)
      
    }
  }
  # en este punto manejamos el tiempo de retardo para cada llamada
  # a http://nominatim.openstreetmap.org, ya que si las llamadas no tienen
  # un retrado de al manos un segundo, la pagina de nominatim nos expulsa
  # y corta la comunicación y las consultas. Se introduce un retraso de 
  # 1.2 segundos por precaución
  Sys.sleep(1.2)
}

# salvamos la lista obtenida en el sistema
save(geocoded_02, file = "geocoded_02.RData", 
     compress = "bzip2", compression_level = 9)

#___________________________________________________________

# Manejamos un loop en el que accederemos a la función rev_geocode_OSM()
# suministrandoles uno a uno los parámetros par obtener el pais, ya que 
# no tenemos en la petición original los datos de los paises.
# Creamos un nuevo dataframe, geo_paises_OSM, que sera el que contenga
# los datos finales con latitud, longitud y pais.

# Dataframe final con los datos completos que utilizaremos 
geo_paises_OSM = data.frame(lat = double(),
                            long = double(),
                            screenName = character(),
                            pais = character(), 
                            code = character(), 
                            stringsAsFactors = FALSE)

for( i in 1:nrow(geocoded_02) ){
  
  # llamamos a la función rev_geocode_OSM() pero capturando los eventos de 
  # warnings que se puedan producir
  
  # creamos un objeto SpatialPoints para suministrarlo a la función 
  # rev_geocode_OSM() con formato long:lat
  sp01 = SpatialPoints(coords = cbind(geocoded_02$long[i], geocoded_02$lat[i]),
                       proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
  result = try(
    rev_geocode_OSM(x=sp01, server = "http://nominatim.openstreetmap.org")
  )
  
  # si no se ha producido un warning de tipo "try_error", si se produce se descarta
  # el usuario
  if(class(result) != "try-error"){
    
    # Si el valor obtenido en resultado fuera nulo se descarta
    if(!is.null(result)){
      
      # añadimos los datos a nuestro dataframe de salida
      geo_paises_OSM <- rbind(geo_paises_OSM, 
                              data.frame(geocoded_02$lat[i],
                                         geocoded_02$long[i],
                                         geocoded_02$screenName[i],
                                         result[[1]]$country, 
                                         result[[1]]$country_code))
    }
  } else{
    # añadimos los datos a nuestro dataframe de salida
    geo_paises_OSM <- rbind(geo_paises_OSM, 
                            data.frame(NULL,NULL,NULL, NULL, NULL))   
  }
  # en este punto manejamos el tiempo de retardo para cada llamada
  # a http://nominatim.openstreetmap.org, ya que si las llamadas no tienen
  # un retrado de al manos un segundo, la pagina de nominatim nos expulsa
  # y corta la comunicación y las consultas. Se introduce un retraso de 
  # 1.2 segundos por precaución
  Sys.sleep(1.2)
}

# colocamos los nombres correctos en el dataframe
colnames(geo_paises_OSM) <- c("lat", "long", "screenName", "country", "country_code")
geo_paises_OSM$country_code <- toupper(geo_paises_OSM$country_code)

# salvamos la lista obtenida en el sistema
saveRDS(geo_paises_OSM, file = "geo_paises_OSM.rds", 
        compress = "bzip2")

# leemos la lista compilada directamente (RMarkdown no lo permite de otra forma)
geocoded <- readRDS(file = "geo_paises_OSM.rds")

#___________________________________________________________

# En este punto pasamos a salvar en formato RDS, este es el dataframe 
# completo geo_paises_OSM
saveRDS(geocoded,"twitter_users_geocoded2.rds") 

# En este punto pasamos a salvar en formato csv
write.table(geocoded, file="twitter_users_geocoded2.csv", 
            sep=",", 
            row.names=FALSE)

#___________________________________________________________

# De manera general creamos un mapa de visualización del conjunto de datos
# de twitts con sus coordenadas establecidas
mapaMundi <- borders("world", colour="gray60", fill="gray60") 
mapa <- ggplot() + mapaMundi 
mapa <- mapa + geom_point(aes(x=geocoded$long, y=geocoded$lat),
                          color="red", size=1)
mapa

#___________________________________________________________

# En este paso procedemos a la carga de los datos del shapefile
# paises generaliados para realizar los computos posteriores

# Carga del fichero
paises_generalizados <- readOGR("./PAISES_GENERALIZADOS/paises_generalizados.shp")

# Convierte el dataframe geocoded_02 en un Spatial object
#coordinates(geocoded_02) <- 2:1 
coordinates(geocoded) <- 2:1 

# formato del objeto spatial
crs_geograficas ='+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' 
#geocoded_02@proj4string <-CRS(crs_geograficas) 
geocoded@proj4string <-CRS(crs_geograficas)
paises_generalizados@proj4string <- CRS(crs_geograficas)


geocoded$contador <- 1 
usuarios.pais <- aggregate(x=geocoded, 
                           by=paises_generalizados, 
                           FUN= length)

#___________________________________________________________


