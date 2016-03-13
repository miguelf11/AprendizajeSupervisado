source("google_api.R")
install("readxl")
library(stringr)
library("readxl")
data <- read_excel("data/hogares.xlsx")

data <- as.data.frame(data)
data <- data[!is.na(data[1]),]

data$Foto <- NULL  
data$Piso <- NULL


#eliminar los saltos de linea
data$Dirección <- str_replace_all(data$Dirección, "[\r\n]" , " ")
data$Distrito <- str_replace_all(data$Distrito, "[\r\n]" , " ")
data$`Tipo de Inmueble` <- str_replace_all(data$`Tipo de Inmueble`, "[\r\n]" , " ")
data$`Habitaciones Disponibles` <- str_replace_all(data$`Habitaciones Disponibles`, "[\r\n]" , " ")
data$`Precio Mensual` <- str_replace_all(data$`Precio Mensual`, "[\r\n]" , " ")
data$Notas <- str_replace_all(data$Notas, "[\r\n]" , " ")

# Colocar su API Key 
api_key = "AIzaSyB6ybttc49Jy60JhSzXRrko5jtK6z7ExEw"

dir <- data$Dirección
dist <- data$Distrito
driving.time <- rep(NA,length(dist))
driving.metros <- rep(NA,length(dist))
for (i in 1:length(dir)){
  origen = paste(dir[i],",",dist[i],",","Italia",sep="")
  destino = c("Sapienza-Università di Roma")
  api_url = get_url(origen, destino, api_key, mode = "driving")
  datos <- get_data(api_url)
  p <- parse_data(datos)
  if (p[2] ==  ""){
    origen = paste(dir[i],",","Italia",sep="")
    api_url = get_url(origen, destino, api_key, mode = "driving")
    datos <- get_data(api_url)
    p <- parse_data(datos)
  }
  if (p[2] ==  ""){
    origen = paste(dir[i],sep="")
    api_url = get_url(origen, destino, api_key, mode = "driving")
    datos <- get_data(api_url)
    p <- parse_data(datos)
  }
  print(paste(origen,"-",p[2]))
  driving.time[i] <- p[6]
  driving.metros[i] <- p[4]
}
data$driving.time <- driving.time
data$driving.metros <- driving.metros

#34 NA


walking.time <- rep(NA,length(dist))
walking.metros <- rep(NA,length(dist))
for (i in 1:length(dir)){
  #origen = paste(dir[i],",",dist[i],",","Italia",sep=" ")
  origen = paste(dir[i],",",dist[i],",","Italia",sep="")
  #origen = paste(dir[i],",",dist[i],",","Italia",sep=" ")
  destino = c("Sapienza-Università di Roma")
  api_url = get_url(origen, destino, api_key, mode = "walking")
  datos <- get_data(api_url)
  p <- parse_data(datos)
  if (p[2] ==  ""){
    origen = paste(dir[i],",","Italia",sep="")
    api_url = get_url(origen, destino, api_key, mode = "walking")
    datos <- get_data(api_url)
    p <- parse_data(datos)
  }
  if (p[2] ==  ""){
    origen = paste(dir[i],sep="")
    api_url = get_url(origen, destino, api_key, mode = "walking")
    datos <- get_data(api_url)
    p <- parse_data(datos)
  }
  print(paste(origen,"-",p[2]))
  walking.time[i] <- p[6]
  walking.metros[i] <- p[4]
}
data$walking.time <- walking.time
data$walking.metros <- walking.metros




write.csv(data,"data/hogares.csv")
