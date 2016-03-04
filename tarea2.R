#install.packages("readxl")
source("google_api.R")
library(stringr)
library("readxl")
data <- read_excel("hogares.xlsx")

data <- as.data.frame(data)
data <- data[!is.na(data[1]),]

data$Foto <- NULL  #
data$Piso <- NULL


#eliminar los saltos de linea
data$Dirección <- str_replace_all(data$Dirección, "[\r\n]" , " ")


dir <- unique(data$Dirección)
for (i in dir){
  origen = c(data$Dirección[1])
  destino = c("Sapienza-Università di Roma")
}



origen = c(data$Dirección[1])
destino = c("Sapienza-Università di Roma")

# Colocar su API Key 
api_key = "AIzaSyB6ybttc49Jy60JhSzXRrko5jtK6z7ExEw"

api_url = get_url(origen, destino, api_key, mode = "driving")

datos <- get_data(api_url)
#datos
