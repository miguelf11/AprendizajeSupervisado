# Seleccionar google_api.R en su sistema de archivos
source(file.choose())

origen = c("Via Paolo Emilio", "Vancouver BC", "Seattle")
destino =c("Piazzale Aldo Moro", "San Francisco", "Victoria BC")

origen = c("Via Paolo Emilio")
destino = c("Sapienza-Università di Roma")


# Colocar su API Key 
api_key = "AIzaSyB6ybttc49Jy60JhSzXRrko5jtK6z7ExEw"

api_url = get_url(origen, destino, api_key, mode = "driving")

datos <- get_data(api_url)
datos


#driving
#walking 
#bicycling
#transit 