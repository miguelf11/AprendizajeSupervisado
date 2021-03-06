#fitted() hace lo mismo que predict()

normalize <- function(x) {
  return (as.numeric(x - min(x)) / as.numeric(max(x) - min(x))) 
}

library('dplyr')
datos <- read.csv("data/hogares.csv")






#ragazzi/ragazze = hombre/mujer (B)(2)
#ragazze = mujer(F)(1)
#ragazzi = hombre(M)(0)

sexo <- rep(NA,length(datos$Notas))
notas <- datos$Notas
notas <- as.character(notas)

for(i in 1:length(sexo)){
  if(grepl("ragazzi/ragazze",notas[i],ignore.case = TRUE)){
    sexo[i] <- 2
  }else{
    if(grepl("ragazzi",notas[i],ignore.case = TRUE)){
      sexo[i] <- 0
    }
    if(grepl("ragazze",notas[i],ignore.case = TRUE)){
      sexo[i] <- 1
    }
    if(is.na(sexo[i])){
      sexo[i] <- 2
    }
  }
    
}
datos$sexo <- sexo






condominio <- rep(0,length(datos$Precio.Mensual))
aseo <- rep(0,length(datos$Precio.Mensual))
agua <- rep(0,length(datos$Precio.Mensual))
internet <- rep(0,length(datos$Precio.Mensual))
calefaccion <- rep(0,length(datos$Precio.Mensual))
internet <- rep(0,length(datos$Precio.Mensual))
precio.mensual <- datos$Precio.Mensual
precio.mensual <- as.character(precio.mensual)

for(i in 1:length(sexo)){
  if(grepl("TUTTO INCLUSO",precio.mensual[i],ignore.case = TRUE)){
    condominio[i] <- 1
    aseo[i] <- 1
    agua[i] <- 1
    internet[i] <- 1
    calefaccion[i] <- 1
    internet[i] <- 1
  }
  if(grepl("acqua",precio.mensual[i],ignore.case = TRUE)){
    agua[i] <- 1
  }
  if(grepl("rifiuti",precio.mensual[i],ignore.case = TRUE)){
    aseo[i] <- 1
  }
  if(grepl("internet inclu",precio.mensual[i],ignore.case = TRUE)){
    internet[i] <- 1
  }
  if(grepl("con",precio.mensual[i],ignore.case = TRUE)){
    condominio[i] <- 1
  }
  if(grepl("risca",precio.mensual[i],ignore.case = TRUE)){
    calefaccion[i] <- 1
  }
  if(grepl("50/meses condominio e riscaldamento",precio.mensual[i],ignore.case = TRUE)){
    condominio[i] <- 0
    calefaccion[i] <- 0
  }
  if(grepl("spese incluse tranne internet",precio.mensual[i],ignore.case = TRUE)){
    condominio[i] <- 1
    aseo[i] <- 1
    agua[i] <- 1
    internet[i] <- 1
    calefaccion[i] <- 1
    internet[i] <- 0
  }
  if(grepl("ad eccezione di internet",precio.mensual[i],ignore.case = TRUE)){
    internet[i] <- 0
  }
  
  
}


#assign vectors in DF to create new features 
datos$condominio <- condominio
datos$aseo <- aseo
datos$agua <- agua
datos$internet <- internet
datos$calefaccion <- calefaccion
datos$internet <- internet






#separate bedrooms and add rows(or instances) in case is needed 

datos$precios <- rep(0,length(datos$driving.time))
datos$Habitaciones.Disponibles <- as.character(datos$Habitaciones.Disponibles)
habitaciones <- datos$Habitaciones.Disponibles
datos$Precio.Mensual <- as.character(datos$Precio.Mensual)
datos2 <- datos
datos.aux <- datos[is.na(datos$Distrito),]
for(i in 1:length(datos2$Descripci�n)){
  habitaciones[i] <- gsub("\\d", "", habitaciones[i])
  if(grepl("(singol)",habitaciones[i],ignore.case = TRUE) &
   (!(grepl("(doppi)",habitaciones[i],ignore.case = TRUE)))&
   (!(grepl("(posto)",habitaciones[i],ignore.case = TRUE)))){
    
    #print(paste(i,"-",habitaciones[i],collapse = " "))
    aux <-unlist(strsplit(datos$Precio.Mensual[i],";"))
    b <- rep(0,2)
    for(j in 1:length(b)){
      b[j] <- as.numeric(gsub("\\D", "", aux[j]))
    }
    b <- b[!is.na(b)]
    datos$Habitaciones.Disponibles[i] <- "2"
    if (b[1] > 10000){
      x <- b[1]
      x <- x/1000
      x <- as.character(x)
      x <- substr(x, 1,3)
      x <- as.numeric(x)
      b[1] <- x
    }
    datos$precios[i] <- b[1]
  }
  if(grepl("(inter)+",habitaciones[i],ignore.case = TRUE)){
    aux <- unlist(strsplit(datos$Precio.Mensual[i],";"))
    b <- rep(0,2)
    for(j in 1:length(b)){
      b[j] <- as.numeric(gsub("\\D", "", aux[j]))
    }
    b <- b[!is.na(b)]
    datos$Habitaciones.Disponibles[i] <- "1"
    datos$precios[i] <- b[1]
  }
  if(grepl("(mini)+",habitaciones[i],ignore.case = TRUE)){
    aux <-unlist(strsplit(datos$Precio.Mensual[i],";"))
    b <- rep(0,2)
    for(j in 1:length(b)){
      b[j] <- as.numeric(gsub("\\D", "", aux[j]))
    }
    b <- b[!is.na(b)]
    datos$Habitaciones.Disponibles[i] <- "1"
    datos$precios[i] <- b[1]
  }
  if(grepl("(mono)+",habitaciones[i],ignore.case = TRUE)){
    aux <-unlist(strsplit(datos$Precio.Mensual[i],";"))
    b <- rep(0,2)
    for(j in 1:length(b)){
      b[j] <- as.numeric(gsub("\\D", "", aux[j]))
    }
    b <- b[!is.na(b)]
    datos$Habitaciones.Disponibles[i] <- "1"
    datos$precios[i] <- b[1]
  }
  if(grepl("dopp+",habitaciones[i],ignore.case = TRUE) &
    (!(grepl("singo+",habitaciones[i],ignore.case = TRUE)))){
    aux <-unlist(strsplit(datos$Precio.Mensual[i],";"))
    b <- rep(0,2)
    for(j in 1:length(b)){
      b[j] <- as.numeric(gsub("\\D", "", aux[j]))
    }
    b <- b[!is.na(b)]
    datos$Habitaciones.Disponibles[i] <- "3"
    datos$precios[i] <- b[1]
  }
  if(grepl("posto+",habitaciones[i],ignore.case = TRUE) &
     (!(grepl("singo+",habitaciones[i],ignore.case = TRUE)))){
    aux <-unlist(strsplit(datos$Precio.Mensual[i],";"))
    b <- rep(0,2)
    for(j in 1:length(b)){
      b[j] <- as.numeric(gsub("\\D", "", aux[j]))
    }
    b <- b[!is.na(b)]
    datos$Habitaciones.Disponibles[i] <- "3"
    datos$precios[i] <- b[1]
  }
  if (datos$precios[i] == 0 ){
    #todos los dem�s que son singola - posto letto
    aux <-unlist(strsplit(datos$Precio.Mensual[i],";|,"))
    b <- rep(0,3)
    for(j in 1:length(b)){
      b[j] <- as.numeric(gsub("\\D", "", aux[j]))
    }
    b <- b[!is.na(b)]
    
    hab.aux <- datos[i,]
    hab.aux$Habitaciones.Disponibles[1] <- "3"
    if (!is.na(b[3]) ){
      hab.aux$precios[1] <- b[3]
    }else{
      hab.aux$precios[1] <- b[2]
    }
    datos$Habitaciones.Disponibles[i] <- "2"
    datos$precios[i] <- b[1]
    datos.aux <- rbind(datos.aux,hab.aux)
  }

  
}


#join the original data with the added rows
datos <- rbind(datos,datos.aux)

datos$Habitaciones.Disponibles <- as.numeric(datos$Habitaciones.Disponibles)

#END OF SEPARATE BEDROOMS



#delete unnecesary feature
datos$Precio.Mensual <- NULL



#class(datos$Tipo.de.Inmueble)


#discreta datos fast 
#combi$Item_Fat_Content <- ifelse(combi$Item_Fat_Content == "Regular",1,0)


#transform tipo.inmueble in numbers
#levels(datos$Tipo.de.Inmueble) #para conocer los tipos de datos


datos$Tipo.de.Inmueble <- as.character(datos$Tipo.de.Inmueble)
 
datos$Tipo.de.Inmueble[datos$Tipo.de.Inmueble == "Apartamento"] = as.character(3)
datos$Tipo.de.Inmueble[datos$Tipo.de.Inmueble == "Apparrtamento"] = as.character(3)
datos$Tipo.de.Inmueble[datos$Tipo.de.Inmueble == "Appartameno"] = as.character(3)
datos$Tipo.de.Inmueble[datos$Tipo.de.Inmueble == "Appartamenti"] = as.character(3)
datos$Tipo.de.Inmueble[datos$Tipo.de.Inmueble == "Appartamento"] = as.character(3)
datos$Tipo.de.Inmueble[datos$Tipo.de.Inmueble == "Mini appartamento"] = as.character(2)
datos$Tipo.de.Inmueble[datos$Tipo.de.Inmueble == "Mini Appartamento"] = as.character(2)
datos$Tipo.de.Inmueble[datos$Tipo.de.Inmueble == "Monolocale"] = as.character(1)
# 
# 
datos$Tipo.de.Inmueble <- as.numeric(datos$Tipo.de.Inmueble)
#from seconds to minutes(entire division)
#more easy to read
datos$walking.time <- datos$walking.time %/% 60
datos$driving.time <- datos$driving.time %/% 60

#select features to do the lm

datos$Distrito <- NULL
datos$Notas <- NULL
datos$X <- NULL
datos$Direcci�n <- NULL
datos$Descripci�n <- NULL

#i decide to use just driving.time to predict the price
datos$driving.metros <- NULL
datos$walking.time <- NULL
datos$walking.metros <- NULL
datos <- as.data.frame(lapply(datos[1:ncol(datos)], as.numeric))


#last step of preprocessing is separate data between male and female

datos.female <- datos[datos$sexo == 1 | datos$sexo == 2, ]
datos.male <- datos[datos$sexo == 0 | datos$sexo == 2, ]



datos.female$sexo <- NULL
datos.male$sexo <- NULL


c <- 1:ncol(datos.male)
c <- c[-9]

precios.female <- datos.female$precios
precios.male <- datos.male$precios

datos.male <- as.data.frame(lapply(datos.male[c], normalize))
datos.female <- as.data.frame(lapply(datos.female[c], normalize))


datos.female$precios <- precios.female
datos.male$precios <- precios.male




#split training & testing for males
sampl <-  sample(nrow(datos.male), floor(nrow(datos.male) * 0.75))
training.male <- datos.male[sampl, ]
testing.male <- datos.male[-sampl, ]

#generate model
model.male <- lm(training.male$precios ~ ., training.male)

#aplying model to testing
testing.male$regresion <- predict(model.male, newdata = testing.male)

#see quality of the model
testing.male$precision <- abs(testing.male$precios - abs(testing.male$regresion))
accuracy.male <- sum(testing.male$precision)
accuracy.male

View(testing.male[ ,c(9,10,11)])





#split training & testing for females

sampl <-  sample(nrow(datos.female), floor(nrow(datos.female) * 0.75))
training.female <- datos.female[sampl, ]
testing.female <- datos.female[-sampl, ]


#generate model
model.female <- lm(training.female$precios ~ ., training.female)
#aplying model to testing
testing.female$regresion <- predict(model.female, newdata = testing.female)


testing.female$precision <- abs(testing.female$precios - abs(testing.female$regresion))
accuracy.female <- sum(testing.female$precision)
accuracy.female



View(testing.female[ ,c(9,10,11)])



#eleccion del hombre
male <- testing.male[testing.male$precision == max(testing.male$precision), ]

#eleccion de la mujer
female <- testing.female[testing.female$precision == max(testing.female$precision), ]






