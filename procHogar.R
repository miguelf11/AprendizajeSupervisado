#fitted() hace lo mismo que predict()
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
for(i in 1:length(datos2$Descripción)){
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
    datos$Habitaciones.Disponibles[i] <- "singola"
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
    datos$Habitaciones.Disponibles[i] <- "entero"
    datos$precios[i] <- b[1]
  }
  if(grepl("(mini)+",habitaciones[i],ignore.case = TRUE)){
    aux <-unlist(strsplit(datos$Precio.Mensual[i],";"))
    b <- rep(0,2)
    for(j in 1:length(b)){
      b[j] <- as.numeric(gsub("\\D", "", aux[j]))
    }
    b <- b[!is.na(b)]
    datos$Habitaciones.Disponibles[i] <- "entero"
    datos$precios[i] <- b[1]
  }
  if(grepl("(mono)+",habitaciones[i],ignore.case = TRUE)){
    aux <-unlist(strsplit(datos$Precio.Mensual[i],";"))
    b <- rep(0,2)
    for(j in 1:length(b)){
      b[j] <- as.numeric(gsub("\\D", "", aux[j]))
    }
    b <- b[!is.na(b)]
    datos$Habitaciones.Disponibles[i] <- "entero"
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
    datos$Habitaciones.Disponibles[i] <- "posto letto"
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
    datos$Habitaciones.Disponibles[i] <- "posto letto"
    datos$precios[i] <- b[1]
  }
  if (datos$precios[i] == 0 ){
    #todos los demás que son singola - posto letto
    print(paste(i,"-",habitaciones[i],collapse = " "))
    aux <-unlist(strsplit(datos$Precio.Mensual[i],";|,"))
    b <- rep(0,3)
    for(j in 1:length(b)){
      b[j] <- as.numeric(gsub("\\D", "", aux[j]))
    }
    b <- b[!is.na(b)]
    
    hab.aux <- datos[i,]
    hab.aux$Habitaciones.Disponibles[1] <- "posto letto"
    if (!is.na(b[3]) ){
      hab.aux$precios[1] <- b[3]
    }else{
      hab.aux$precios[1] <- b[2]
    }
    datos$Habitaciones.Disponibles[i] <- "singola"
    datos$precios[i] <- b[1]
    datos.aux <- rbind(datos.aux,hab.aux)
  }

  
}

#join the original data with the added rows
datos <- rbind(datos,datos.aux)

#END OF SEPARATE BEDROOMS



#delete unnecesary feature
datos$Precio.Mensual <- NULL


#last step of preprocessing is separate data between male and female

#datos.female <- datos[datos$sexo == 1 | datos$sexo == 2, ]
#datos.male <- datos[datos$sexo == 0 | datos$sexo == 2, ]

