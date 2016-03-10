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

datos.female <- datos[datos$sexo == 1 | datos$sexo == 2, ]
datos.male <- datos[datos$sexo == 0 | datos$sexo == 2, ]



condominio <- rep(0,length(datos$Precio.Mensual))
aseo <- rep(0,length(datos$Precio.Mensual))
agua <- rep(0,length(datos$Precio.Mensual))
internet <- rep(0,length(datos$Precio.Mensual))
calefaccion <- rep(0,length(datos$Precio.Mensual))
internet <- rep(0,length(datos$Precio.Mensual))
precio.mensual <- datos$Precio.Mensual
precio.mensual <- as.character(precio.mensual)

for(i in 1:length(sexo)){
  if(grepl("TUTTO INCLUSO",precio.mensual[i])){
    condominio[i] <- 1
    aseo[i] <- 1
    agua[i] <- 1
    internet[i] <- 1
    calefaccion[i] <- 1
    internet[i] <- 1
  }
  if(grepl("acqua",precio.mensual[i])){
    agua[i] <- 1
  }
  if(grepl("rifiuti",precio.mensual[i])){
    aseo[i] <- 1
  }
  if(grepl("internet inclu",precio.mensual[i])){
    internet[i] <- 1
  }
  if(grepl("con",precio.mensual[i])){
    condominio[i] <- 1
  }
  if(grepl("risca",precio.mensual[i])){
    calefaccion[i] <- 1
  }
  if(grepl("50/meses condominio e riscaldamento",precio.mensual[i])){
    condominio[i] <- 0
    calefaccion[i] <- 0
  }
  if(grepl("spese incluse tranne internet",precio.mensual[i])){
    condominio[i] <- 1
    aseo[i] <- 1
    agua[i] <- 1
    internet[i] <- 1
    calefaccion[i] <- 1
    internet[i] <- 0
  }
  if(grepl("ad eccezione di internet",precio.mensual[i])){
    internet[i] <- 0
  }
  

}
datos$condominio <- condominio
datos$aseo <- aseo
datos$agua <- agua
datos$internet <- internet
datos$calefaccion <- calefaccion
datos$internet <- internet



View(datos)
# precio.mensual <- as.character(datos$Precio.Mensual)

# #falta separar las habitaciones creando nuevas filas
# 
# # precio.mensual <- as.character(precio.mensual)
# # b <- rep("NA",10000)
# # a <- rep("0",length(precio.mensual))
# # k <- 0
# # for(i in 1:length(precio.mensual)){
# #   a[i] <- strsplit(precio.mensual[i],";|,")
# #   a <- unlist(a[i])
# #   for (j in 1:length(a)){
# #     b <- strsplit(a,",")
# #     k <- k+1
# #   }
# # }
# 
# 
# a <- strsplit(precio.mensual[4],";|,")
# a <- unlist(a)
# b <- strsplit(a[1],",")
# b <- unlist(b)








# c <- c(7,14,15,16,17,18)
# prueba <- datos[,c]
# View(prueba)

