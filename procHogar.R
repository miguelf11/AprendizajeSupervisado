#prueba

datos <- read.csv("data/hogares.csv")






#ragazzi/ragazze = hombre/mujer (B)
#ragazze = mujer(F)
#ragazzi = hombre(M)

sexo <- rep("NA",length(datos$Notas))
notas <- datos$Notas
notas <- as.character(notas)

for(i in 1:length(sexo)){
  if(grepl("ragazzi/ragazze",notas[i])){
    sexo[i] <- "B"
  }else{
    if(grepl("ragazzi",notas[i])){
      sexo[i] <- "M"
    }
    if(grepl("ragazze",notas[i])){
      sexo[i] <- "F"
    }
    if(is.na(sexo[i])){
      sexo[i] <- "B"
    }
  }
    
}
datos$sexo <- sexo

datos.female <- datos[datos$sexo == "F" | datos$sexo == "B", ]
datos.male <- datos[datos$sexo == "M" | datos$sexo == "B", ]



todo.incluido <- rep(0,length(datos$Notas))
notas <- datos$Precio.Mensual
notas <- as.character(notas)

for(i in 1:length(sexo)){
  if(grepl("TUTTO INCLUSO",notas[i])){
    todo.incluido[i] <- 1
  }
  
}
datos$todo.incluido <- todo.incluido

#datos[fila,columna]


#falta separar las habitaciones creando nuevas filas


a <- rep("0",length(notas))
for(i in 1:length(notas)){
  a[i] <- strsplit(notas[i],";")
}

notas[1]
a <- strsplit(notas[1],";")
a <- unlist(a)
