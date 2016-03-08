#provee funcion para instalar paquetes
source("google_api.R")
install("class")
install("dplyr")

#normalizar datos entre 0 y 1
normalize <- function(x) {
  return (as.numeric(x - min(x)) / as.numeric(max(x) - min(x))) 
}

dif.fecha <- function(y){
  return(as.Date.date(y,format="%d/%m/%Y"))
}

dat <- read.csv("data/minable.csv", stringsAsFactors = FALSE)



#delete unnecessary features

dat$jReprobadas <- NULL
dat$dHabitacion <- NULL
dat$sugerencias <- NULL
dat$aEconomica <- NULL
dat$cDireccion <- NULL
dat$oSolicitudes <- NULL
dat$pReside <- NULL
dat$cIdentidad <- NULL


a <- rep(NA,length(dat$fNacimiento))
for(i in 1:length(dat$fNacimiento)){
  a[i] <- as.integer(dat$grOdontologicos[i])
  if(is.na(a[i])){
    a[i] <- 0
  }
}
dat$grOdontologicos <- a

a <- rep(as.Date("14/03/1979",format="%d/%m/%Y"),length(dat$fNacimiento))
for(i in 1:length(dat$fNacimiento)){
  a[i] <- as.Date(dat$fNacimiento[i],format="%d/%m/%Y")
}
dat$fNacimiento <- a



#KNN
c <- 1:length(dat)
c <- c[-6] 
dat.knn <- as.data.frame(lapply(dat[c], normalize))
#normalizar todas las columnas menos el feature tag


knn_train <- dat.knn[1:133,]
knn_test <- dat.knn[133:190,]

knn_train_labels <- dat[1:133, 6]
knn_test_labels <- dat[133:190, 6]   
#This code takes the diagnosis factor in column 1 
#of the prc data frame and on turn creates prc_train_labels and prc_test_labels data frame


prc_test_pred <- knn(train = knn_train, test = knn_test,cl = knn_train_labels, k=10)


