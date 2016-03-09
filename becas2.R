#provee funcion para instalar paquetes
#install.packages('party')
#install.packages('caret')
#install.packages('rpart.plot')
#install.packages('RWeka')
#install.packages('FSelector')

source("google_api.R")
install("class")
install("dplyr")
install("rpart")
install("party")
install("caret")
install("rpart.plot")
install("RWeka")
install("FSelector")


#IMPORTANTE MANTENER EL % DE CLASES DENTRO DEL TRAINING Y TEST DATA


#normalizar datos entre 0 y 1
normalize <- function(x) {
  return (as.numeric(x - min(x)) / as.numeric(max(x) - min(x))) 
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
dat$mIngreso <- as.factor(dat$mIngreso)


a <- rep(NA,length(dat$fNacimiento))
for(i in 1:length(dat$fNacimiento)){
  a[i] <- as.integer(dat$grOdontologicos[i])
  if(is.na(a[i])){
    a[i] <- 0
  }
}
dat$grOdontologicos <- a


actual <- as.Date("01/03/2016",format("%d/%m/%Y"))
probando <- as.Date("14/03/1979",format="%d/%m/%Y")
a <- rep(as.Date("14/03/1979",format="%d/%m/%Y"),length(dat$fNacimiento))
edad <- rep(NA,length(dat$fNacimiento))
for(i in 1:length(dat$fNacimiento)){
  a[i] <- as.Date(dat$fNacimiento[i],format="%d/%m/%Y")
  edad[i] <- as.integer(actual-a[i]) %/% 365
}
dat$fNacimiento <- NULL
dat$edad <- edad






#KNN 
c <- 1:length(dat)
c <- c[-5] 
dat.knn <- as.data.frame(lapply(dat[c], normalize))
#normalizar todas las columnas menos el feature tag

knn_train <- dat.knn[1:133,]
knn_test <- dat.knn[133:190,]

knn_train_labels <- dat[1:133, 5]
knn_test_labels <- dat[133:190, 5]   



prc_test_pred <- knn(train = knn_train, test = knn_test,cl = knn_train_labels, k=8)
confusionMatrix.knn<- table(knn_test_labels,prc_test_pred)
confusionMatrix.knn

#confusionMatrix.knn[fila,columna]

accuracy.knn <- (confusionMatrix.knn[1,1] + confusionMatrix.knn[2,2] + confusionMatrix.knn[3,3]+
  confusionMatrix.knn[4,4]) / sum(confusionMatrix.knn)



  
###############################################################
#Decision trees
training <- sample_n(dat,133)
testing <- sample_n(dat,67)
tree <- rpart(mIngreso ~ ., data = training, method = "class", control = rpart.control(minsplit = 10, cp = 0.001))
rpart.plot(tree)



#confusion matrix
confusionMatrix.tree <- table(testing$mIngreso, predict(tree, newdata = testing,type = "class"))


accuracy.tree <- (confusionMatrix.tree[1,1] + confusionMatrix.tree[2,2] + confusionMatrix.tree[3,3]+
                   confusionMatrix.tree[4,4]) / sum(confusionMatrix.tree)
 
accuracy.tree




########################################################################
#Clasification rules


rules = JRip(formula = mIngreso ~ ., data = training)

#confusion matrix
confusionMatrix.clasif = table(testing$mIngreso, predict(rules, newdata = testing,type = "class"))
confusionMatrixClasification


accuracy.tree <- (confusionMatrix.clasif[1,1] + confusionMatrix.clasif[2,2] +
                    confusionMatrix.clasif[3,3]+confusionMatrix.clasif[4,4])/
                    sum(confusionMatrix.clasif)

accuracy.tree

#debido a que todos los valores posibles son igual de importantes
#no es de importancia calcular la especifidad











