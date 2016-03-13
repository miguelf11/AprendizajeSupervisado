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


#1 instance of mIngreso = 1 , is not good enough
dat$mIngreso <- as.character(dat$mIngreso)
dat <- dat[dat$mIngreso != 1, ]




dat0 <- dat[dat$mIngreso == 0, ]
dat2 <- dat[dat$mIngreso == 2, ]
dat3 <- dat[dat$mIngreso == 3, ]

sampl0 <-  sample(nrow(dat0), floor(nrow(dat0) * 0.75))
sampl2 <-  sample(nrow(dat2), floor(nrow(dat2) * 0.75))
sampl3 <-  sample(nrow(dat3), floor(nrow(dat3) * 0.75))

dat_train0 <- dat0[sampl0, ] 
dat_train2 <- dat2[sampl2, ]
dat_train3 <- dat3[sampl3, ]

dat_train <- rbind(dat_train0,dat_train2,dat_train3)

dat_test0 <- dat0[-sampl0, ]
dat_test2 <- dat2[-sampl2, ]
dat_test3 <- dat3[-sampl3, ]

dat_test <- rbind(dat_test0,dat_test2,dat_test3)



dat_train$mIngreso <- as.factor(dat_train$mIngreso)
dat_test$mIngreso <- as.factor(dat_test$mIngreso)



#KNN 
c <- 1:length(dat)
c <- c[-5] 
#dat.knn <- as.data.frame(lapply(dat_train[c], normalize))

knn_train <- as.data.frame(lapply(dat_train[c], normalize))
knn_test <- as.data.frame(lapply(dat_test[c], normalize))
#normalizar todas las columnas menos el feature tag

#IMPORTANTE MANTENER EL % DE CLASES DENTRO DEL TRAINING Y TEST DATA



knn_train_labels <- dat_train[ ,5]
knn_test_labels <- dat_test[ ,5]   



prc_test_pred <- knn(train = knn_train, test = knn_test,cl = knn_train_labels, k=8)
confusionMatrix.knn8<- table(knn_test_labels,prc_test_pred)
confusionMatrix.knn8

#confusionMatrix.knn[fila,columna]

accuracy.knn8 <- (confusionMatrix.knn[1,1] + confusionMatrix.knn[2,2] +
                   confusionMatrix.knn[3,3])/ sum(confusionMatrix.knn)

accuracy.knn8


prc_test_pred <- knn(train = knn_train, test = knn_test,cl = knn_train_labels, k=9)
confusionMatrix.knn9<- table(knn_test_labels,prc_test_pred)
confusionMatrix.knn9

#confusionMatrix.knn[fila,columna]

accuracy.knn9 <- (confusionMatrix.knn[1,1] + confusionMatrix.knn[2,2] +
                    confusionMatrix.knn[3,3])/ sum(confusionMatrix.knn)

accuracy.knn9






  
###############################################################
#Decision trees


training <- dat_train
testing <- dat_test

tree <- rpart(mIngreso ~ ., data = training, method = "class", control = rpart.control(minsplit = 10, cp = 0.001))
rpart.plot(tree)



#confusion matrix
confusionMatrix.tree <- table(testing$mIngreso, predict(tree, newdata = testing,type = "class"))
confusionMatrix.tree

accuracy.tree <- (confusionMatrix.tree[1,1] + confusionMatrix.tree[2,2]
                  + confusionMatrix.tree[3,3]) / sum(confusionMatrix.tree)
 
accuracy.tree




########################################################################
#Clasification rules


rules = JRip(formula = mIngreso ~ ., data = training)

#confusion matrix
confusionMatrix.clasif = table(testing$mIngreso, predict(rules, newdata = testing,type = "class"))
confusionMatrix.clasif


accuracy.rules <- (confusionMatrix.clasif[1,1] + confusionMatrix.clasif[2,2] +
                    confusionMatrix.clasif[3,3])/sum(confusionMatrix.clasif)

accuracy.rules

#debido a que todos los valores posibles son igual de importantes
#no es de importancia calcular la especifidad











