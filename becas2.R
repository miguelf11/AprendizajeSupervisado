#install.packages('rpart.plot')
#install.packages('RWeka')


# funcion para instalar paquetes (no funciona siempre)
install = function(pkg){
  # Si ya está instalado, no lo instala.
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, repos = "http:/cran.rstudio.com")
    if (!require(pkg, character.only = TRUE)) stop(paste("load failure:", pkg))
  }
}

install("class")
install("dplyr")
install("rpart")
install("rpart.plot")
install("RWeka")





#normalizar datos entre 0 y 1
normalize <- function(x) {
  return (x - min(x)) / (max(x) - min(x)) 
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



dat$grOdontologicos[dat$grOdontologicos == "o"] <- as.character(0)
dat$grOdontologicos <- as.integer(dat$grOdontologicos)




actual <- as.Date("01/03/2016",format("%d/%m/%Y"))
a <- rep(as.Date("14/03/1979",format="%d/%m/%Y"),length(dat$fNacimiento))
edad <- rep(NA,length(dat$fNacimiento))
for(i in 1:length(dat$fNacimiento)){
  a[i] <- as.Date(dat$fNacimiento[i],format="%d/%m/%Y")
  edad[i] <- as.integer(actual-a[i]) %/% 365
}
dat$fNacimiento <- NULL
dat$edad <- edad

dat <- dat[dat$edad > 5, ]



#1 instance of mIngreso = 1 , is not good enough
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

knn_train <- as.data.frame(lapply(dat_train[c], normalize))
knn_test <- as.data.frame(lapply(dat_test[c], normalize))
#normalizar todas las columnas menos el feature tag

#IMPORTANTE MANTENER EL % DE CLASES DENTRO DEL TRAINING Y TEST DATA



knn_train_labels <- dat_train[ ,5]
knn_test_labels <- dat_test[ ,5]   



prc_test_pred <- knn(train = knn_train, test = knn_test,cl = knn_train_labels, k=8)


confusionMatrix.knn8<- table(knn_test_labels,prc_test_pred)


accuracy.knn8 <- (confusionMatrix.knn8[1,1] + confusionMatrix.knn8[2,2] +
                   confusionMatrix.knn8[3,3])/ sum(confusionMatrix.knn8)




prc_test_pred <- knn(train = knn_train, test = knn_test,cl = knn_train_labels, k=9)
confusionMatrix.knn9<- table(knn_test_labels,prc_test_pred)



accuracy.knn9 <- (confusionMatrix.knn9[1,1] + confusionMatrix.knn9[2,2] +
                    confusionMatrix.knn9[3,3])/ sum(confusionMatrix.knn9)


prc_test_pred <- knn(train = knn_train, test = knn_test,cl = knn_train_labels, k=10)
confusionMatrix.knn10<- table(knn_test_labels,prc_test_pred)



accuracy.knn10 <- (confusionMatrix.knn10[1,1] + confusionMatrix.knn10[2,2] +
                    confusionMatrix.knn10[3,3])/ sum(confusionMatrix.knn10)





  
###############################################################
#Decision trees


training <- dat_train
testing <- dat_test

tree <- rpart(mIngreso ~ ., data = training, method = "class", control = rpart.control(minsplit = 10, cp = 0.001))
#rpart.plot(tree)


#confusion matrix
confusionMatrix.tree <- table(testing$mIngreso, predict(tree, newdata = testing,type = "class"))


accuracy.tree <- (confusionMatrix.tree[1,1] + confusionMatrix.tree[2,2]
                  + confusionMatrix.tree[3,3]) / sum(confusionMatrix.tree)


tree2 <- rpart(mIngreso ~ ., data = training, method = "class", control = rpart.control(minsplit = 15, cp = 0.001))
#rpart.plot(tree2)

confusionMatrix.tree2 <- table(testing$mIngreso, predict(tree2, newdata = testing,type = "class"))


accuracy.tree2 <- (confusionMatrix.tree2[1,1] + confusionMatrix.tree2[2,2]
                  + confusionMatrix.tree2[3,3]) / sum(confusionMatrix.tree2)



tree3 <- rpart(mIngreso ~ ., data = training, method = "class", control = rpart.control(minsplit = 10, cp = 0.01))
#rpart.plot(tree3)

confusionMatrix.tree3 <- table(testing$mIngreso, predict(tree3, newdata = testing,type = "class"))

accuracy.tree3 <- (confusionMatrix.tree3[1,1] + confusionMatrix.tree3[2,2]
                   + confusionMatrix.tree3[3,3]) / sum(confusionMatrix.tree3)
 





########################################################################
#Clasification rules


rules = JRip(formula = mIngreso ~ ., data = training)

#confusion matrix
confusionMatrix.clasif = table(testing$mIngreso, predict(rules, newdata = testing,type = "class"))



accuracy.rules <- (confusionMatrix.clasif[1,1] + confusionMatrix.clasif[2,2] +
                    confusionMatrix.clasif[3,3])/sum(confusionMatrix.clasif)




rules2 = OneR(formula = mIngreso ~ ., data = training)

#confusion matrix
confusionMatrix.clasif2 = table(testing$mIngreso, predict(rules2, newdata = testing,type = "class"))



accuracy.rules2 <- (confusionMatrix.clasif2[1,1] + confusionMatrix.clasif2[2,2] +
                     confusionMatrix.clasif2[3,3])/sum(confusionMatrix.clasif2)


rules2 = OneR(formula = mIngreso ~ ., data = training)

#confusion matrix
confusionMatrix.clasif2 = table(testing$mIngreso, predict(rules2, newdata = testing,type = "class"))



accuracy.rules2 <- (confusionMatrix.clasif2[1,1] + confusionMatrix.clasif2[2,2] +
                      confusionMatrix.clasif2[3,3])/sum(confusionMatrix.clasif2)




rules3 = PART(formula = mIngreso ~ ., data = training)

#confusion matrix
confusionMatrix.clasif3 = table(testing$mIngreso, predict(rules3, newdata = testing,type = "class"))



accuracy.rules3 <- (confusionMatrix.clasif3[1,1] + confusionMatrix.clasif3[2,2] +
                      confusionMatrix.clasif3[3,3])/sum(confusionMatrix.clasif3)





#knn
confusionMatrix.knn8
accuracy.knn8
confusionMatrix.knn9
accuracy.knn9
confusionMatrix.knn10
accuracy.knn10


#tree
confusionMatrix.tree
accuracy.tree
confusionMatrix.tree2
accuracy.tree2
confusionMatrix.tree3
accuracy.tree3

#clasif rules
confusionMatrix.clasif
accuracy.rules
confusionMatrix.clasif2
accuracy.rules2
confusionMatrix.clasif3
accuracy.rules3




comparar <- rep(0,9)
comparar[1] <- accuracy.knn8
comparar[2] <- accuracy.knn9
comparar[3] <- accuracy.knn10
comparar[4] <- accuracy.tree
comparar[5] <- accuracy.tree2
comparar[6] <- accuracy.tree3
comparar[7] <- accuracy.rules
comparar[8] <- accuracy.rules2
comparar[9] <- accuracy.rules3
comparar <- comparar == max(comparar)
comparar
