#Jonathan Holman
#Practical Machine Learning Course Project
#19 November, 2016

library(caret)

#Load the data
setwd("C:/Users/jonho/Documents/ADS Training/Coursera/Practical Machine Learning")
starttraining <- read.csv("pml-training.csv")
endtesting <- read.csv("pml-testing.csv")

#Assign the sizes of the training, testing, and validation sets
ptrain <- 0.6
ptest <- 0.2
pvalid <- 0.2

#Filter to only important variables
#Filter to each variable with a name starting in "roll", "pitch", "yaw", "total", "gryos", "accel", "magnet", and the variable "classe"
fdf <- cbind(starttraining[grep("^roll",names(starttraining))], starttraining[grep("^pitch",names(starttraining))], 
                  starttraining[grep("^yaw",names(starttraining))], starttraining[grep("^total",names(starttraining))], 
                  starttraining[grep("^gryos",names(starttraining))], starttraining[grep("^accel",names(starttraining))],
                  starttraining[grep("^magnet",names(starttraining))], starttraining[grep("classe",names(starttraining))])

#Convert all fdf columns (except classe) to numeric
fdf[,-41] <- lapply(fdf[,-41],as.numeric)

#Pull out the training data
set.seed(1920)
inTrain <- createDataPartition(fdf$classe, p=ptrain, list = FALSE)
training <- fdf[inTrain,]

#Split the remaining data into testing and validation
tv <- fdf[-inTrain,]
inTest <- createDataPartition(tv$classe, p = ptest/(1-ptrain),list=FALSE)
testing <- tv[inTest,]
validation <- tv[-inTest,]

set.seed(9000)

#Run several different models
fitrf <- train(classe~.,data=training,method="rf")
fitgbm <- train(classe~.,data=training,method="gbm")
fitrpart <- train(classe~.,data=training,method="rpart")
fitlda <- train(classe~.,data=training,method="lda")

#Predict the results within the training set itself
predrf <- predict(fitrf,training)
predgbm <- predict(fitgbm,training)
predrpart <- predict(fitrpart,training)
predlda <- predict(fitlda,training)

#Look at performance of the models within themselves
confusionMatrix(predrf,training$classe)
confusionMatrix(predgbm,training$classe)
confusionMatrix(predrpart,training$classe)
confusionMatrix(predlda,training$classe)

#Predict the results within the validation set
predrf <- predict(fitrf,validation)
predgbm <- predict(fitgbm,validation)
predrpart <- predict(fitrpart,validation)
predlda <- predict(fitlda,validation)

#Look at performance of the models within the validation set
confusionMatrix(predrf,validation$classe)$table
confusionMatrix(predgbm,validation$classe)
confusionMatrix(predrpart,validation$classe)
confusionMatrix(predlda,validation$classe)

#Try a combination of all four models
predall <- data.frame(predrf,predgbm,classe=validation$classe)
fitfour <- train(classe~.,data=predall)
predfour <- predict(fitfour,predall)
confusionMatrix(predfour,predall$classe)

fittwo <- train(classe~predrf+predgbm,data=predall)
predtwo <- predict(fittwo,predall)
confusionMatrix(predtwo,predall$classe)

#Note that this reduces to the random forest model as it had 100% accuracy in the test set.  Now we use the testing set to estimate errors.
predfinal <- predict(fittwo,testing)
confusionMatrix(predfinal,testing$classe)
