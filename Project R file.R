

## Course Project Machine Learning

## Cite the sources here

## Explain the project here

library(caret)
library(ggplot2)
library(rpart)
library(randomForest)
library(gbm)
library(lattice)
library(kernlab)
library(corrplot)
library(rattle)



##read in data

train<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", header = TRUE)

validation<-read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", header = TRUE)

#remove NAs

tData<- train[,colSums(is.na(train))==0]
vData<- validation[,colSums(is.na(validation))==0]


#remove uneccessary columns
tData<-tData[,-c(1:7)]
vData<-vData[,-c(1:7)]

set.seed(1234)


##partition data
tpart<-createDataPartition(y=tData$classe, p=0.7, list = FALSE)
trainData<-tData[tpart,]
testData<- tData[-tpart,]

#remove near zeros
nZVtD<-nearZeroVar(trainData)
trainData<-trainData[,-nZVtD]
testData<-testData

testData$classe<-as.factor(testData$classe)


##Create controls
trControlRF <- trainControl(method="cv", number=3, verboseIter=FALSE)

trControlGBM<-trainControl(method = "repeatedcv", number = 5, repeats = 1)
    
##Model Gradient Bosted Trees

modGBM <- train(classe~., data=trainData, method="gbm", trControl=trControlGBM, verbose = FALSE)

predGBM <- predict(modGBM, testData)
cnfmGBM <- confusionMatrix(predGBM, factor(testData$classe))
cnfmGBM

##Model Decision  Trees

modTr <- train(classe~., data=trainData, method="rpart")
fancyRpartPlot(modTr$finalModel)

predTr <- predict(modTr, testData)
cnfmTr <- confusionMatrix(predTr, factor(testData$classe))
cnfmTr


##Model Random Forests
modRF <- train(classe~., data=trainData, method="parRF", trControl=trControlRF)


predictRF <- predict(modRF, newdata=testData)
cnfmrf <- confusionMatrix(predictRF, testData$classe)
cnfmrf