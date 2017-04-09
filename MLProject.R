library(data.table)
library(caret)


## Loading data
training <- fread("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",na.strings=c("NA","#DIV/0!",""))
testing <- fread("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",na.strings=c("NA","#DIV/0!",""))

## Cleaning Data
training <- data.frame(training[,-c(1:7)])
training <- training[,colSums(is.na(training))==0]
testing <- data.frame(testing[,-c(1:7)])
testing <- testing[,colSums(is.na(testing))==0]

## set up Cross-Validation
inTrain <- createDataPartition(training$classe,p=0.7,list=F)
train <- training[inTrain,]
cv <- training[-inTrain,]

## Train using 3 models
fitRF <- train(classe~.,data=train,method="rf")
fitGBM <- train(classe~.,data=train,method="gbm")
fitLDA <- train(classe~.,data=train,method="lda")

## Predicions
predRF <- predict(fitRF,cv)
predGBM <- predict(fitGBM,cv)
predLDA <- predict(fitLDA,cv)

## Show Accuracies
results <- data.frame("Accuracy" = c(confusionMatrix(predRF,cv$classe)$overall[1],confusionMatrix(predGBM,cv$classe)$overall[1],confusionMatrix(predLDA,cv$classe)$overall[1]),row.names = c("Random Forest","Boosted Trees","Linear Discriminant Analysis"))
results

## Test data
predictions <- predict(fitRF,testing)
predictions
