library(ggplot2); library(caret)
set.seed(1)
rm(list=ls())
train1 = read.csv('pml-training.csv',row.names = 1,header = T,)
test1 = read.csv('pml-testing.csv',row.names = 1,header = T)

nalevel = sapply(train1,function(x) mean((is.na(x))|x==""))>0.9
train1 = train1[,nalevel==F]
nalevel = sapply(test1,function(x) mean(is.na(x)))>0.9
test1 = test1[,nalevel==F]
dim(train1);dim(test1)

NZV = nearZeroVar(train1)
train1 = train1[, -NZV]
test1  = test1[, -NZV]

train1 = train1[,-c(1:4)]
test1 = test1[,-c(1:4)]
inTrain  = createDataPartition(train1$classe, p=0.7, list=FALSE)
train_set = train1[inTrain, ]
test_set  = train1[-inTrain, ]

control1 <- trainControl(method="cv", number=5, verboseIter=FALSE)
modrf <- train(classe~., data=train_set, method="rf",
                          trControl=control1)
predrf <- predict(modrf, newdata=test_set)
confusionMatrix(predrf,test_set$classe)

modgbm <- train(classe~., data=train_set, method="gbm",
               trControl=control1,verbose=FALSE)
predgbm <- predict(modgbm, newdata=test_set)
confusionMatrix(predgbm,test_set$classe)

predva <- predict(modrf, newdata=test1)
predva <- predict(modgbm, newdata=test1)
