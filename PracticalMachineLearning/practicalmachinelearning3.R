rm(list=ls())

library(ggplot2)
data(iris)
library(caret)
intrain = createDataPartition(y=iris$Species,p=0.7,list=F)
training = iris[intrain,]
testing = iris[-intrain,]
modfit = train(Species~. , method="rpart",data = training)

x11()
plot(modfit$finalModel,uniform=T)
text(modfit$finalModel,use.n = T,all = T,cex=.8)

modFit <- train(Species~ .,data=training,method="rf",prox=TRUE)
modFit
pred <- predict(modFit,testing); 
testing$predRight <- pred==testing$Species
table(pred,testing$Species)
qplot(Petal.Width,Petal.Length,colour=predRight,data=testing,main="newdata Predictions")

Wage = subset(Wage,select=-c(logwage))
inTrain = createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
training = Wage[inTrain,]; testing <- Wage[-inTrain,]
modFit = train(wage ~ ., method="gbm",data=training,verbose=FALSE)
print(modFit)

intrain = createDataPartition(y=iris$Species,p=0.7,list=F)
training = iris[intrain,]
testing = iris[-intrain,]

modlda = train(Species ~ .,data=training,method="lda")
modnb = train(Species ~ ., data=training,method="nb")
plda = predict(modlda,testing); pnb = predict(modnb,testing)
table(plda,pnb)

equalPredictions = (plda==pnb)
qplot(Petal.Width,Sepal.Width,colour=equalPredictions,data=testing)

# quiz
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
head(segmentationOriginal)
training = segmentationOriginal[segmentationOriginal$Case=="Train",]
testing = segmentationOriginal[segmentationOriginal$Case=="Test",]
modfit = train(Class~. , method="rpart",data = training)
x11()
set.seed(125)
plot(modfit$finalModel,uniform=T)
text(modfit$finalModel,use.n = T,all = T,cex=.8)

install.packages("pgmm")
library(pgmm)
data(olive)
olive = olive[,-1]
str(olive)
newdata = as.data.frame(t(colMeans(olive)))
head(newdata)
modfit1 = train(Area~., method="rpart",data = olive)
complete.cases(olive)
predict(modfit1,newdata)

