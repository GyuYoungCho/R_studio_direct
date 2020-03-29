library(caret)
install.packages("kernlab")
library(kernlab)
install.packages("e1071")
library(e1071)
data(spam)
intrain = createDataPartition(y=spam$type,p=0.75,list = F)
training = spam[intrain,]
testing = spam[-intrain,]
dim(training)
set.seed(32343)
modelfit = train(type~.,data=training,method = "glm")
modelfit
modelfit$finalModel
prediction = predict(modelfit,newdata=testing)
confusionMatrix(prediction,testing$type)


folds = createFolds(y=spam$type, k=10,list=TRUE, returnTrain = TRUE)
sapply(folds,length)
folds = createResample(y=spam$type, times=10,list=TRUE)
sapply(folds,length)
tme = 1000
folds = createTimeSlices(y=tme, initialWindow = 20,horizon = 10)
names(folds)

install.packages("ISLR")
library(ISLR)
library(ggplot2)
library(caret)
data("Wage")
summary(Wage)

intrain = createDataPartition(y=Wage$wage,p=0.7,list = F)
training = Wage[intrain,]
testing = Wage[-intrain,]
dim(training)
featurePlot(x=training[,c("age","education","jobclass")],y=training$wage,plot="pairs")
qplot(age,wage,colour=jobclass, data=training)
x11()
qq = qplot(age,wage,colour=education, data=training)
qq + geom_smooth(method='lm',formula = y~x)

install.packages("Hmisc")
library(Hmisc)
cutwage = cut2(training$wage,g=3)
table(cutwage)
p1 = qplot(cutwage,age,data=training,fill=cutwage,geom=c("boxplot"))
p2 = qplot(cutwage,age,data=training,fill=cutwage,geom=c("boxplot","jitter"))
p2

t1 = table(cutwage, training$jobclass)
prop.table(t1,1)
qplot(wage,colour=education,data=training, geom="density")



intrain = createDataPartition(y=spam$type,p=0.75,list = F)
training = spam[intrain,]
testing = spam[-intrain,]
preobj = preProcess(training[,-58],method=c("center","scale"))
traincap = predict(preobj,training[,-58])$capitalAve
testcap = predict(preobj,testing[,-58])$capitalAve

preobj = preProcess(training[,-58],method=c("BoxCox"))
traincaps = predict(preobj,training[,-58])$capitalAve
qqnorm(traincaps)

set.seed(13343)
training$capAve = training$capitalAve
selectna = rbinom(dim(training)[1],size=1,prob = 0.05)==1
training$capAve[selectna] = NA

install.packages("RANN")
library(RANN)
preobj = preProcess(training[,-58],method = "knnImpute")
capAve = predict(preobj,training[,-58])$capAve

capAvetr = training$capitalAve
capAvetr = (capAvetr-mean(capAvetr))/sd(capAvetr)

quantile(capAve - capAvetr)
quantile((capAve - capAvetr)[selectna])
quantile((capAve - capAvetr)[!selectna])


rm(list=ls())
data(Wage)
intrain = createDataPartition(y=Wage$wage,p=0.7,list = F)
training = Wage[intrain,]
testing = Wage[-intrain,]

dummies = dummyVars(wage~jobclass,data=training)
head(predict(dummies,newdata=training))
# 변동성 없는 변수
nearZeroVar(training,saveMetrics = T)

# 회귀에서 사용(다항 만들기)
library(splines)
bsBasis = bs(training$age,df=3)

lm1 = lm(wage~bsBasis,data=training)
plot(training$age,training$wage,pch=10,cex=0.5)
points(training$age,predict(lm1,newdata=training),col="red",pch=19,cex=1)


intrain = createDataPartition(y=spam$type,p=0.75,list = F)
training = spam[intrain,]
testing = spam[-intrain,]

m = abs(cor(training[,-58]))
diag(m) =0
which(m>0.8,arr.ind = T)
plot(spam[,34],spam[,32])
small = spam[,c(32,34)]
prco = prcomp(small)
prco$rotation
typecolor = ((spam$type=="spam")*1+1)
prco = prcomp(log10(spam[-58]+1))
plot(prco$x[,1],prco$x[,2],col=typecolor)


prcoc =  preProcess(log10(spam[-58]+1),method = "pca",pcaComp = 2)
spampc = predict(prcoc,log10(spam[-58]+1))
plot(spampc[,1],spampc[,2],col=typecolor)

prcoc =  preProcess(log10(training[,-58]+1),method = "pca",pcaComp = 2)
trainpc = predict(prcoc,log10(training[,-58]+1))
modelfit = train(training$type ~ .,method = "glm",data=trainpc)
testpc = predict(prcoc,log10(testing[,-58]+1))
confusionMatrix(testing$type,predict(modelfit,testpc))

modelfit = train(type ~ .,method = "glm",preProcess="pca", data=training)
confusionMatrix(testing$type,predict(modelfit,testing))


#quiz

#2
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
training$inde = 1:774
training$cutcs = cut2(training$CompressiveStrength,g=3)
qplot(inde, cutcs,colour=Age, data=training)

#3
sp = log10(training$Superplasticizer)
qplot(Superplasticizer, data=training)
qplot(sp)
range(sp)
#4
set.seed(3433);data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
tr = training[,c(1,58:69)]
head(tr)
prtr =  preProcess(tr,method = "pca",thresh = 0.9)
prtr$rotation

#5
modelfit = train(diagnosis~.,data=tr,method = "glm")
prediction = predict(modelfit,newdata=testing[,c(1,58:69)])
confusionMatrix(prediction,testing$diagnosis)

modelfit = train(diagnosis~.,data=tr,preProcess="pca", method = "glm")
prediction = predict(modelfit,newdata=testing[,c(1,58:69)])
confusionMatrix(prediction,testing$diagnosis)
