rm(list=ls())
library(ISLR); data(Wage); library(ggplot2); library(caret);
Wage <- subset(Wage,select=-c(logwage))
inBuild <- createDataPartition(y=Wage$wage,
                               p=0.7, list=FALSE)
validation <- Wage[-inBuild,]; buildData <- Wage[inBuild,]
inTrain <- createDataPartition(y=buildData$wage,
                               p=0.7, list=FALSE)
training <- buildData[inTrain,]; testing <- buildData[-inTrain,]

set.seed(1234)
mod1 <- train(wage ~.,method="glm",data=training,verbose=FALSE)
mod2 <- train(wage ~.,method="rf",
              data=training, 
              trControl = trainControl(method="cv"),number=3)
pred1 <- predict(mod1,testing) 
pred2 <- predict(mod2,testing)
qplot(pred1,pred2,colour=wage,data=testing)
install.packages("nlme")
library(nlme)
predDF <- data.frame(pred1,pred2,wage=testing$wage)
combModFit <- train(wage ~.,method="gam",data=predDF)
combPred <- predict(combModFit,predDF)
sqrt(sum((pred1-testing$wage)^2))
sqrt(sum((pred2-testing$wage)^2))
sqrt(sum((combPred-testing$wage)^2))



install.packages("quantmod")
library(quantmod)
install.packages("forecast")
library(forecast)
from.dat <- as.Date("01/01/08", format="%m/%d/%y")
to.dat <- as.Date("12/31/13", format="%m/%d/%y")
getSymbols("GOOG", src="yahoo", from = from.dat, to = to.dat)
head(GOOG)

X11()
mGoog <- to.monthly(GOOG)
googOpen <- Op(mGoog)
ts1 <- ts(googOpen,frequency=12)
plot(ts1,xlab="Years+1", ylab="GOOG")

plot(decompose(ts1),xlab="Years+1")
ts1Train <- window(ts1,start=1,end=5)
ts1Test <- window(ts1,start=5,end=(7-0.01))
plot(ts1Train)
lines(ma(ts1Train,order=3),col="red")

ets1 <- ets(ts1Train,model="MMM")
fcast <- forecast(ets1)
plot(fcast); lines(ts1Test,col="red")
accuracy(fcast,ts1Test)
