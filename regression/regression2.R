install.packages("UsingR")
library(UsingR); library(ggplot2)
data(diamond)
g = ggplot(diamond, aes(x=carat,y=price))
g = g+xlab("Mass (carats)");g = g+ylab("price")
g = g + geom_point(size=6,colour="black", alpha=0.2)
g = g + geom_point(size=5,colour="blue", alpha=0.2)
g = g+ geom_smooth(method="lm",colour="red")
x11()
g
fit1 = lm(price~ carat,data=diamond)
# lm 식내에서 방정식을 쓰기 위함
summary(fit2)
e = resid(fit1)
yhat = predict(fit1)
y = diamond$price; x= diamond$carat
max(abs(e-(y-yhat)))

plot(x,e,bg = "lightblue",col="black",cex=2,pch=21,frame=F)
abline(h=0,lwd=2)
for (i in 1:length(x))
    lines(c(x[i],x[i]),c(e[i],0),col="red")
sqrt(sum(resid(fit1)^2)/(length(x)-2))

beta1 = cor(y,x)*sd(y)/sd(x)
beta0 = mean(y) - beta1*mean(x)
e = y - beta0- beta1*x
sigma = sqrt(sum(e^2)/(length(x)-2))
ssx = sum((x-mean(x))^2)
sebeta0 = (1/length(x)+mean(x)^2/ssx)^.5 *sigma
sebeta1 = sigma/sqrt(ssx)
tbeta0 = beta0 / sebeta0; tbeta1 = beta1/sebeta1
pbeta0 = 2*pt(abs(tbeta0),df = length(x)-2,lower.tail = F)
pbeta1 = 2*pt(abs(tbeta1),df = length(x)-2,lower.tail = F)
coeftable = rbind(c(beta0, sebeta0,tbeta0,pbeta0),c(beta1, sebeta1,tbeta1,pbeta1))
colnames(coeftable) = c("estimate", " std.error", "t value","P>|t|")
rownames(coeftable) = c("intercept","x")


sumcoef = summary(fit1)$coefficients
sumcoef[1,1] + c(-1,1)*qt(.975,df=fit1$df)*sumcoef[1,2]


#quiz
x <- c(0.61, 0.93, 0.83, 0.35, 0.54, 0.16, 0.91, 0.62, 0.62)
y <- c(0.67, 0.84, 0.6, 0.18, 0.85, 0.47, 1.1, 0.65, 0.36)
fit = lm(y~x)
summary(fit)
sum(resid(fit)^2)

sum((x-mean(x))^2)
data(mtcars)
fit1 = lm(mpg~wt,data = mtcars)
fit2 = lm(mpg~1,data = mtcars)
summary(fit1)
pred1 = predict(fit1)
pred2 = predict(fit2)
sum((mtcars$mpg-pred1)^2)
sumcoef = summary(fit1)$coefficients
sumcoef[1,1] + c(-1,1)*qt(.975,df=fit1$df)*sumcoef[1,2]
predict(fit1, newdata = data.frame(wt = 2), interval = "confidence")
