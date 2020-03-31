library(ggplot2)
data(mtcars)
str(mtcars)
mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
mtcars$am <- factor(mtcars$am,labels=c('Automatic','Manual'))


boxplot(mpg ~ am, data=mtcars, col=(c("red","lightblue")), xlab="Transmission", ylab="mpg", main="compare Automatic vs Manual")
tapply(mtcars$mpg,mtcars$am,mean)
t.test(mpg ~ am, data = mtcars)
trans_lm = lm(mpg~am,data=mtcars)
summary(trans_lm)

base_fit <- lm(mpg ~ ., data = mtcars)
fit1 = step(base_fit,direction = "both")
summary(fit1)
anova(fit1,base_fit)

par(mfrow = c(2, 2))
plot(fit1)
install.packages("tinytex")
