rm(list=ls())
library(ggplot2);library(stats)
data(swiss)
summary(lm(Fertility ~ . , data = swiss))
data(InsectSprays)

x11()
g = ggplot(data = InsectSprays, aes(y=count, x=spray, fill = spray))
g = g+ geom_violin(colour = "black",size=2)
g
summary(lm(count ~ spray, data = InsectSprays))$coef
summary(lm(count ~ 
               I(1 * (spray == 'B')) + I(1 * (spray == 'C')) + 
               I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
               I(1 * (spray == 'F'))
           , data = InsectSprays))$coef


summarise(group_by(InsectSprays, spray), mn = mean(count))
spray2 <- relevel(InsectSprays$spray, "C")
summary(lm(count ~ spray2, data = InsectSprays))$coef

swiss = mutate(swiss, CatholicBin = 1 * (Catholic > 50))

g = ggplot(swiss, aes(y=Fertility, x=Agriculture, colour = factor(CatholicBin)))
g = g+ geom_point(colour = "black",size=6) + geom_point(size=4)
g = g + ylab("fertility")
g


fit1 = lm(Fertility ~ Agriculture, data = swiss)
g1 = g
g1 = g1 +geom_abline(intercept = coef(fit1)[1], slope = coef(fit1)[2],size=2)

fit2 = lm(Fertility ~ Agriculture+ factor(CatholicBin), data = swiss)
g2 = g
g2 = g2 +geom_abline(intercept = coef(fit2)[1], slope = coef(fit2)[2],size=2)
g2 = g2 +geom_abline(intercept = coef(fit2)[1]+coef(fit2)[3], slope = coef(fit2)[2],size=2)
g2

fit3 = lm(Fertility ~ Agriculture* factor(CatholicBin), data = swiss)
g3 = g
g3 = g3 +geom_abline(intercept = coef(fit3)[1], slope = coef(fit3)[2],size=2)
g3 = g3 +geom_abline(intercept = coef(fit3)[1]+coef(fit3)[3], slope = coef(fit3)[2]+coef(fit3)[4],size=2)
g3


n <- 100; x <- c(10, rnorm(n)); y <- c(10, c(rnorm(n)))
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y ~ x))  
fit <- lm(y ~ x)
round(dfbetas(fit)[1 : 10, 2], 3)
round(hatvalues(fit)[1 : 10], 3)

dat <- read.table('http://www4.stat.ncsu.edu/~stefanski/NSF_Supported/Hidden_Images/orly_owl_files/orly_owl_Lin_4p_5_flat.txt', header = FALSE)
pairs(dat)

fit <- lm(V1 ~ . - 1, data = dat); plot(predict(fit), resid(fit), pch = '.')


n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n); 
betas <- sapply(1 : nosim, function(i){
    y <- x1 + rnorm(n, sd = .3)
    c(coef(lm(y ~ x1))[2], 
      coef(lm(y ~ x1 + x2))[2], 
      coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)

fit = lm(Fertility ~ ., data = swiss)
library(car)
vif(fit)



#quiz
data(mtcars)
mtcars$cyl = as.factor(mtcars$cyl)
fit = lm(mpg~cyl+wt,data=mtcars)
summary(fit)
fit2 = lm(mpg~cyl,data=mtcars)
summary(fit2)
anova(fit,fit2)
summary(lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars))

x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit4 <- lm(y ~ x)
round(dfbetas(fit4)[1 : 5, 2], 3)
round(hatvalues(fit4)[1 : 5], 4)

fit4 <- lm(y ~ x)
round(dfbetas(fit4)[1 : 5, 2], 3)
round(hatvalues(fit4)[1 : 5], 4)
