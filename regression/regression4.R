rm(list=ls())
download.file("https://dl.dropboxusercontent.com/u/7710864/data/ravensData.rda", destfile="ravensData.rda",method="curl")
load("ravensData.rda")


logRegRavens <- glm(ravensData$ravenWinNum ~ ravensData$ravenScore,family="binomial")
summary(logRegRavens)

n <- 500; x <- seq(0, 4 * pi, length = n); y <- sin(x) + rnorm(n, sd = .3)
knots <- seq(0, 8 * pi, length = 20); 
splineTerms <- sapply(knots, function(knot) (x > knot) * (x - knot))
xMat <- cbind(1, x, splineTerms)
yhat <- predict(lm(y ~ xMat - 1))
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)


notes4 <- c(261.63, 293.66, 329.63, 349.23, 392.00, 440.00, 493.88, 523.25)
t <- seq(0, 2, by = .001); n <- length(t)
c4 <- sin(2 * pi * notes4[1] * t); e4 <- sin(2 * pi * notes4[3] * t); 
g4 <- sin(2 * pi * notes4[5] * t)
chord <- c4 + e4 + g4 + rnorm(n, 0, 0.3)
x <- sapply(notes4, function(freq) sin(2 * pi * freq * t))
fit <- lm(chord ~ x - 1)
summary(fit)
a = fft(chord); plot(Re(a)^2,type = "l")
plot(fit$coefficients,type="l")


#QUIZ
library(MASS)
data("shuttle")
str(shuttle)
install.packages("glmnet")
library(glmnet)
fit1 = glm(use~wind, data=shuttle, family =binomial(link = "logit"))
summary(fit1)
exp(-0.031)

data("InsectSprays")
fit2 = glm(count~spray,data=InsectSprays, family = poisson(link = "log"))
summary(fit2)
1/exp(0.05588)

x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
z <- (x > 0) * x
fit <- lm(y ~ x + z)
summary(fit)
coef(fit)
