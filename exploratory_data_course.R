library(lattice)
library(datasets)
airquality = transform(airquality,Month = factor(Month))
x11()
p = xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5,1))
p

library(ggplot2)
str(mpg)
qplot(displ, hwy, data = mpg,color=drv)
qplot(displ, hwy, data = mpg,geom = c("point", "smooth"))
qplot(displ, data= mpg,fill=drv)
qplot(displ, hwy, data = mpg,color=drv)
qplot(displ, hwy, data = mpg,facets=drv~. ,binwidth=2)
qplot(displ, hwy, data = mpg,facets=.~drv)


str(maacs)

g = ggplot()
g + geom_point() + geom_smooth()


testdat = data.frame(x= 1:100, y=rnorm(100))
testdat[50,2] = 100
g = ggplot(testdat, aes(x=x,y=y))
g + geom_line()+coord_cartesian(ylim=c(-3,3))
xyplot()

library(nlme)
library(lattice)
xyplot(weight ~ Time | Diet, BodyWeight)



set.seed(1234)
x = rnorm(12,rep(1:3,each=4),0.2)
y = rnorm(12,rep(c(1,2,1),each=4),0.2)
plot(x,y)
data = data.frame(x=x,y=y)
distxy = dist(data)
hcluster = hclust(distxy)
plot(hcluster)
datamat = as.matrix(data)[sample(1:12),]
heatmap(datamat)


kmeansob = kmeans(data,centers = 3)
names(kmeansob)
plot(x,y,col = kmeansob$cluster,pch=19)
points(kmeansob$centers,col=1:3,pch=3)
kmeansob2 = kmeans(datamat,centers = 3)
par(mfrow=c(1,2),mar=c(2,4,0.1,0.1))
image(t(datamat)[,nrow(datamat):1],yaxt = "n")
image(t(datamat)[,order(kmeansob$cluster)],yaxt = "n")


rm(list=ls())
set.seed(12345)
datamat <- matrix(rnorm(400), nrow = 40)
set.seed(678910)
for (i in 1:40){
    coinflip = rbinom(1,size=1,prob = 0.5)
    if (coinflip){
        datamat[i,] <- datamat[i,] + rep(c(0,3),each=5)
    }
}
x11()
hh <- hclust(dist(datamat))
dataMatrixOrdered <- datamat[hh$order, ]
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered), 40:1, , xlab = "Row Mean", ylab = "Row", pch = 19)
plot(colMeans(dataMatrixOrdered), xlab = "Column", ylab = "Column Mean", pch = 19)


svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd1$u[, 1], 40:1, , xlab = "Row", ylab = "First left singular vector", 
     pch = 19)
plot(svd1$v[, 1], xlab = "Column", ylab = "First right singular vector", pch = 19)

par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained", 
     pch = 19)
