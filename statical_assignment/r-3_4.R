#3
x = list(a=1:5,b=rnorm(10))
lapply(x,mean)
x = list(a=matrix(1:4,2,2),b=matrix(1:6,3,2))
lapply(x,function(elt) elt[,1])         
x
x = matrix(rnorm(200),20,10)
apply(x,2,mean)
apply(x,1,quantile, probs=c(0.25,0.75))

mapply(rep,1:4,4:1)
# list(rep(1,4),rep(2,3),rep(3,2),rep(4,1))

data(iris)
mean(iris[iris$Species=="virginica",]$Sepal.Length)
apply(iris[,1:4],2,mean)
data(mtcars)
str(mtcars)

sapply(split(mtcars$mpg, mtcars$cyl),mean)
with(mtcars,tapply(mpg,cyl,mean))
tapply(mtcars$mpg, mtcars$cyl, mean)

mean(mtcars[mtcars$cyl==4,]$hp) - mean(mtcars[mtcars$cyl==8,]$hp)

#4
set.seed(1)
rpois(5,2)


