choose(8,6)

n <- 20
pvals <- seq(0.1, 0.9, by = 0.05)
nosim <- 1000
coverage <- sapply(pvals, function(p) {
    phats <- rbinom(nosim, prob = p, size = n)/n
    ll <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n)
    ul <- phats + qnorm(0.975) * sqrt(phats * (1 - phats)/n)
    mean(ll < p & ul > p)
})
plot(coverage)

n=100
coverage2 <- sapply(pvals, function(p) {
    phats <- rbinom(nosim, prob = p, size = n)/n
    ll <- phats - qnorm(0.975) * sqrt(phats * (1 - phats)/n)
    ul <- phats + qnorm(0.975) * sqrt(phats * (1 - phats)/n)
    mean(ll < p & ul > p)
})
plot(coverage2)

ppois(q = 10,lambda = 15)
qnorm(p = 0.95,mean = 1100,sd = 75/10)

qt(p=0.95,df = 9)*sqrt((1.5^2)/9 + (1.8^2)/9)
d1 = rnorm(10,3,.6)
d2 = rnorm(10,5,.68)
t.test(d1,d2,paired = T)


library(ggplot2)
install.packages("manipulate")
library(manipulate)
mu0 = 30
myplot <- function(sigma, mua, n, alpha){
    g = ggplot(data.frame(mu = c(27, 36)), aes(x = mu))
    g = g + stat_function(fun=dnorm, geom = "line", 
                          args = list(mean = mu0, sd = sigma / sqrt(n)), 
                          size = 2, col = "red")
    g = g + stat_function(fun=dnorm, geom = "line", 
                          args = list(mean = mua, sd = sigma / sqrt(n)), 
                          size = 2, col = "blue")
    xitc = mu0 + qnorm(1 - alpha) * sigma / sqrt(n)
    g = g + geom_vline(xintercept=xitc, size = 3)
    g
}
x11()
manipulate(
    myplot(sigma, mua, n, alpha),
    sigma = slider(1, 10, step = 1, initial = 4),
    mua = slider(30, 35, step = 1, initial = 32),
    n = slider(1, 50, step = 1, initial = 16),
    alpha = slider(0.01, 0.1, step = 0.01, initial = 0.05)
)


t.test(c(140,138,150,148,135),c(132,135,151,146,130),alternative = "two.sided",paired = TRUE)
qt(p=0.95,df = 8) * 30
power.t.test(n = 100, delta = .01, sd=.04, type = "one.sample",  alt = "one.sided")$power
a = matrix(c(3,1,1,3),2,2)
a = as.table(a)
chisq.test(a)
fisher.test(a)
t.test(rnorm(9,-3,1.5),rnorm(9,1,1.8),alternative = "less")
