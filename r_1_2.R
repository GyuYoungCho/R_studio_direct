y = matrix(rep(10,4),2,2);y
x=4;class(x)
complex(4,2)
x <- list(2, "a", "b", TRUE);x[[2]]

x <- c(17, 14, 4, 5, 13, 12, 10)
x[x>=11]<-4
x
x=1:4;y=2;x+y
x = read.csv("hw1_data.csv")
names(x)
tail(x)
x[47,]
is.na(x)
mean(x[,1],na.rm = T)
x1 = x[(x$Ozone > 31)&(x$Temp>90),]
x1
mean(x1$Solar.R,na.rm = T)
x2 = x[x$Month==6,];mean(x2$Temp,na.rm = T)
x2 = x[x$Month==5,]
max(x2$Ozone,na.rm = T)
sum(is.na(x$Ozone))

#2
cube <- function(x, n) {
    x^3
}
cube(3)
x <- 1:10
if(x > 5) {
    x <- 0
}
f <- function(x) {
    g <- function(y) {
        y + z
    }
    z <- 4
    x + g(x)
}
z <- 10
f(3)

# assignment
pollutantmean <-function(directory,pollutant, id = 1:332){
    filelist = list.files(path=directory, pattern = ".csv",full.names = TRUE)
    values = numeric()
    for (i in id){
        data = read.csv(filelist[i])
        values = c(values,data[[pollutant]])
    }
    mean(values, na.rm = T)
}


complete <-function(directory,id=1:332)
{
    filelist = list.files(path=directory, pattern = ".csv",full.names = TRUE)
    values = numeric()
    for (i in id){
        data = read.csv(filelist[i])
        values = c(values,sum(complete.cases(data)))
    }
    data.frame(id,values)
}

corr <- function(directory, threshold = 0) {
    csv_files <- list.files(directory, full.names = TRUE)
    v <- vector(mode = "numeric", length = 0)
    for (i in 1:length(csv_files)) {
        x <- read.csv(csv_files[i])
        csum <- sum(complete.cases(x))

        if (csum > threshold) {
            xSulfate <- x[which(!is.na(x$sulfate)), ]
            xPollutant <- xSulfate[which(!is.na(xSulfate$nitrate)), ]
            v <- c(v, cor(xPollutant$sulfate, xPollutant$nitrate))
        }
    }
    return(v)
}

set.seed(42)
pollutantmean("C:/Users/Gyu/Documents/rstudio_file/data/rprog_data_specdata/specdata/","nitrate")
cc = complete("C:/Users/Gyu/Documents/rstudio_file/data/rprog_data_specdata/specdata/", 332:1)
use = sample(332,10)
cc[use,"values"]
cr <- corr("C:/Users/Gyu/Documents/rstudio_file/data/rprog_data_specdata/specdata",129)
cr = sort(cr)
set.seed(197)
out = c(length(cr), round(cr[sample(length(cr),5)],4))
out

cr <- corr("C:/Users/Gyu/Documents/rstudio_file/data/rprog_data_specdata/specdata",2000)
n = length(cr)
cr <- corr("C:/Users/Gyu/Documents/rstudio_file/data/rprog_data_specdata/specdata",1000)
cr = sort(cr)
c(n,round(cr,4))
