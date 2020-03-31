dat = read.csv("getdata_data_ss06hid.csv")
sum(dat$HINCP > 1000000,na.rm = T)
install.packages("xlsx")
library(xlsx)
sum(dat[dat$WORKSTAT>1000000])
dat$FES
data <- read("getdata_data_DATA.gov_NGAP.xlsx",sheetIndex=1,header=TRUE)

dat = read.csv("getdata_data_ss06pid.csv")
dat$pwgtp15
tapply(dat$pwgtp15,dat$SEX,mean)
library(data.table)
dat = data.table(dat)
dat[,mean(pwgtp15),by=SEX]
