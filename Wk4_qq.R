library(downloader)
library("dplyr")
library(rafalib)

load("skew.RData") # 1000x9 dimensional data
dim(dat)
dat[,1]
par(mfrow = c(3,3))
par(mar=c(1,1,1,1))
for (i in 1:9){
  qqnorm(dat[,i])
  qqline(dat[,i])
}