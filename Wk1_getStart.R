install.packages("downloader")

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv" 
download(url, destfile=filename)

dat = read.csv("femaleMiceWeights.csv")

dat[12,2]

bdwt = dat$Bodyweight

length(dat$Bodyweight)

mean(bdwt[13:24])

set.seed(1)
sample(13:24,1)