install.packages("rafalib")
library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- read.csv(filename) 

dat <- na.omit( dat )

library("dplyr")

head(dat)

x =filter(dat, Diet == "chow", Sex == "M") %>% select(Bodyweight) %>% unlist()
mean(x)

library("rafalib")
#population standard deviation
popsd(x)






set.seed(1)
meanxSample = mean(sample(x, 25))


y =filter(dat, Diet == "hf", Sex == "M") %>% select(Bodyweight) %>% unlist()
mean(y)

popsd(y)

set.seed(1)
meanySample = mean(sample(y, 25))

abs(mean(y) - mean(x) - meanySample + meanxSample)


# population vs sample mean
xf =filter(dat, Diet == "chow", Sex == "F") %>% select(Bodyweight) %>% unlist()
yf =filter(dat, Diet == "hf", Sex == "F") %>% select(Bodyweight) %>% unlist()
set.seed(1)
meanxSampleF = mean(sample(xf, 25))
set.seed(1)
meanySampleF = mean(sample(yf, 25))
abs(mean(yf) - mean(xf) - meanySampleF + meanxSampleF)