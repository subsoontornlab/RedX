library(downloader) 
library("dplyr")
library("rafalib")
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- na.omit( read.csv(filename) )

pnorm(1,0,1) - pnorm(-1,0,1)
pnorm(2,0,1) - pnorm(-2,0,1)
pnorm(3,0,1) - pnorm(-3,0,1)


# 1 2 3 sd away

xm =filter(dat, Diet == "chow", Sex == "M") %>% select(Bodyweight) %>% unlist()

meanxm = mean(xm)
sdxm = popsd(xm)
zxm = (xm - meanxm)/sdxm

mean(zxm < 1) - mean(zxm < -1)
mean(zxm < 2) - mean(zxm < -2)
mean(zxm < 3) - mean(zxm < -3)

###### make qq plot

mypar(2,2)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)

# CTL exercise
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)

mean(avgs)

sd(avgs)




