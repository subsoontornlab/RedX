#baby data set
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)

library("dplyr")

#smolker vs non-smolker baby wt
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist


#Now, we can look for the true population difference 
#in means between smoking and non-smoking birth weights.

library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

# draw sample of 25 from each population
set.seed(1)
dat.ns = sample(bwt.nonsmoke, 25)
dat.s = sample(bwt.smoke, 25)

t.test(dat.ns, dat.s)

# probability a t-statistic that actually does follow
#the null hypothesis would have larger absolute value
#than the absolute value of the t-value we just observed
1- (pnorm(2.1209) - pnorm(-2.1029))


# 99% confident interval
z99 = qnorm(0.995)
sqrt(sd(dat.ns)^2/25 + sd(dat.s)^2/25)*z99 # var 99%


