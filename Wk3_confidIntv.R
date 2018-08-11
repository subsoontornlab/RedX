library(downloader)
library("dplyr")
library(rafalib)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)

bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

# confidence interval with t distribution
N = 25
# take sample
set.seed(1)
dat.ns = sample(bwt.nonsmoke, N)
dat.s = sample(bwt.smoke, N)



qt99 = qt(0.005, 2*N-2)
sqrt(sd(dat.ns)^2/25 + sd(dat.s)^2/25)*qt99


# power of the test (type II error)
N = 5
set.seed(1)
dat.ns = sample(bwt.nonsmoke, N)
dat.s = sample(bwt.smoke, N)
t.test(dat.ns, dat.s)

