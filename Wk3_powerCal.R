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

# power VS type-I error trade off
set.seed(1)
dat.ns = sample(bwt.nonsmoke, 5)
dat.s = sample(bwt.smoke, 5)
t.test(dat.ns, dat.s)$p.value

# repeate 10,000 time and see proportion of rej 0.05
drawN = function(N){
  dat.ns = sample(bwt.nonsmoke, N)
  dat.s = sample(bwt.smoke, N)
  t.test(dat.ns, dat.s)$p.value
}
set.seed(1)
drawN(5)
B = 10000 # number repeat
repeatDraw = replicate(B, drawN(5))
1-mean(repeatDraw > 0.05)

# check N vs power relationship
repDrawN = function(N){
  repeatDraw = replicate(B, drawN(N))
  1-mean(repeatDraw > 0.05)
}
  
Ns = seq(30, 120, 30)
set.seed(1)
power = sapply(Ns,function(N){
  repeatDraw = replicate(B, drawN(N))
  1-mean(repeatDraw > 0.05) 
})

plot(Ns, power, type = "b")

# repeate with alpha 0.01
set.seed(1)
power = sapply(Ns,function(N){
  repeatDraw = replicate(B, drawN(N))
  1-mean(repeatDraw > 0.01) 
})

plot(Ns, power, type = "b")

