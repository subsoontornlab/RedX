library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

set.seed(1)
N = 1000
meanList = vector("numeric", N)
for(n in 1:N){
  meanList[n] = mean(sample(x, 5))
}

p = sum(abs(mean(x) - meanList) > 1)/N

##############

set.seed(1)
N = 10000
meanList = vector("numeric", N)
for(n in 1:N){
  meanList[n] = mean(sample(x, 5))
}

p = sum(abs(mean(x) - meanList) > 1)/N
p

######

set.seed(1)
N = 10000
meanList = vector("numeric", N)
for(n in 1:N){
  meanList[n] = mean(sample(x, 50))
}

p = sum(abs(mean(x) - meanList) > 1)/N
p
