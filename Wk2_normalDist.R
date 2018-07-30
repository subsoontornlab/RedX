library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

set.seed(1)
N = 1000
samplesize = 5
meanList1 = vector("numeric", N)
for(n in 1:N){
  meanList1[n] = mean(sample(x, samplesize))
}
par(mar = c(1,1,1,1))
hist(meanList1)



set.seed(1)
N = 1000
samplesize = 50
meanList2 = vector("numeric", N)
for(n in 1:N){
  meanList2[n] = mean(sample(x, samplesize))
}
par(mar = c(1,1,1,1))
hist(meanList2)

mean(meanList2 <=25) - mean(meanList2 <=23)

#Now ask the same question of a normal distribution
#with average 23.9
#and standard deviation 0.43.
pnorm(25, 23.9, 0.43) - pnorm(23, 23.9, 0.43) 
