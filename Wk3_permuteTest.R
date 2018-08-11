library(downloader)
library("dplyr")
library(rafalib)

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)
bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

# random var  on sample size 10, see mean difference
N=10
set.seed(1)
nonsmokers <- sample(bwt.nonsmoke , N)
smokers <- sample(bwt.smoke , N)
obs <- mean(smokers) - mean(nonsmokers)

# test significance without relying on normal or t-dist 
# assumption
dat <- c(smokers,nonsmokers)
shuffle <- sample( dat )  # permute the entire thing
smokersstar <- shuffle[1:N]
nonsmokersstar <- shuffle[(N+1):(2*N)]
mean(smokersstar)-mean(nonsmokersstar)

# permute function
permSample = function(testSet, controlSet, N){
  dat = c(testSet,controlSet)
  shuffle = sample(dat)
  testSample = shuffle[1:N]
  controlSample = shuffle[(N+1):(2*N)]
  mean(testSample) - mean(controlSample)
}

set.seed(1)
N=10
B = 1000
avgdiff = replicate(B, permSample(smokers, nonsmokers,N))
#We add a 1 to the numerator and denominator to account
#for misestimation of the p-value (for more details see 
#Phipson and Smyth, Permutation P-values should never be zero).

pval = (sum(abs(avgdiff) > abs(obs)) + 1) / (length(avgdiff) + 1)

# check p value for median difference
obsMed <- median(smokers) - median(nonsmokers)

permSampleMed = function(testSet, controlSet, N){
  dat = c(testSet,controlSet)
  shuffle = sample(dat)
  testSample = shuffle[1:N]
  controlSample = shuffle[(N+1):(2*N)]
  median(testSample) - median(controlSample)
}

set.seed(1)
N=10
B = 1000
meddiff = replicate(B, permSampleMed(smokers, nonsmokers,N))

pval = (sum(abs(meddiff) > abs(obsMed)) + 1) / (length(meddiff) + 1)


