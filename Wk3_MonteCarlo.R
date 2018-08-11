library(downloader)
library("dplyr")
library(rafalib)

#t-stat from normal distribution
set.seed(1)
n5 = rnorm(5,0,1)
t = sqrt(5)*mean(n5)/sd(n5)

# fraction of t-stat larger than 2
tgen = function(N){
  nN = rnorm(N, 0, 1)
  t = sqrt(N)*mean(nN)/sd(nN)
}
B = 1000 # number of repeate
pB = replicate(B, tgen(5))
mean(pB>2) # proportion > 2

# compare monte carlo t-distribution
B = 100
N = 50
ps = seq(1/(B+1), 1-1/(B+1),len=B)  # fraction above -Inf
qpsB = qt(ps, df=4) # ideal t-distribution

set.seed(1)
pB = replicate(B, tgen(N)) # monte carlo t-distribution
qqplot(qpsB, pB)
abline(0,1)

#Use Monte Carlo simulation to corroborate that 
#the t-statistic comparing two means and obtained 
#with normally distributed (mean 0 and sd) data 
#follows a t-distribution. 
B = 100
N = 50
set.seed(1)
tMonte = function(N){
  s1 = rnorm(N,0,1)
  s2 = rnorm(N,0,1) 
  t12 = (mean(s1)-mean(s2))/sqrt(sd(s1)^2/N + sd(s2)^2/N)
}

tmonteN = replicate(B, tMonte(N))
qqplot(qpsB, tmonteN)
abline(0,1)

## ans
Ns<-seq(5,30,5)
B <- 1000
mypar(3,2)
LIM <- c(-4.5,4.5)
for(N in Ns){
  ts <- replicate(B,{
    x <- rnorm(N)
    y <- rnorm(N)
    t.test(x,y, var.equal = TRUE)$stat
  })
  ps <- seq(1/(B+1),1-1/(B+1),len=B)
  qqplot(qt(ps,df=2*N-2),ts,main=N,
         xlab="Theoretical",ylab="Observed",
         xlim=LIM, ylim=LIM)
  abline(0,1)
}  

#----
## Is the following statement true or false? If instead 
#of generating the sample with X=rnorm(15) we generate 
#it with binary data (either positive or negative 1 with
#probability 0.5) X =sample(c(-1,1), 15, replace=TRUE) 
#then the t-statistic
set.seed(1)
N = 15
B = 10000
tstats = replicate(B, {
  X = sample(c(-1,1), N, replace = TRUE)
  sqrt(N)*mean(X)/sd(X)
})
ps = seq(1/(B+1), 1-1/(B+1), len = B)
qqplot(qt(ps, N-1), tstats, xlim = range(tstats))
abline(0,1)

#---
# redo abve with N = 1000
set.seed(1)
N = 1000
B = 10000
tstats = replicate(B, {
  X = sample(c(-1,1), N, replace = TRUE)
  sqrt(N)*mean(X)/sd(X)
})
ps = seq(1/(B+1), 1-1/(B+1), len = B)
qqplot(qt(ps, N-1), tstats, xlim = range(tstats))
abline(0,1)

# distribution of median
set.seed(1)
N = 50
B = 10000
Xstats = replicate(B, {
  X = rnorm(N, 0, 1)
  median(X)
})
ps = seq(1/(B+1), 1-1/(B+1), len = B)
qqplot(qt(ps, N-1), Xstats, xlim = range(tstats))


qqnorm(Xstats)
qqline(Xstats)

# -- ans
set.seed(1)
Ns <- seq(5,45,5)
library(rafalib)
mypar(3,3)
for(N in Ns){
  medians <- replicate(10000, median ( rnorm(N) ) )
  title <- paste("N=",N,", avg=",round( mean(medians), 2) , ", sd*sqrt(N)=", round( sd(medians)*sqrt(N),2) )
  qqnorm(medians, main = title )
  qqline(medians)
}
##there is an asymptotic result that says SD is sqrt(N*4*dnorm(0)^2)


###

