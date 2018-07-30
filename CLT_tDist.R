library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv"
if(!file.exists("femaleMiceWeights.csv")) download(url,destfile=filename)
dat <- read.csv(filename)

library("dplyr")
library("rafalib")

#### http://genomicsclass.github.io/book/

#Suppose we are interested in the proportion of times
#we see a 6 when rolling n=100 die. 
n=100
x=sample(1:6, n, replace=TRUE)

mean(x==6)
p = 1/6
varx = p*(1-p)/n

z = (mean(x==6) - p) / sqrt(varx)

### roll n = 100 dice 10,000 times
n = 100
set.seed(1)
# replicate
xmean = replicate(10000, mean(sample(1:6, n, replace=TRUE)==6))
p = 1/6 # mean
varx = p*(1-p)/n #variance

zarray = (xmean - p) / sqrt(p*(1-p)/n)

# fraction of > 2 std
mean(abs(zarray)>2)


# repeat p = 0.5 n =5
n = 5
set.seed(1)
# replicate
xmean = replicate(10000, mean(sample(5:6, n, replace=TRUE)==6))
p = 1/2 # mean
varx = p*(1-p)/n #variance

zarray1 = (xmean - p) / sqrt(p*(1-p)/n)



# repeat p = 0.5 n =30
n = 30
set.seed(1)
# replicate
xmean = replicate(10000, mean(sample(5:6, n, replace=TRUE)==6))
p = 1/2 # mean
varx = p*(1-p)/n #variance

zarray2 = (xmean - p) / sqrt(p*(1-p)/n)


# repeat p = 0.01 n =30
n = 30
set.seed(1)
# replicate
xmean = replicate(10000, mean(sample(1:100, n, replace=TRUE)==6))
p = 1/100 # mean
varx = p*(1-p)/n #variance

zarray3 = (xmean - p) / sqrt(p*(1-p)/n)


# repeat p = 0.01 n =100
n = 100
set.seed(1)
# replicate
xmean = replicate(10000, mean(sample(1:100, n, replace=TRUE)==6))
p = 1/100 # mean
varx = p*(1-p)/n #variance

zarray4 = (xmean - p) / sqrt(p*(1-p)/n)

qqnorm(zarray4)

##########

X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
set.seed(1)
sd(sample(X, 12))
mean(sample(X, 12))
mean(X)

popsd(X)

###  prob of more than 2 g from muX

2*(1-pnorm(0))
2*(1-pnorm(sqrt(12)*2/sd(X)))

### SE of Xmean - Ymean
sqrt(sd(X)^2/12 + sd(Y)^2/12)

t.test(X,Y)
mean(X)
mean(Y)

## t distribution
# effect of df on tail
1 - pt(3,df=3)
1 - pt(3,df=15)
1 - pt(3,df=30)
1 - pnorm(3)


z = ( mean(Y) - mean(X) ) / sqrt( var(X)/12 + var(Y)/12)
1-pnorm(z)
t.test(X,Y)
