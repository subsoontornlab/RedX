# load and prepare data
library(dplyr)
data(ChickWeight)
head(ChickWeight)
plot( ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)

chick = reshape(ChickWeight, idvar=c("Chick","Diet"), timevar="Time",
                direction="wide")
head(chick)

chick = na.omit(chick)


#---------------------------
# check day 4 diet 1
x = filter(chick, Diet == 1)$weight.4
# check day 4 diet 4
y = filter(chick, Diet == 4)$weight.4
# t.test
t.test(x,y)$p.value
wilcox.test(x,y)

#add outlier wt 3000 g
xout = c(x, 200)
t.test(xout,y)$p.value

wilcox.test(xout,y)$p.value

# Mann-Whitney-Wilcoxon limitation
library(rafalib)
mypar(1,3)
boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)

t.test(x,y+10)$statistic - t.test(x,y+100)$statistic

# ... when separation between sample is large, p value of
# Wilcoxon test will be small even if sample size is small
# sample separation large enough, no longer made difference
a = c(1,2,3)
b = c(4,5,6)
d = c(400,500,600)
wilcox.test(a,b)$p.value

wilcox.test(a,d)$p.value