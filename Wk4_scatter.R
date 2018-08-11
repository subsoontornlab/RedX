library(dplyr)
library(UsingR)
data(nym.2002, package="UsingR")

# get male and female data
mdata = filter(nym.2002, gender == 'Male')
fdata = filter(nym.2002, gender == 'Female')

# correlation between age and time to finish
cor(mdata$age, mdata$time)
cor(fdata$age, fdata$time)

# boxplot of time stratified by age group (20-25, 25-30, )
boxplot(split(nym.2002$time, floor(nym.2002$age/5)*5 ))
# ans
males = mdata
females = fdata
library(rafalib)
mypar(2,2)
plot(females$age, females$time)
plot(males$age, males$time)
group <- floor(females$age/5) * 5
boxplot(females$time~group)
group <- floor(males$age/5) * 5
boxplot(males$time~group)