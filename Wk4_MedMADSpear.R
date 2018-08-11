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
# chick weight on day 4 w/o or w/ 3000g chick outlier. 
day4wt = chick$weight.4
day4wtOut = c(chick$weight.4, 3000)
# chick weight on day 21 w/o or w/ 3000g chick outlier. 
day21wt = chick$weight.21
day21wtOut = c(chick$weight.21, 3000)

# avg change
mean(day4wtOut)/mean(day4wt)

# median change
median(day4wtOut)/median(day4wt)

# sd change
sd(day4wtOut)/sd(day4wt)

# mad change
mad(day4wtOut)/mad(day4wt)

# scatter plot showing day 4 and day 21 wt
plot(day4wt , day21wt, main="wt corr", xlab= "wt day 4", ylab = "wt day 21" )

# scatter plot showing day 4 and day 21 wt + outlier
plot(day4wtOut , day21wtOut, main="wt corr", xlab= "wt day 4", ylab = "wt day 21" )

# pearson correlation change
cor(day4wtOut,day21wtOut,method="pearson") / cor(day4wt,day21wt,method="pearson")
