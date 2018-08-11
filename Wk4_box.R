head(InsectSprays)

#two way of making a boxplot
boxplot(split(InsectSprays$count, InsectSprays$spray))
boxplot(count~spray, data = InsectSprays)


####

library(dplyr)
install.packages("UsingR", dependencies=TRUE)
library(UsingR)
data(nym.2002, package="UsingR")
head(nym.2002)
boxplot()