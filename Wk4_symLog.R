library(dplyr)
library(UsingR)
data(nym.2002, package="UsingR")

# vector time of the sorted time
time = sort(nym.2002$time)
time
# fastest time / median time
time[1]/median(time)
# slowest time / median time
time[length(time)]/median(time)

#plot of ratio
plot(time/median(time), ylim=c(1/4,4))
abline(h=c(1/2,1,2))

#plot of log2 ratio
plot(log2(time/median(time)),ylim=c(-2,2))
abline(h=-1:1)