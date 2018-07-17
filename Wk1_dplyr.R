library(downloader)
library("dplyr")
url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- basename(url)
download(url,filename)

install.packages("dplyr")

sleepDat = read.csv("msleep_ggplot2.csv")

class(sleepDat)

primateSleep = filter(sleepDat, order == "Primates")
nrow(primateSleep)
class(primateSleep)

primateSleep = filter(sleepDat, order == "Primates") %>% select(sleep_total)
class(primateSleep)

mean(unlist(primateSleep))

primateSleep2 = filter(sleepDat, order == "Primates")
summarise(primateSleep2, avg_sleep = mean(sleep_total))