library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

mean(x)

set.seed(1)

abs(mean(sample(x, 5)) - mean(x))

set.seed(5)

abs(mean(sample(x, 5)) - mean(x))
