library(downloader)
library("dplyr")
library(rafalib)

d = read.csv("assoctest.csv")

# summarise data in a table
td = table(d)

#chisquare test
chisq.test(td)

#fischer exact test (better than chi sq esp
# for small sample. but harder to calculate)
fisher.test(td)