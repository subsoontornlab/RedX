install.packages("gapminder")

library(gapminder)
library("dplyr")

data(gapminder)
head(gapminder)

class(gapminder)

#Create a vector 'x' of the life expectancies 
#of each country for the year 1952. Plot a histogram of these 
#life expectancies to see the spread of the different countries.

x = filter(gapminder, year == 1952) %>% select(lifeExp) %>% unlist()
class(x)
par(mar = c(1,1,1,1))
hist(x)


# proportion of countries in 1952 that have life exp
#less than or equal to 40
mean(x<=40)

# What is the proportion of countries in 1952 
#that have a life expectancy between 40 and 60 years? 
#Hint: this is the proportion that have a life expectancy
#less than or equal to 60 years, minus the proportion 
#that have a life expectancy less than or equal to 40 years.
mean(x<=60) - mean(x<=40)

#make cumulative distribution plot
prop = function(q) {
  mean(x <= q)
}

qs = seq(from=min(x), to=max(x), length=20)

props = sapply(qs, prop)

plot(qs, props)

# one line version
props = sapply(qs, function(q) mean(x <= q))

# build-in version
plot(ecdf(x))



