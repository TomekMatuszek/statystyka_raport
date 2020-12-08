library(ggplot2)
library(dplyr)
library(stringr)

dane = mutate(dane, country = str_sub(geo, 1, 2))

ggplot(dane, aes(x = x, y = values)) + geom_boxplot()
ggplot(dane, aes(x = values)) + geom_histogram(binwidth = 0.05)
