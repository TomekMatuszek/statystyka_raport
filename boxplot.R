library(ggplot2)
library(dplyr)
library(stringr)

#dane2 = mutate(dane, country = geo)
x = c()
for (i in seq(1:nrow(dane))) {
  kod = as.character(dane$geo[i])
  x = c(x, str_sub(kod, 1, 2))
}
dane = cbind(dane, as.data.frame(x))

ggplot(dane, aes(x = x, y = values)) + geom_boxplot()
