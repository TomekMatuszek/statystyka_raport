library(ggplot2)
library(dplyr)
library(stringr)

dane = mutate(dane, country = str_sub(geo, 1, 2))

ggplot(map_data, aes(x = country, y = values, col = country)) +
  coord_flip() +
  geom_hline(yintercept = mean(map_data$values), color = "red",
             size = 1) +
  #geom_jitter(size = 2, alpha = 0.25, width = 0.1) +
  geom_boxplot(color = "black")
  
ggplot(dane, aes(x = values)) + geom_histogram(binwidth = 0.05)
