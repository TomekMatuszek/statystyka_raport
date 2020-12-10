library(ggplot2)
library(dplyr)
library(stringr)

dane = mutate(dane, country = str_sub(geo, 1, 2))
dane = filter(dane, geo != "FRY5", geo != "FRY3", geo != "FRY4", 
                  geo != "FRY2", geo != "FRY1", geo != "ES7", geo != "ES70", 
                  geo != "PT2", geo != "PT20", geo != "PT3", geo != "PT30")

ggplot(dane, aes(x = country, y = values, col = country)) +
  coord_flip() +
  geom_hline(yintercept = mean(dane$values), color = "red",
             size = 1) +
  #geom_jitter(size = 2, alpha = 0.25, width = 0.1) +
  geom_boxplot(col = "black")
  
ggplot(dane, aes(x = values)) + geom_histogram(binwidth = 0.05)
