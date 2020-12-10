library(ggplot2)
library(dplyr)
library(stringr)

dane = mutate(dane, country = str_sub(geo, 1, 2))
dane = filter(dane, geo != "FRY5", geo != "FRY3", geo != "FRY4", 
                  geo != "FRY2", geo != "FRY1", geo != "ES7", geo != "ES70", 
                  geo != "PT2", geo != "PT20", geo != "PT3", geo != "PT30")

ggplot(dane_regiony, aes(x = region, y = values, col = region)) +
  coord_flip() + scale_color_manual(values = c("orange", "black", "darkgreen", "blue", "red")) +
  #geom_boxplot(col = "black") +
  labs(y = "Wspolczynnik dzietnosci", x = "", col = "Region") +
  geom_hline(yintercept = mean(dane$values), color = "gray30", size = 1) +
  geom_jitter(size = 2, alpha = 0.2, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 5)
  
  
ggplot(dane, aes(x = values)) + geom_histogram(binwidth = 0.05)
