library(ggplot2)
library(dplyr)
library(stringr)

dane = mutate(dane, country = str_sub(geo, 1, 2))
dane = filter(dane, geo != "FRY5", geo != "FRY3", geo != "FRY4", 
                  geo != "FRY2", geo != "FRY1", geo != "ES7", geo != "ES70", 
                  geo != "PT2", geo != "PT20", geo != "PT3", geo != "PT30")

ggplot(dane_regiony, aes(x = region, y = values, col = region)) +
  coord_flip() + scale_color_manual(values = c("tomato", "black", "chartreuse4", "dodgerblue3", "violetred3")) +
  #geom_boxplot(col = "black") +
  labs(y = "Wspolczynnik dzietnosci", x = element_blank(), col = "Region") +
  theme(legend.position = "none", 
        plot.background = element_rect(fill = "#252525"), 
        panel.background = element_rect(fill = "#252525"),
        axis.text = element_text(size = 12, color = "#eeeeee"),
        axis.title.x = element_text(color = "#eeeeee")) +
  geom_hline(yintercept = mean(dane$values), color = "darkred", size = 1.5) +
  geom_jitter(size = 2, alpha = 0.5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 5)
  
  
ggplot(dane, aes(x = values)) + geom_histogram(binwidth = 0.05)
