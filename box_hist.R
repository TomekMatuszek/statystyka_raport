library(ggplot2)
library(dplyr)
library(stringr)

dane = mutate(dane, country = str_sub(geo, 1, 2))
dane = filter(dane, geo != "FRY5", geo != "FRY3", geo != "FRY4", 
                  geo != "FRY2", geo != "FRY1", geo != "ES7", geo != "ES70", 
                  geo != "PT2", geo != "PT20", geo != "PT3", geo != "PT30")
dane_regiony = mutate(dane, region = str_replace_all(country, c("BG" = "Bałkany",
                                                                "EL" = "Bałkany",  "HR" = "Bałkany", "SI" = "Bałkany", "RS" = "Bałkany", 
                                                                "ME" = "Bałkany", "MK" = "Bałkany", "BA" = "Bałkany", "XK" = "Bałkany", 
                                                                "IT" = "Bałkany", "AL" = "Bałkany", "MT" = "Bałkany",
                                                                "DK" = "Skandynawia", "NO" = "Skandynawia", "FI" = "Skandynawia",
                                                                "SE" = "Skandynawia", "IS" = "Skandynawia", "IE" = "Europa Zachodnia",
                                                                "BE" = "Europa Zachodnia", "UK" = "Europa Zachodnia", "FR" = "Europa Zachodnia",
                                                                "LU" = "Europa Zachodnia", "NL" = "Europa Zachodnia", "ES" = "Europa Zachodnia", 
                                                                "CY" = "Europa Zachodnia", "PT" = "Europa Zachodnia","CZ" = "Europa Środkowa", 
                                                                "DE" = "Europa Środkowa", "PL" = "Europa Środkowa", "AT" = "Europa Środkowa",
                                                                "HU" = "Europa Środkowa", "SK" = "Europa Środkowa", "LI" = "Europa Środkowa",
                                                                "CH" = "Europa Środkowa", "SI" = "Europa Środkowa", "EE" = "Europa Wschodnia",
                                                                "LT" = "Europa Wschodnia", "BG" = "Europa Wschodnia", "LV" = "Europa Wschodnia", 
                                                                "RO" = "Europa Wschodnia", "BY" = "Europa Wschodnia", "UA" = "Europa Wschodnia", 
                                                                "MD" = "Europa Wschodnia", "TR" = "Europa Wschodnia")))

ggplot(dane_regiony, aes(x = region, y = values, col = region)) +
  coord_flip() + scale_color_manual(values = c("tomato", "coral4", "chartreuse4", "dodgerblue3", "violetred3")) +
  #geom_boxplot(col = "black") +
  labs(y = "Wspolczynnik dzietnosci", x = element_blank(), col = "Region") +
  theme(legend.position = "none", 
        plot.background = element_rect(fill = "gray25"), 
        panel.background = element_rect(fill = "gray20"),
        axis.text = element_text(size = 12, color = "#eeeeee"),
        axis.title.x = element_text(color = "#eeeeee")) +
  geom_hline(yintercept = mean(dane$values), color = "darkred", size = 1.5) +
  geom_jitter(size = 2, alpha = 0.5, width = 0.3) +
  stat_summary(fun = mean, geom = "point", size = 8)
  
  
ggplot(dane_regiony, aes(x = values)) + geom_histogram(binwidth = 0.05)
