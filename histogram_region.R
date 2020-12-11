library(ggplot2)
library(dplyr)
library(stringr)

#potrzebne dane z pliku pobieranie_danych 

dane = mutate(dane, country = str_sub(geo, 1, 2))
dane = filter(dane, geo != "FRY5", geo != "FRY3", geo != "FRY4", 
              geo != "FRY2", geo != "FRY1", geo != "ES7", geo != "ES70", 
              geo != "PT2", geo != "PT20", geo != "PT3", geo != "PT30")
dane_regiony = mutate(dane, region = str_replace_all(country, c("BG" = "Bałkany",
"EL" = "Europa Południowa",  "HR" = "Bałkany", "SI" = "Bałkany", "RS" = "Bałkany", 
"ME" = "Bałkany", "MK" = "Bałkany", 
"IT" = "Europa Południowa", "AL" = "Bałkany", "MT" = "Europa Południowa",
"DK" = "Europa Północna", "NO" = "Europa Północna", "FI" = "Europa Północna",
"SE" = "Europa Północna", "IS" = "Europa Północna", "IE" = "Europa Zachodnia",
"BE" = "Europa Zachodnia", "UK" = "Europa Zachodnia", "FR" = "Europa Zachodnia",
"LU" = "Europa Zachodnia", "NL" = "Europa Zachodnia", "ES" = "Europa Południowa", 
"CY" = "Europa Południowa", "PT" = "Europa Południowa","CZ" = "Europa Środkowa", 
"DE" = "Europa Środkowa", "PL" = "Europa Środkowa", "AT" = "Europa Środkowa",
"HU" = "Europa Środkowa", "SK" = "Europa Środkowa", "LI" = "Europa Środkowa",
"CH" = "Europa Zachodnia", "SI" = "Europa Środkowa", "EE" = "Europa Północna",
"LT" = "Europa Północna", "LV" = "Europa Północna", 
"RO" = "Bałkany", "TR" = "Europa Południowa")))

#srednia dzietnosci dla poszczegolnych regionow
grupy = group_by(dane_regiony, region)
grupy_summary =  summarize(grupy, srednia = mean(values))

#obciecie wystających wartosci dla lepszej widocznosci histogramow
dane_regiony2 = arrange(dane_regiony, desc(values))
dane_regiony2 = dane_regiony2[-c(1:7), ]


ggplot(dane_regiony2, aes(x=values,
                         fill = region)) +
  scale_fill_manual(values = c("tomato", "coral4", "chartreuse4", "dodgerblue3", "violetred3")) +
  geom_histogram(data=subset(dane_regiony2, region == 'Europa Południowa'), alpha = 0.4, binwidth = 0.05) +
  geom_histogram(data=subset(dane_regiony2, region == 'Europa Północna'), alpha = 0.4, binwidth = 0.05) +
  geom_histogram(data=subset(dane_regiony2, region == 'Europa Zachodnia'), alpha = 0.4, binwidth = 0.05) +
  geom_histogram(data=subset(dane_regiony2, region == 'Europa Środkowa'), alpha = 0.4, binwidth = 0.05) +
  geom_histogram(data=subset(dane_regiony2, region == 'Bałkany'), alpha = 0.4, binwidth = 0.05) + 
  labs(x = "Współczynnik dzietności", 
     y = "Liczba państw",
     fill = 'Region' ) + 
  theme(
    plot.background = element_rect(fill = "gray20"), 
    panel.background = element_rect(fill = "gray25"), 
    axis.title = element_text(size = 15,
                              color = "#eeeeee"), 
    legend.background = element_rect(color = "#222222", 
                                     fill = "gray30"),  
    legend.title = element_text(size = 13, colour = "#eeeeee"),
    legend.text = element_text(size = 12, colour = "#eeeeee"),
    axis.text = element_text(size = 12, 
                             color = "#eeeeee"),
    axis.title.x = element_text(vjust = 0))


