library(ggplot2)
library(dplyr)
library(stringr)

#potrzebne dane z pliku pobieranie_danych 

dane = mutate(dane, country = str_sub(geo, 1, 2))

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
View(dane_regiony)


#srednia dzietnosci dla poszczegolnych regionow
grupy = group_by(dane_regiony, region)
grupy_summary =  summarize(grupy, srednia = mean(values))
View(grupy_summary)

#gowno na szybko
ggplot(data = grupy_summary, aes(x = srednia, y = region)) + 
  geom_col()

#srednie dzietnosci dla poszczegolnych regionow 
#sr_balkany = grupy_summary[1, ]
#sr_europa_srodkowa = grupy_summary[2, ]
#sr_europa_wschodnia = grupy_summary[3, ]
#sr_europa_zachodnia = grupy_summary[4, ]
#sr_skandynawia = grupy_summary[5, ]

#obciecie wystających wartosci dla lepszej widocznosci histogramow
dane_regiony = arrange(dane_regiony, desc(values))
dane_regiony = dane_regiony[-c(1:7), ]


ggplot(dane_regiony, aes(x=values,
                         fill = region
                         )) + 
geom_histogram(data=subset(dane_regiony, region == 'Europa Wschodnia'), alpha = 0.4, binwidth = 0.05) +
geom_histogram(data=subset(dane_regiony, region == 'Skandynawia'), alpha = 0.4, binwidth = 0.05) +
geom_histogram(data=subset(dane_regiony, region == 'Europa Zachodnia'), alpha = 0.4, binwidth = 0.05) +
geom_histogram(data=subset(dane_regiony, region == 'Europa Środkowa'), alpha = 0.4, binwidth = 0.05) +
geom_histogram(data=subset(dane_regiony, region == 'Bałkany'), alpha = 0.4, binwidth = 0.05) + 
labs(x = "Średnia dzietność", 
     y = "Liczba państw",
     fill = 'Region' ) + 
theme(
    plot.background = element_rect(fill = "#252525"), 
    panel.background = element_rect(fill = "#555555"), 
    axis.title = element_text(size = 15,
                              color = "#eeeeee"), 
    legend.background = element_rect(color = "#222222", 
                                     fill = "#777777"),  
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 12, 
                             color = "#eeeeee"),
    axis.title.x = element_text(vjust = 0))


