#install.packages("kableExtra")
library(eurostat)
library(kableExtra)
library(dplyr)
library(stringr)

fer = search_eurostat(pattern = "fertility rate", type = "table")

dane2018 = get_eurostat("tgs00100", type = "code", time_format = "date", filters = list(time = 2018))

dane2018 = mutate(dane2018, country = str_sub(geo, 1, 2))

pattern = c("BG" = "Bałkany",
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
            "RO" = "Bałkany", "TR" = "Europa Południowa")
dane2018 = mutate(dane2018, region = str_replace_all(country, pattern))

#potrzebne dane z srednia_dziet_08-18(dane2018)

#statystyki opisowe dla poszczególnych regionów
grupy = group_by(dane2018, region)
statystyka_dziet <- summarise(grupy, 
                                       średnia = mean(values), 
                                       mediana = median(values),
                                       min = min(values), 
                                       max = max(values),
                                       IQR = IQR(values))
                                       zakres = max(values)-min(values)
colnames(statystyka_dziet) <- c("Region", "Średnia",
                                         "Mediana", "Minimum",
                                         "Maksimum", "IQR", "Zakres")
View(statystyka_dziet)


tabela = kbl(statystyka_dziet, align = 'c', digits = 2, caption = '<span style = "color: #222222; font-size:130%">Statystyki opisowe dla dzietnosci 
             w 2018 roku w podziale na regiony</SPAN>')
kable_material_dark(tabela, html_font = "Arial", full_width = T)




