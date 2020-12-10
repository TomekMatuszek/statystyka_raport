library(eurostat)
library(ggplot2)
library(dplyr)
library(stringr)
fer = search_eurostat(pattern = "fertility rate", type = "table")
dane2008 = get_eurostat("tgs00100", type = "code", time_format = "date", filters = list(time = 2008))
dane2010 = get_eurostat("tgs00100", type = "code", time_format = "date", filters = list(time = 2010))
dane2012 = get_eurostat("tgs00100", type = "code", time_format = "date", filters = list(time = 2012))
dane2014 = get_eurostat("tgs00100", type = "code", time_format = "date", filters = list(time = 2014))
dane2016 = get_eurostat("tgs00100", type = "code", time_format = "date", filters = list(time = 2016))
dane2018 = get_eurostat("tgs00100", type = "code", time_format = "date", filters = list(time = 2018))


dane2008 = mutate(dane2008, country = str_sub(geo, 1, 2))
dane2010 = mutate(dane2010, country = str_sub(geo, 1, 2))
dane2012 = mutate(dane2012, country = str_sub(geo, 1, 2))
dane2014 = mutate(dane2014, country = str_sub(geo, 1, 2))
dane2016 = mutate(dane2016, country = str_sub(geo, 1, 2))
dane2018 = mutate(dane2018, country = str_sub(geo, 1, 2))

pattern = c("BG" = "Bałkany",
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
            "MD" = "Europa Wschodnia", "TR" = "Europa Wschodnia")


dane2008 = mutate(dane2008, region = str_replace_all(country, pattern))
dane2010 = mutate(dane2010, region = str_replace_all(country, pattern))
dane2012 = mutate(dane2012, region = str_replace_all(country, pattern))
dane2014 = mutate(dane2014, region = str_replace_all(country, pattern))
dane2016 = mutate(dane2016, region = str_replace_all(country, pattern))
dane2018 = mutate(dane2018, region = str_replace_all(country, pattern))

dane2008_grouped = group_by(dane2008, region)
dane2010_grouped = group_by(dane2010, region)
dane2012_grouped = group_by(dane2012, region)
dane2014_grouped = group_by(dane2014, region)
dane2016_grouped = group_by(dane2016, region)
dane2018_grouped = group_by(dane2018, region)

summary2008 =  summarize(dane2008_grouped, srednia = as.numeric(mean(values, na.rm = TRUE)))
summary2010 =  summarize(dane2010_grouped, srednia = as.numeric(mean(values, na.rm = TRUE)))
summary2012 =  summarize(dane2012_grouped, srednia = as.numeric(mean(values, na.rm = TRUE)))
summary2014 =  summarize(dane2014_grouped, srednia = as.numeric(mean(values, na.rm = TRUE)))
summary2016 =  summarize(dane2016_grouped, srednia = as.numeric(mean(values, na.rm = TRUE)))
summary2018 =  summarize(dane2018_grouped, srednia = as.numeric(mean(values, na.rm = TRUE)))
View(summary2008)


  
balkany = as.data.frame(matrix(as.numeric(c(summary2008[1, 2], summary2010[1, 2],
                 summary2012[1, 2], summary2014[1, 2],
                 summary2016[1, 2], summary2018[1, 2])), ncol=1,byrow=TRUE))

europa_sr = as.data.frame(matrix(as.numeric(c(summary2008[2, 2], summary2010[2, 2],
                   summary2012[2, 2], summary2014[2, 2],
                   summary2016[2, 2], summary2018[2, 2])), ncol=1,byrow=TRUE))

europa_wsch = as.data.frame(matrix(as.numeric(c(summary2008[3, 2], summary2010[3, 2],
                   summary2012[3, 2], summary2014[3, 2],
                   summary2016[3, 2], summary2018[3, 2])), ncol=1,byrow=TRUE))

europa_zach = as.data.frame(matrix(as.numeric(c(summary2008[4, 2], summary2010[4, 2],
                   summary2012[4, 2], summary2014[4, 2],
                   summary2016[4, 2], summary2018[4, 2])), ncol=1,byrow=TRUE))

skandynawia = as.data.frame(matrix(as.numeric(c(summary2008[5, 2], summary2010[5, 2],
                       summary2012[5, 2], summary2014[5, 2],
                       summary2016[5, 2], summary2018[5, 2])), ncol=1,byrow=TRUE))


View(skandynawia)

skandynawia = mutate(skandynawia, rok = as.character(c(2008, 2010, 2012, 2014, 2016, 2018)))
europa_wsch = mutate(europa_wsch, rok = as.character(c(2008, 2010, 2012, 2014, 2016, 2018)))
europa_sr = mutate(europa_sr, rok = as.character(c(2008, 2010, 2012, 2014, 2016, 2018)))
europa_zach = mutate(europa_zach, rok = as.character(c(2008, 2010, 2012, 2014, 2016, 2018)))
balkany = mutate(balkany, rok =as.character(c(2008, 2010, 2012, 2014, 2016, 2018)))

colnames(balkany) = c("srednia", "rok")
colnames(europa_sr) = c("srednia", "rok")
colnames(europa_wsch) = c("srednia", "rok")
colnames(europa_zach) = c("srednia", "rok")
colnames(skandynawia) = c("srednia", "rok")

skandynawia = skandynawia[as.integer(2, 1)]


skandynawia = mutate(skandynawia, region = "Skandynawia")
europa_wsch = mutate(europa_wsch, region = "Europa Wschodnia")
europa_sr = mutate(europa_sr, region = "Europa Środkowa")
europa_zach = mutate(europa_zach, region = "Europa Zachodnia")
balkany= mutate(balkany, region = "Bakany")

regiony = rbind(skandynawia, balkany, europa_wsch, europa_zach, europa_sr)

View(regiony)

ggplot(regiony, aes(rok, srednia, group = region, color = region)) + 
  geom_line(size = 1.8) + 
  theme(
    plot.background = element_rect(fill = "#252525"), 
    panel.background = element_rect(fill = "#555555"), 
    axis.title = element_text(size = 15,
                              color = "#eeeeee"), 
    plot.title = element_text(size = 18,
                              color = "#eeeeee",
                              vjust = 2,
                              hjust = 0.5), 
    legend.background = element_rect(color = "#222222", 
                                     fill = "#777777"),  
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 12, 
                             color = "#eeeeee"),
    axis.title.x = element_text(vjust = 0))









