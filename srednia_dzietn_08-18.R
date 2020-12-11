library(eurostat)
library(ggplot2)
library(dplyr)
library(stringr)
fer = search_eurostat(pattern = "fertility rate", type = "table")
dane2010 = get_eurostat("tgs00100", type = "code", time_format = "date", filters = list(time = 2010))
dane2012 = get_eurostat("tgs00100", type = "code", time_format = "date", filters = list(time = 2012))
dane2014 = get_eurostat("tgs00100", type = "code", time_format = "date", filters = list(time = 2014))
dane2016 = get_eurostat("tgs00100", type = "code", time_format = "date", filters = list(time = 2016))
dane2018 = get_eurostat("tgs00100", type = "code", time_format = "date", filters = list(time = 2018))

dane2010 = mutate(dane2010, country = str_sub(geo, 1, 2))
dane2012 = mutate(dane2012, country = str_sub(geo, 1, 2))
dane2014 = mutate(dane2014, country = str_sub(geo, 1, 2))
dane2016 = mutate(dane2016, country = str_sub(geo, 1, 2))
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



dane2010 = mutate(dane2010, region = str_replace_all(country, pattern))
dane2012 = mutate(dane2012, region = str_replace_all(country, pattern))
dane2014 = mutate(dane2014, region = str_replace_all(country, pattern))
dane2016 = mutate(dane2016, region = str_replace_all(country, pattern))
dane2018 = mutate(dane2018, region = str_replace_all(country, pattern))

dane2010_grouped = group_by(dane2010, region)
dane2012_grouped = group_by(dane2012, region)
dane2014_grouped = group_by(dane2014, region)
dane2016_grouped = group_by(dane2016, region)
dane2018_grouped = group_by(dane2018, region)

summary2010 =  summarize(dane2010_grouped, srednia = as.numeric(mean(values, na.rm = TRUE)))
summary2012 =  summarize(dane2012_grouped, srednia = as.numeric(mean(values, na.rm = TRUE)))
summary2014 =  summarize(dane2014_grouped, srednia = as.numeric(mean(values, na.rm = TRUE)))
summary2016 =  summarize(dane2016_grouped, srednia = as.numeric(mean(values, na.rm = TRUE)))
summary2018 =  summarize(dane2018_grouped, srednia = as.numeric(mean(values, na.rm = TRUE)))



  
balkany = as.data.frame(matrix(as.numeric(c(summary2010[1, 2],
                 summary2012[1, 2], summary2014[1, 2],
                 summary2016[1, 2], summary2018[1, 2])), ncol=1,byrow=TRUE))

europa_pd = as.data.frame(matrix(as.numeric(c(summary2010[2, 2],
                   summary2012[2, 2], summary2014[2, 2],
                   summary2016[2, 2], summary2018[2, 2])), ncol=1,byrow=TRUE))

europa_pn = as.data.frame(matrix(as.numeric(c(summary2010[3, 2],
                   summary2012[3, 2], summary2014[3, 2],
                   summary2016[3, 2], summary2018[3, 2])), ncol=1,byrow=TRUE))

europa_sr = as.data.frame(matrix(as.numeric(c(summary2010[4, 2],
                   summary2012[4, 2], summary2014[4, 2],
                   summary2016[4, 2], summary2018[4, 2])), ncol=1,byrow=TRUE))

europa_zach = as.data.frame(matrix(as.numeric(c(summary2010[5, 2],
                       summary2012[5, 2], summary2014[5, 2],
                       summary2016[5, 2], summary2018[5, 2])), ncol=1,byrow=TRUE))


europa_pn = mutate(europa_pn, rok = as.character(c(2010, 2012, 2014, 2016, 2018)))
europa_pd = mutate(europa_pd, rok = as.character(c(2010, 2012, 2014, 2016, 2018)))
europa_sr = mutate(europa_sr, rok = as.character(c( 2010, 2012, 2014, 2016, 2018)))
europa_zach = mutate(europa_zach, rok = as.character(c(2010, 2012, 2014, 2016, 2018)))
balkany = mutate(balkany, rok =as.character(c(2010, 2012, 2014, 2016, 2018)))

colnames(balkany) = c("srednia", "rok")
colnames(europa_sr) = c("srednia", "rok")
colnames(europa_pd) = c("srednia", "rok")
colnames(europa_zach) = c("srednia", "rok")
colnames(europa_pn) = c("srednia", "rok")


europa_pn = mutate(europa_pn, region = "Europa Północna")
europa_pd = mutate(europa_pd, region = "Europa Południowa")
europa_sr = mutate(europa_sr, region = "Europa Środkowa")
europa_zach = mutate(europa_zach, region = "Europa Zachodnia")
balkany= mutate(balkany, region = "Balkany")

regiony = rbind(europa_pn, balkany, europa_pd, europa_zach, europa_sr)


ggplot(regiony, aes(rok, srednia, group = region, color = region)) + 
  geom_line(size = 1.8) +
  labs(x = element_blank(), y = "Współczynnik dzietności", col = "Region") +
  theme(plot.background = element_rect(fill = "#252525"), 
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
    legend.key = element_rect(fill = "#777777"),
    axis.text = element_text(size = 12, 
                             color = "#eeeeee"),
    axis.title.x = element_text(vjust = 0))









