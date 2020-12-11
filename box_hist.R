library(ggplot2)
library(dplyr)
library(stringr)
library(tmap)

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

ggplot(dane_regiony, aes(x = region, y = values, col = region)) +
  coord_flip() + scale_color_manual(values = c("tomato", "coral4", "chartreuse4", "dodgerblue3", "violetred3")) +
  #geom_boxplot(col = "black") +
  labs(y = "Wspolczynnik dzietnosci", x = element_blank(), col = "Region") +
  theme(legend.position = "none", 
        plot.background = element_rect(fill = "gray20"), 
        panel.background = element_rect(fill = "gray25"),
        axis.text = element_text(size = 12, color = "#eeeeee"),
        axis.title.x = element_text(color = "#eeeeee")) +
  geom_hline(yintercept = median(dane$values), color = "darkred", size = 1.5) +
  geom_jitter(size = 2, alpha = 0.6, width = 0.4) +
  stat_summary(fun = median, geom = "point", size = 8)

geodata = get_eurostat_geospatial(output_class = "sf",
                                  resolution = "60",
                                  nuts_level = 2,
                                  year = 2016)
map_data_l = inner_join(geodata, dane_regiony)
map_data_l = filter(map_data_l, geo != "FRY5", geo != "FRY3", geo != "FRY4", 
                  geo != "FRY2", geo != "FRY1", geo != "ES7", geo != "ES70", 
                  geo != "PT2", geo != "PT20", geo != "PT3", geo != "PT30")

map_legend = tm_shape(map_data_l, is.master = TRUE) +
  tm_polygons(col = "region", n = 5, title = "",
              border.col = "gray20") +
  tm_layout(frame.lwd = 1, bg.color = "gray20",
            legend.position = c(0.75, 0.38),
            legend.text.size = 1,
            legend.title.size = 1.15,
            legend.text.color = "#eeeeee",
            legend.title.color = "#eeeeee")
map_legend

ggplot(dane_regiony, aes(x = values)) + 
  geom_histogram(binwidth = 0.05, color = "gray50", fill = "gray50") +
  labs(x = "Wspolczynnik dzietnosci", y = element_blank()) +
  theme(plot.background = element_rect(fill = "gray25"),
        panel.background = element_rect(fill = "gray20"),
        axis.text = element_text(size = 12, color = "#eeeeee"),
        axis.title.x = element_text(color = "#eeeeee"))
