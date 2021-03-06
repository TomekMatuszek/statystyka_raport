#install.packages("tmap")
library(tmap)
library(eurostat)
geodata = get_eurostat_geospatial(output_class = "sf",
                                   resolution = "60",
                                   nuts_level = 2,
                                   year = 2016)
map_data = inner_join(geodata, dane)
map_data = filter(map_data, geo != "FRY5", geo != "FRY3", geo != "FRY4", 
                  geo != "FRY2", geo != "FRY1", geo != "ES7", geo != "ES70", 
                  geo != "PT2", geo != "PT20", geo != "PT3", geo != "PT30")

map2 = tm_shape(map_data, is.master = TRUE) +
  tm_add_legend(title = "Współczynnik\ndzietności", 
                col = c("#ffface", "#fee697", "#febe4a", "#f88b22", "#db5d0a", "#a33803"),
                labels = c("1 - 1.2", "1.2 - 1.4", "1.4 - 1.6",
                           "1.6 - 1.8", "1.8 - 2.0", "> 2.0")) +
  tm_polygons(col = "values", n = 7,
              border.col = "gray20",
              legend.show = FALSE,
              breaks = c(1, 1.2, 1.4, 1.6, 1.8, 2, Inf),
              legend.hist = TRUE) +
  tm_layout(frame.lwd = 1, bg.color = "gray25", attr.color = "black",
            legend.position = c(0.79, 0.25),
            #legend.position = c(0.01, 0.01),
            legend.frame = TRUE,
            legend.text.size = 0.75,
            legend.height = 1.05,
            legend.title.size = 0.9,
            legend.hist.width = 0.2,
            legend.hist.height = 0.2,
            legend.hist.bg.color = "gray40",
            legend.bg.color = "gray40") +
  tm_compass(type = "rose", position = c("left", "bottom"),
             color.light = "gray40", size = 4) 
  
map2
    