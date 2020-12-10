install.packages("tmap")
library(tmap)
geodata = get_eurostat_geospatial(output_class = "sf",
                                   resolution = "60",
                                   nuts_level = 2,
                                   year = 2016)
map_data = inner_join(geodata, dane)
map_data = filter(map_data, geo != "FRY5", geo != "FRY3", geo != "FRY4", 
                  geo != "FRY2", geo != "FRY1", geo != "ES7", geo != "ES70", 
                  geo != "PT2", geo != "PT20", geo != "PT3", geo != "PT30")

map2 = tm_shape(map_data, is.master = TRUE) +
  tm_polygons("values", n = 3, title = "",
              palette = "YlOrRd", 
              border.col = "black",
              breaks = c(1, 1.2, 1.4, 1.6, 1.8, 2)) +
  tm_layout(frame.lwd = 2, bg.color = "gray90", attr.color = "black",
            legend.position = c(0.82, 0.4),
            legend.text.size = 1.2)
map2
?tm_layout
?tm_polygons
