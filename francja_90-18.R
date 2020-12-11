library(eurostat)
library(stringr)
library(dplyr)
library(ggplot2)

fr = get_eurostat("demo_r_frate2", type = "code", time_format = "date", 
                  filters = list(time = c(1990, 1995, 2000, 2005, 2010, 2015, 2018), 
                                 geo = 'FR', age = 'TOTAL'))

fr = select(fr, Data = `time`, Wartość = `values`)
fr$Data = str_replace(fr$Data, "-01-01", "")


ggplot(fr, aes(Data, Wartość, group = 1)) + geom_line(color = 'dodgerblue3', size = 1.5) +
  ylim(1.5, 2.3) + labs(y = "Współczynnik dzietności") +
  theme(plot.background = element_rect(fill = "gray25"), 
        panel.background = element_rect(fill = "gray20"),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 15, color = "#eeeeee"), 
        axis.text = element_text(size = 12,  color = "#eeeeee"))

                            
                            
                            
                            