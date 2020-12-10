library(eurostat)
library(stringr)
library(dplyr)
library(ggplot2)

fr = get_eurostat("demo_r_frate2", type = "code", time_format = "date", 
                  filters = list(time = c(1990, 1995, 2000, 2005, 2010, 2015, 2018), 
                                 geo = 'FR', age = 'TOTAL'))

fr = select(fr, Data = `time`, Wartość = `values`)
fr$Data = str_replace(fr$Data, "-01-01", "")

p = ggplot(fr, aes(x = Data, y = Wartość)) + geom_bar(stat='identity')
p = p +  
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  coord_cartesian(ylim = c(1.5, 2.1)) +
  scale_y_continuous(breaks=seq(1.6,2.1,0.2)) +
  
  labs(title = "Współczynnik dzietności we Francji", subtitle = 'Lata 1990-2018')
p


