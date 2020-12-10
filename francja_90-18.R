library(eurostat)
library(stringr)
library(dplyr)
library(ggplot2)

fr1990 = get_eurostat("demo_r_frate2", type = "code", time_format = "date", filters = list(time = 1990, geo = 'FR', age = 'TOTAL'))
fr1995 = get_eurostat("demo_r_frate2", type = "code", time_format = "date", filters = list(time = 1995, geo = 'FR', age = 'TOTAL'))
fr2000 = get_eurostat("demo_r_frate2", type = "code", time_format = "date", filters = list(time = 2000, geo = 'FR', age = 'TOTAL'))
fr2005 = get_eurostat("demo_r_frate2", type = "code", time_format = "date", filters = list(time = 2005, geo = 'FR', age = 'TOTAL'))
fr2010 = get_eurostat("demo_r_frate2", type = "code", time_format = "date", filters = list(time = 2010, geo = 'FR', age = 'TOTAL'))
fr2015 = get_eurostat("demo_r_frate2", type = "code", time_format = "date", filters = list(time = 2015, geo = 'FR', age = 'TOTAL'))
fr2018 = get_eurostat("demo_r_frate2", type = "code", time_format = "date", filters = list(time = 2018, geo = 'FR', age = 'TOTAL'))

fr1990 = select(fr1990, Data = `time`, Wartość = `values`)
fr1995 = select(fr1995, Data = `time`, Wartość = `values`)
fr2000 = select(fr2000, Data = `time`, Wartość = `values`)
fr2005 = select(fr2005, Data = `time`, Wartość = `values`)
fr2010 = select(fr2010, Data = `time`, Wartość = `values`)
fr2015 = select(fr2015, Data = `time`, Wartość = `values`)
fr2018 = select(fr2018, Data = `time`, Wartość = `values`)

fr1990$Data = str_replace(fr1990$Data, "-01-01", "")
fr1995$Data = str_replace(fr1995$Data, "-01-01", "")
fr2000$Data = str_replace(fr2000$Data, "-01-01", "")
fr2005$Data = str_replace(fr2005$Data, "-01-01", "")
fr2010$Data = str_replace(fr2010$Data, "-01-01", "")
fr2015$Data = str_replace(fr2015$Data, "-01-01", "")
fr2018$Data = str_replace(fr2018$Data, "-01-01", "")

fr_final = bind_rows(fr1990, fr1995, fr2000, fr2005, fr2010, fr2015, fr2018)
p = ggplot(fr_final, aes(x = Data, y = Wartość)) + geom_bar(stat='identity')
p = p +  
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  coord_cartesian(ylim = c(1.5, 2.1)) +
  scale_y_continuous(breaks=seq(1.6,2.1,0.2)) +
  
  labs(title = "Współczynnik dzietności we Francji", subtitle = 'Lata 1990-2018')
p


