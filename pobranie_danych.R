library(eurostat)
fer = search_eurostat(pattern = "fertility rate", type = "table")
dane = get_eurostat("tgs00100", type = "code", time_format = "date", filters = list(time=2018))
