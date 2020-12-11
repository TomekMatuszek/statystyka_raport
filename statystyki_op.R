# minimalna i maksymalna dzietnosc
min(dane$values)
max(dane$values)

# srednia i miediana
mean(dane$values)
median(dane$values)

# odchylenie standardowe
sd(dane$values)

# NUTSy z najwyzsza i najnizsza dzietnoscia
dane[which.max(dane$values),]
dane[which.min(dane$values),]

# kwantyle
quantile(dane$values)[c(2, 4)]
