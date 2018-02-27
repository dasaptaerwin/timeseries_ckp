# Time Series R code
# Adapted from: Ryan Womack's code and tutorial (https://www.youtube.com/watch?v=QHsmAM6nktY)
# Revised 2017-02-19
<<<<<<< HEAD
# Written to analyse time series data from Cikapundung River (taken by Sri Aditya)
=======
>>>>>>> 301e40f633e42a9f729bda64e0f1835f8e0d8700


# README: data time series ini diukur di tiga lokasi, pada masing-masing lokasi pengukuran dilakukan 4 kali sehari, dua kali seminggu, selama 8 bulan di tahun 2017.

# LOAD LIBRARY
library('lubridate')
library('tidyverse')
install.packages('forecast')
library('forecast')
install.packages('tseries')
library('tseries')
install.packages('gridExtra')
library('gridExtra')

# LOAD DATA
data<-read.csv('data.csv')

### TDS ###

# CONVERT TO TIME SERIES OBJECT OK
data$combine <- as.POSIXct(paste(data$DATE, data$TIME_WIB), format="%Y-%m-%d %H:%M")
data$combine

data$ts.tds <- ts(data$TDS_PPM)
class(data$ts.tds)
data$ts.tempriver <- ts(data$TEMP_RIVER_C)
class(data$ts.tempriver)
data$ts.tempair <- ts(data$TEMP_AIR_C)
class(data$ts.tempair)

## plot histogram OK
ggplot(data=data, aes(data$TEMP_AIR_C)) +
  geom_histogram(fill="blue") +
  geom_histogram(aes(data$TEMP_RIVER_C), fill="red") +
  labs(title = "Histogram temperatur udara (biru) vs air sungai (merah) Curug Panganten, Bandung",
       subtitle = "Maret-November 2017",
       x = "Temperatur (oC)")

## plot histogram TDS ts OK
ggplot(data=data, aes(data$TDS_PPM)) +
  geom_histogram(fill="blue") +
  labs(title = "Histogram TDS Curug Panganten, Bandung",
       subtitle = "Maret-November 2017",
       x = "TDS (ppm)")

## plot histogram temp river vs air OK
plot(data$ts.tempriver)
ggplot(data=data, aes(x = data$combine)) +
  geom_line(aes(y = data$ts.tempriver), col="red") +
  geom_line(aes(y = data$ts.tempair), col="blue") +
  labs(title = "Temp air sungai dan temp udara Curug Panganten, Bandung",
       subtitle = "Maret-November 2017",
       x = "Bulan", y = "Temperatur (derajat C)") + theme(legend.position="top")

## plot temp air vs river ts OK
ggplot(data=data, aes(x = data$TEMP_RIVER_C, y=data$TEMP_AIR_C)) +
  geom_point() +
  labs(title = "Temp air sungai vs temp udara Curug Panganten, Bandung",
       subtitle = "Maret-November 2017",
       x = "Temperatur Air Sungai (oC)", y = "Temperatur Udara (oC)") 

ggplot(data=data, aes(x = data$combine, y = data$ts.tempair)) +
  geom_line(stat = "identity") +
  labs(title = "Temp udara Curug Panganten, Bandung",
       subtitle = "Maret-November 2017",
       x = "Bulan", y = "Temp (derajat C)")

## plot TDS ts
plot(data$ts.tds)
ggplot(data=data, aes(x = data$combine, y = data$ts.tds)) + geom_line() +
  labs(title = "TDS PLOT Curug Panganten, Bandung",
       subtitle = "Maret-November 2017",
       x = "Bulan", y = "TDS (ppm)")

# MOVING AVERAGE (MA)
data$tdsma7 <- ma(data$ts.tds, order=7)   # orde mingguan (7 hari)
data$tdsma30 <- ma(data$ts.tds, order=30) # orde bulanan (30 hari)
data$tempriverma7 <- ma(data$ts.tempriver, order=7)
data$tempriverma30 <- ma(data$ts.tempriver, order=30)
data$tempairma7 <- ma(data$ts.tempair, order=7)
data$tempairma30 <- ma(data$ts.tempair, order=30)

# CLEAN MISSING VALUES
data$tdsma7 <- tsclean(data$tdsma7)
data$tdsma30 <- tsclean(data$tdsma30)
data$tempriverma7 <- tsclean(data$tempriverma7)
data$tempriverma30 <- tsclean(data$tempriverma30)
data$tempairma7 <- tsclean(data$tempairma7)
data$tempairma30 <- tsclean(data$tempairma30)

par(mfrow=c(3,1))
plot(data$ts.tds)
plot(data$tdsma7)
plot(data$tdsma30)

p1 <- ggplot() +
  geom_line(data=data, aes(x = data$combine, y = data$ts.tds)) +
  labs(title = "Moving average TDS orde 7 dan 30 hari Curug Panganten, Bandung",
       x = "Bulan", y = "TDS data (ppm)",
       subtitle = "Maret-November 2017") +
  ylim(60, 70)

p2 <- ggplot() +
  geom_line(data=data, aes(x = data$combine, y = data$tdsma7)) +
  labs(x = "Bulan", y = "TDS MA orde 7 (ppm)",
       subtitle = "Maret-November 2017")  +
  ylim(60, 70)

p3 <- ggplot() +
  geom_line(data=data, aes(x = data$combine, y = data$tdsma30)) +
  labs(x = "Bulan", y = "TDS MA orde 30 (ppm)",
       subtitle = "Maret-November 2017")  +
  ylim(60, 70)

grid.arrange(p1,p2,p3, ncol=1)

# DECOMPOSE
## TDS
tds_ma <- ts(na.omit(data$tdsma7), frequency=100) #OK
decomp_tds <- stl(tds_ma, s.window="periodic")
plot(decomp_tds)



tds_ma <- ts(na.omit(data$tdsma30), frequency=30)
decomp <- stl(tds_ma, s.window="periodic")
deseasonal <- seasadj(decomp)
plot(decomp)

## TEMPRIVER
tempriver_ma <- ts(na.omit(data$tempriverma7), frequency=100) #OK
decomp_tempriver <- stl(tempriver_ma, s.window="periodic")
deseasonal <- seasadj(decomp)
plot(decomp)

tempriver_ma <- ts(na.omit(data$tempriverma30), frequency=30)
decomp <- stl(tempriver_ma, s.window="periodic")
deseasonal <- seasadj(decomp)
plot(decomp)

## TEMPAIR
tempair_ma <- ts(na.omit(data$tempairma7), frequency=100)
decomp_tempair <- stl(tempair_ma, s.window="periodic")
deseasonal <- seasadj(decomp)
plot(decomp)

tempair_ma <- ts(na.omit(data$tempairma30), frequency=30)
decomp <- stl(tempair_ma, s.window="periodic")
deseasonal <- seasadj(decomp)
plot(decomp)

# CORRELATIONS
ts.plot(data$ts.tds)
ts.plot(data$tdsma7)
ts.plot(data$tdsma30)
ts.plot(data$ts.tds, data$tdsma7, data$tdsma30, col=c("green", "blue", "red"))
ts.plot(data$ts.tds, data$ts.tempriver, col=c("green", "blue"))


dev.off()
par(mar = c(5, 5, 3, 5))
plot(data$ts.tds, type ="l", ylab = "TDS (ppm)",
     main = "TDS and Temp Air", xlab = "Time",
     col = "blue")
par(new = TRUE)
plot(data$TEMP_RIVER_C, type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 2)
axis(side = 4)
mtext("Temp Air (oC)", side = 4, line = 3)
legend("topleft", c("TDS", "Temp Air"),
       col = c("blue", "red"), lty = c(1, 2))

par(mar = c(5, 5, 3, 5))
plot(data$ts.tds, type ="l", ylab = "TDS (ppm)",
     main = "TDS and Temp Air", xlab = "Time",
     col = "blue")
par(new = TRUE)
plot(data$TEMP_RIVER_C, type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 2)
axis(side = 4)
mtext("Temp Air (oC)", side = 4, line = 3)
legend("topleft", c("TDS", "Temp Air"),
       col = c("blue", "red"), lty = c(1, 2))

# begin testing xts 
decomp_tds <- ets(tds_ma)
plot(decomp_tds$states)
install.packages('xts')
library('xts')
plot(as.xts(decomp_tds))
deseasonal <- seasadj(decomp)
plot(decomp)
# end testing xts

# begin testing forecast
install.packages("fpp")
library(fpp)
data(ausbeer)
timeserie_beer = tail(head(ausbeer, 17*4+2),17*4-4)
plot(as.ts(timeserie_beer))
install.packages("forecast")
library(forecast)
trend_beer = ma(timeserie_beer, order = 4, centre = T)
plot(as.ts(timeserie_beer))
lines(trend_beer)
plot(as.ts(trend_beer))
detrend_beer = timeserie_beer - trend_beer
plot(as.ts(detrend_beer))

decompose_beer = decompose(ts_beer, "additive")

plot(as.ts(decompose_beer$seasonal))
plot(as.ts(decompose_beer$trend))
plot(as.ts(decompose_beer$random))
plot(decompose_beer)

trend_tds = ma(data$ts.tds, order = 10, centre = T)
plot(as.ts(data$ts.tds), col="grey")
lines(trend_tds, col="red")
detrend_tds = data$ts.tds - trend_tds
plot(as.ts(detrend_tds))

decompose_tds <- decompose(tds_ma, "additive")
plot(as.ts(decompose_tds$seasonal))
plot(as.ts(decompose_tds$trend))
plot(as.ts(decompose_tds$random))
plot(decompose_tds)

# end testing forecast