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

ggplot() +
  geom_line(data=data, aes(x = data$combine, y = data$ts.tds, col = "data")) +
  geom_line(data=data, aes(x = data$combine, y = data$tdsma7, col = "MA orde 7"))  +
  geom_line(data=data, aes(x = data$combine, y = data$tdsma30, col = "MA orde 30"))  +
  labs(title = "Moving average TDS orde 7 dan 30 hari Curug Panganten, Bandung",
       x = "Bulan", y = "TDS (ppm)",
       subtitle = "Maret-November 2017") + theme(legend.position="top")

# DECOMPOSE
## TDS
tds_ma <- ts(na.omit(data$tdsma7), frequency=30)
decomp <- stl(tds_ma, s.window="periodic")
deseasonal <- seasadj(decomp)
plot(decomp)

tds_ma <- ts(na.omit(data$tdsma30), frequency=30)
decomp <- stl(tds_ma, s.window="periodic")
deseasonal <- seasadj(decomp)
plot(decomp)

## TEMPRIVER
tempriver_ma <- ts(na.omit(data$tempriverma7), frequency=30)
decomp <- stl(tempriver_ma, s.window="periodic")
deseasonal <- seasadj(decomp)
plot(decomp)

tempriver_ma <- ts(na.omit(data$tempriverma30), frequency=30)
decomp <- stl(tempriver_ma, s.window="periodic")
deseasonal <- seasadj(decomp)
plot(decomp)

## TEMPAIR
tempair_ma <- ts(na.omit(data$tempairma7), frequency=30)
decomp <- stl(tempair_ma, s.window="periodic")
deseasonal <- seasadj(decomp)
plot(decomp)

tempair_ma <- ts(na.omit(data$tempairma30), frequency=30)
decomp <- stl(tempair_ma, s.window="periodic")
deseasonal <- seasadj(decomp)
plot(decomp)



plot(data$ts.tds)
plot(data$tdsma7)
plot(data$tdsma30)
