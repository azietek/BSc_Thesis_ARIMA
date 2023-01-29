## code to section 2

library(nasapower)
library(data.table)
library(stats)
library(forecast)
library(ggplot2)
library(xts)
library(RColorBrewer)
library(gridExtra)

colors <- brewer.pal(8, "Dark2")

## importing the data & creating TS
get_ts = function(lon, lat){
  data_info = get_power(community = 'ag', pars = 'T2M', 
                        temporal_api = 'daily', lonlat = c(lon, lat),
                        time_standard = 'UTC', 
                        dates = c('1985-01-01', '2019-12-31'))
  
  data = data.table(data_info)
  # deleting 29.02
  data = data[!(MM == 2 & DD == 29)]
  
  
  data = data[, c('LON', 'LAT', 'YEAR', 'MM', 'DD', 'DOY') := NULL]
  
  temperatures.ts = xts(data[, T2M], order.by = data[, YYYYMMDD], 
                        frequency = 365)
  attr(temperatures.ts, 'frequency') = 365
  names(temperatures.ts) = 'temperature'
  
  return(temperatures.ts)
}


BERGEN = get_ts(5.2, 60.23)
KATANIA = get_ts(15.08, 37.51)
LIBREVILLE = get_ts(9.27, 0.23)
HONG_KONG = get_ts(114.17, 22.32) 
ANTARKTYDA = get_ts(164.06, -74.41) 
BUENOS = get_ts(-58.55, -34.35) 

## insight plots
par(mfrow = c(3, 2))
plot(BERGEN, ylim = c(-46, 36), col = colors[3])
plot(KATANIA, ylim = c(-46, 36), col = colors[3])
plot(LIBREVILLE, ylim = c(-46, 36), col = colors[3])
plot(HONG_KONG, ylim = c(-46, 36), col = colors[3], main = 'HONG KONG')
plot(ANTARKTYDA, ylim = c(-46, 36), col = colors[3])
plot(BUENOS, ylim = c(-46, 36), col = colors[3], main = 'BUENOS AIRES')

amplitude = function(place){
  return(max(place) - min(place))
}

amplitude(BERGEN)
amplitude(KATANIA)
amplitude(LIBREVILLE)
amplitude(HONG_KONG)
amplitude(ANTARKTYDA)
amplitude(BUENOS)

acf(BERGEN)
pacf(BERGEN)

acf(KATANIA)
pacf(KATANIA)

acf(LIBREVILLE)
pacf(LIBREVILLE)

acf(HONG_KONG)
pacf(HONG_KONG)

acf(ANTARKTYDA)
pacf(ANTARKTYDA)

acf(BUENOS)
pacf(BUENOS)


par(mfrow = c(2, 2))
acf(BERGEN, lag.max = 30, main = 'Wykres ACF dla Bergen: 30 dni', col = colors[5])
acf(BERGEN, lag.max = 365, main = 'Wykres ACF dla Bergen: 1 rok', col = colors[5])
acf(BERGEN, lag.max = 3650, main = 'Wykres ACF dla Bergen: 10 lat', col = colors[5])
acf(BERGEN, lag.max = 7300, main = 'Wykres ACF dla Bergen: 20 lat', col = colors[5])

par(mfrow = c(2, 2))
acf(LIBREVILLE, lag.max = 30, main = 'Wykres ACF dla Libreville: 30 dni', col = colors[5])
acf(LIBREVILLE, lag.max = 365, main = 'Wykres ACF dla Libreville: 1 rok', col = colors[5])
acf(LIBREVILLE, lag.max = 3650, main = 'Wykres ACF dla Libreville: 10 lat', col = colors[5])
acf(LIBREVILLE, lag.max = 7300, main = 'Wykres ACF dla Libreville: 20 lat', col = colors[5])

par(mfrow = c(1, 2))
pacf(BERGEN, main = 'Wykres PACF dla Bergen', col = colors[5])
pacf(LIBREVILLE, main = 'Wykres PACF dla Libreville', col = colors[5])
