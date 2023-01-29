# code for section 4

library(nasapower)
library(data.table)
library(stats)
library(forecast)
library(ggplot2)
library(xts)
library(RColorBrewer)
library(gridExtra)
library(lubridate)

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
  names(temperatures.ts) = 'temperature'
  
  return(temperatures.ts)
}


BERGEN = get_ts(5.2, 60.23)
KATANIA = get_ts(15.08, 37.51)
LIBREVILLE = get_ts(9.27, 0.23)
HONG_KONG = get_ts(114.17, 22.32) 
ANTARKTYDA = get_ts(164.06, -74.41) 
BUENOS = get_ts(-58.55, -34.35) 

# decompose
my_seasonality = function(place){
  place_matrix = t(matrix(place, nrow = 365, ncol = 35))
  return(colMeans(place_matrix))
}

deseasoned = function(place){
  s = my_seasonality(place)
  season_comp = rep(s, 35)
  return(place - season_comp)
}


des_BE = deseasoned(BERGEN)
des_KAT = deseasoned(KATANIA)
des_LIB = deseasoned(LIBREVILLE)
des_HK = deseasoned(HONG_KONG)
des_ANT = deseasoned(ANTARKTYDA)
des_BUEN = deseasoned(BUENOS)

my_seasonality_after_trend = function(place){
  place = place[183:12592]
  place_matrix = t(matrix(place, nrow = 365, ncol = 34))
  return(colMeans(place_matrix))
}

deseasoned_after_trend = function(place){
  s = my_seasonality_after_trend(place)
  season_comp = rep(s, 34)
  return(place[183:12592] - season_comp)
}


dates = seq(ymd('1985-07-02'), ymd('2019-07-01'), by = "days")
dates = dates[!(month(dates) == 2 & day(dates) == 29)]


LIB_trend = ma(LIBREVILLE, order = 365)
det_LIB = as.ts(LIBREVILLE) - LIB_trend
des_LIB = ts(deseasoned_after_trend(det_LIB))
dt = data.table(dates, des_LIB)
des_LIB = as.xts(dt, order.by = dt[, dates])


HK_trend = ma(HONG_KONG, order = 365)
det_HK = as.ts(HONG_KONG) - HK_trend
des_HK = ts(deseasoned_after_trend(det_HK))
dt = data.table(dates, des_HK)
des_HK = as.xts(dt, order.by = dt[, dates])

# --- modelling
arima_be = auto.arima(des_BE, ic = 'bic') # 2, 1, 2 <- ARMA
arima_kat = auto.arima(des_KAT, ic = 'bic') # 0, 1, 3 <- MA
arima_lib = auto.arima(des_LIB, ic = 'bic') # 1, 0, 3 <- ARMA
arima_hk = auto.arima(des_HK, ic = 'bic') # 1, 0, 2 <- ARMA
arima_ant = auto.arima(des_ANT, ic = 'bic') # 0, 1, 4 <- MA
arima_buen = auto.arima(des_BUEN, ic = 'bic') # 1, 0, 1<- ARMA


# ARIMA simulation
par(mfrow = c(3, 2))
## be
plot(data.table(id=1:12775, des_BE), type = 'l', main = 'BERGEN', 
     xlim=c(5475, 6570))
lines(arima_be$fitted, col = 'red')

## KAT
plot(data.table(id=1:12775, des_KAT), type = 'l', main = 'KATANIA', 
     xlim=c(5475, 6570))
lines(arima_kat$fitted, col = 'red')

## LIB
plot(data.table(id=1:12410, des_LIB), type = 'l', main = 'LIBREVILLE', 
     xlim=c(5475, 6570))
lines(arima_lib$fitted, col='red')


## HK
plot(data.table(id=1:12410, des_HK), type = 'l', main = 'HONG KONG', 
     xlim=c(5475, 6570))
lines(arima_hk$fitted, col = 'red')

## ANT
plot(data.table(id = 1:12775, des_ANT), type = 'l', main = 'ANTARKTYDA', 
     xlim=c(5475, 6570))
lines(arima_ant$fitted, col='red')

## BUEN
plot(data.table(id=1:12775, des_BUEN), type = 'l', main = 'BUENOS AIRES', 
     xlim=c(5475, 6570))
lines(arima_buen$fitted, col = 'red')

# inverse roots

A = autoplot(arima_be, main = 'Inverse roots: Bergen ARIMA(2, 1, 2)') + 
  theme(legend.position =  'None', plot.title = element_text(hjust = 0.5))
B = autoplot(arima_lib, main = 'Inverse roots: Libreville ARIMA(1, 0, 3)') + 
  theme(legend.position =  'None', plot.title = element_text(hjust = 0.5))
C = autoplot(arima_lib, main = 'Inverse roots: Hong Kong ARIMA(1, 0, 2)') + 
  theme(legend.position =  'None', plot.title = element_text(hjust = 0.5))
D = autoplot(arima_buen, main = 'Inverse roots: Buenos Aires ARIMA(1, 0, 1)') + 
  theme(legend.position =  'None', plot.title = element_text(hjust = 0.5))
grid.arrange(A, B, C, D, nrow = 2)

X = autoplot(arima_kat, main = 'Inverse roots MA: Katania ARIMA(0, 1, 3)')  + 
  theme(legend.position =  'None', plot.title = element_text(hjust = 0.5))
Y = autoplot(arima_ant, main = 'Inverse roots MA: Antarktyda ARIMA(0, 1, 4)') + 
  theme(legend.position =  'None', plot.title = element_text(hjust = 0.5))
grid.arrange(X, Y, nrow = 1)


# acfs for RESIDUALS
par(mfrow = c(3, 2))
acf(arima_be$residuals, main = 'ACF, fluktuacje losowe modelu Bergen', 
    col = colors[4], lag.max = 20)
acf(arima_kat$residuals, main = 'ACF, fluktuacje losowe modelu Katanii', 
    col = colors[4], lag.max = 20)
acf(arima_lib$residuals, main = 'ACF, fluktuacje losowe modelu Libreville', 
    col = colors[4], lag.max = 20)
acf(arima_hk$residuals, main = 'ACF, fluktuacje losowe modelu Hong Kongu', 
    col = colors[4], lag.max = 20)
acf(arima_ant$residuals, main = 'ACF, fluktuacje losowe modelu Antarktydy', 
    col = colors[4], lag.max = 20)
acf(arima_buen$residuals, main = 'ACF, fluktuacje losowe modelu Buenos Aires', 
    col = colors[4], lag.max = 20)

par(mfrow = c(3, 2))
pacf(arima_be$residuals, main = 'PACF, fluktuacje losowe modelu Bergen', 
    col = colors[4], lag.max = 20)
pacf(arima_kat$residuals, main = 'PACF, fluktuacje losowe modelu Katanii', 
    col = colors[4], lag.max = 20)
pacf(arima_lib$residuals, main = 'PACF, fluktuacje losowe modelu Libreville', 
    col = colors[4], lag.max = 20)
pacf(arima_hk$residuals, main = 'PACF, fluktuacje losowe modelu Hong Kongu', 
    col = colors[4], lag.max = 20)
pacf(arima_ant$residuals, main = 'PACF, fluktuacje losowe modelu Antarktydy', 
    col = colors[4], lag.max = 20)
pacf(arima_buen$residuals, main = 'PACF, fluktuacje losowe modelu Buenos Aires', 
    col = colors[4], lag.max = 20)
