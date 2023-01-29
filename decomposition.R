# code for section 3

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
  # usunięcie 29.02
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

# seasonality

my_seasonality = function(place, len=35){
  place_matrix = t(matrix(place, nrow = 365, ncol = len))
  return(colMeans(place_matrix))
}

deseasoned = function(place, len = 35){
  s = my_seasonality(place, len)
  season_comp = rep(s, len)
  return(place - season_comp)
}

des_BE = deseasoned(BERGEN)
des_KAT = deseasoned(KATANIA)
des_LIB = deseasoned(LIBREVILLE)
des_HK = deseasoned(HONG_KONG)
des_ANT = deseasoned(ANTARKTYDA)
des_BUEN = deseasoned(BUENOS)

# trend

draw_ma = function(place, title = ''){
  my_dt = data.table(id=1:12775, place)
  ma_30 = data.table(id=1:12775, miesiac=ma(place, order=30))
  ma_183 = data.table(id=1:12775, pol_roku=ma(place, order=183))
  ma_365 = data.table(id=1:12775, rok=ma(place, order=365))
  ma_3y = data.table(id=1:12775, lata3=ma(place, order=365*3))
  ma_5y = data.table(id=1:12775, lata5=ma(place, order=365*5))
  
  my_plot = ggplot(my_dt) + 
    geom_line(aes(x = id, y = temperature, color = 'temperature')) +
    geom_line(data = ma_30, aes(x = id, y = miesiac, color='miesiac'), linewidth=0.8) +
    geom_line(data = ma_183, aes(x = id, y = pol_roku, color = 'pol roku'), linewidth=0.8) +
    geom_line(data = ma_365, aes(x = id, y = rok, color = 'rok'), linewidth=0.8)  +
    geom_line(data = ma_3y, aes(x = id, y = lata3, color = '3 lata'), linewidth=0.8) +
    geom_line(data = ma_5y, aes(x = id, y = lata5, color = '5 lat'), linewidth=0.8) +
    labs(x = NULL, y = NULL, title = title) +
    scale_color_manual(values = c('temperature' = colors[6], 
                                  'miesiac' = colors[3], "pol roku" = colors[2],
                                  'rok' = colors[8], '3 lata' = colors[5],
                                  '5 lat' = colors[1])) +
    labs(color = 'wartosc q') + 
    theme(panel.border = element_blank(),
          panel.grid.major = element_line(colour = 'grey'),
          panel.grid.minor = element_line(colour = 'grey'),
          plot.title = element_text(hjust = 0.5),
          legend.position = 'right') + scale_x_continuous(limits = c(5475, 7300))
  return(my_plot)
}


# xlim, nazwy osi do zmiany
BER = draw_ma(BERGEN, title = 'Bergen')
KAT = draw_ma(KATANIA, title = 'Katania')
LIB = draw_ma(LIBREVILLE, title = 'Libreville')
HK = draw_ma(HONG_KONG, title = 'Hong Kong')
ANT = draw_ma(ANTARKTYDA, title = 'Antarktyda')
BUE = draw_ma(BUENOS, title = 'Buenos Aires')

grid.arrange(BER, KAT, LIB, HK, ANT, BUE, nrow = 3, 
             top = 'Symetryczna ruchoma srednia dla lokacji zalezna od q',
             left = 'temperatura', bottom = 'dzien')

draw_year_ma = function(place, title = ''){
  ma_365 = data.table(id=1:12775, ROK=ma(place, order=365))
  print(-(first(na.omit(ma_365))$ROK - last(na.omit(ma_365))$ROK))
  
  my_plot = ggplot(ma_365) + 
    geom_line(aes(x = id, y = ROK, color = 'temperature'), linewidth=1) +
    labs(x = 'dzien', y = 'temperatura', subtitle = title, title = 'Filtr ruchomej średniej dla q=182') +
    theme(panel.border = element_blank(),
          panel.grid.major = element_line(colour = 'grey'),
          panel.grid.minor = element_line(colour = 'grey'),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = 'none') + 
    scale_x_continuous(limits = c(0, 12775))
  return(my_plot)
}

BER2 = draw_year_ma(BERGEN, title = 'Bergen')
KAT2 = draw_year_ma(KATANIA, title = 'Katania')
LIB2 = draw_year_ma(LIBREVILLE, title = 'Libreville')
HK2 = draw_year_ma(HONG_KONG, title = 'Hong Kong')
ANT2 = draw_year_ma(ANTARKTYDA, title = 'Antarktyda')
BUE2 = draw_year_ma(BUENOS, title = 'Buenos')
grid.arrange(BER2, KAT2, ANT2, BUE2, nrow = 2, 
             top = 'Filtr ruchomej średniej dla q=182',
             left = 'temperatura', bottom = 'dzien')
LIB2
HK2

# decomposition

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

library(lubridate)
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

par(mfrow = c(3, 2))
plot(des_BE, col = colors[1], main = 'Bergen')
plot(des_KAT, col = colors[1], main = 'Katania')
plot(des_LIB, col = colors[1], main = 'Libreville')
plot(des_HK, col = colors[1], main = 'Hong Kong')
plot(des_ANT, col = colors[1], main = 'Antarktyda')
plot(des_BUEN, col = colors[1], main = 'Buenos')

par(mfrow = c(1, 2))
acf(des_BE, lag.max = 365, main = 'ACF, fluktuacje losowe Bergen', 
    col = colors[5])
pacf(des_BE, main = 'PACF, fluktuacje losowe Bergen', 
     col = colors[5])


par(mfrow = c(1, 2))
acf(des_LIB, lag.max = 365, main = 'ACF, fluktuacje losowe Libreville', 
    col = colors[5])
pacf(des_LIB, main = 'PACF, fluktuacje losowe Libreville', 
     col = colors[5])
