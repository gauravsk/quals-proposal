# read iButtons

rm(list = ls())

library(ggplot2)
library(dplyr, tidyr)

fls <- dir('~/Dropbox/spatial_tapioca/data/environmental/ibutton_data/clean/', '.csv$', full.names = T)

ib <- lapply(fls, read.csv)

ib_all <- do.call(rbind, ib)

ib_all$date <- as.Date( strptime( ib_all$date, '%m-%d-%y') )

ib_all$date <- as.Date( ib_all$date )


ib_daily <- 
  ib_all %>% 
  group_by(plot_id, date, doy) %>% 
  summarise(Tmin = min(value), Tmax = max(value), n = n()) %>%
  filter( n == 12 ) %>% 
  filter( doy < 175) %>%
  mutate( range = Tmax - Tmin , Tave = (Tmin + Tmax)/2)

#
stat_weather <- read.csv('~/Dropbox/spatial_tapioca/data/environmental/daily_weather_2015_to_2016.csv', sep = ',') 

stat_weather$date <- as.Date(strptime( stat_weather$date, '%m/%d/%y'))
stat_weather$year <- as.numeric(stat_weather$year)

stat_weather[ stat_weather == -9999.0 ] <- NA

stat_weather <- 
  stat_weather %>% 
  dplyr::select(date, doy, solar.rad.kW.hr.m2, air.temp.max.deg..C, air.temp.min.deg..C, air.temp.ave..deg..C) %>% 
  rename(solar_rad = solar.rad.kW.hr.m2, Tmax_stat = air.temp.max.deg..C, Tmin_stat = air.temp.min.deg..C, Tave_stat = air.temp.ave..deg..C)

#
ib_daily <- merge( ib_daily %>% ungroup(), stat_weather, by = c('date', 'doy'))

ib_daily <- 
  ib_daily %>% 
  mutate( plot_anom_Tmin = Tmin - Tmin_stat, plot_anom_Tmax = Tmax - Tmax_stat)

monthly_temps <- 
  ib_daily %>% 
  mutate( month = as.numeric(strftime(date, '%m'))) %>% 
  group_by( month, plot_id ) %>% 
  summarise( Tmax = mean(Tmax), Tmin  = mean(Tmin))

# write.csv(monthly_temps, '~/Dropbox/Sedgwick/spatial_tapioca/data/environmental/monthly_plot_temps.csv')


ggplot(ib_daily, aes( x = date, y = plot_anom_Tmin, color = factor(plot_id))) + 
  geom_line() + 
  facet_wrap(~plot_id) 

ggplot(ib_daily, aes( x = date, y = plot_anom_Tmax, color = factor(plot_id))) + 
  geom_line() + 
  facet_wrap(~plot_id) 

monthly_plot_anoms <- 
  ib_daily %>% 
  mutate( month = as.numeric( strftime(date , '%m') )) %>% 
  group_by(plot_id, month) %>% 
  summarise( Tmax_stat = mean(Tmax_stat), Tmin_stat = mean(Tmin_stat) , Tmax_anom = mean(plot_anom_Tmax), Tmin_anom = mean(plot_anom_Tmin))

ggplot( monthly_plot_anoms, aes( x = month, y = Tmin_anom , color = factor( plot_id ) )) + geom_point() + geom_line() 
ggplot( monthly_plot_anoms, aes( x = month, y = Tmax_anom , color = factor( plot_id ) )) + 
  geom_point() + geom_line() + ggtitle("Plot temperature anomoly", subtitle = "Difference between air temperature at plot and air temperature at central weather")





ggplot( monthly_plot_anoms, aes( x = Tmin_anom, y = Tmax_anom, color = factor(plot_id))) + geom_smooth(se = F, method = 'lm') + geom_point() 




monthly_aves <- ib_daily %>% 
  mutate( month = as.numeric( strftime(date , '%m') )) %>% 
  group_by(plot_id, month) %>% summarise(Tave_month = mean(Tave))

ggplot(monthly_aves, aes(x = month, y = Tave_month, color = factor(plot_id))) + geom_point()+geom_line()

monthly_aves %>% ungroup %>% group_by(month) %>% summarise(range = max(Tave_month)-min(Tave_month))