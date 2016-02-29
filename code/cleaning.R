this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)
library(dplyr)
library(tidyr)
library(xts)
library(lubridate)
library(ggplot2)

# read raw sunspot data
sunspot <- read.fwf(file = '../data/Sunspots/data.txt',
                    widths = c(4, 2, 2, 4, 8, 4, 1, 4, 1, 4, 1, 4, 1, 4, 1, 5,
                               1, 5, 1, 5, 1, 5, 1, 5))[,-c(7,9,11,13,15,17,19,21,23)]
colnames(sunspot) <- c('year', 'month', 'day', 'time', 'SPG', 'SPGT', 'umbral.area',
                       'spot.area','', 'Corrected.area', 'dist.center', 'angle.hn', 'Carr.Long', 'Lat', 'Central.mer.dist')
sunspot <- sunspot[,-9]
sunspot <- sunspot[,] %>% 
  mutate(datetime = ymd(paste(year, month, day, sep="-")) + minutes(round(24*60*time))) %>%
  select(-(year:time))

# read raw temperature data
temperature <- read.csv('../data/Temperature/GLB.Ts.csv',na.strings = c('***','****'))


# tidying temperature dataset
temperature <- temperature %>%
  select(Year:Dec) %>%
  gather(Month, LOTI, -Year) %>%
  mutate(datetime=ymd(paste(Year, Month, '01', sep='-'))) %>%
  arrange(datetime) %>%
  select(-(Year:Month))

write.csv(sunspot.monthly, '../data/Sunspots/cleaned/sunspots.montlhy.csv')
# Sunspot data aggregated by datetime
sunspot.agg <- sunspot %>%
  group_by(datetime) %>%
  summarise(count = n(), 
            mean.dist = mean(dist.center), 
            total.area = sum(Corrected.area),
            area.distance = total.area / mean.dist,
            mean.area = mean(Corrected.area),
            density=total.area/count)

# Sunspot data aggregated by year and month
sunspot.monthly <- sunspot.agg %>%
  group_by(year=year(datetime), month=month(datetime)) %>%
  summarise(mean.cnt = mean(count),
            sd.cnt = sd(count),
            mean.mean.dist = mean(mean.dist),
            sd.mean.dist = sd(mean.dist),
            mean.total.area = mean(total.area),
            sd.total.area = sd(total.area),
            mean.ad = mean(area.distance),
            sd.ad = sd(area.distance),
            mean.mean.area=mean(mean.area),
            sd.mean.area=sd(mean.area),
            mean.density=mean(density),
            sd.density=sd(density)) %>%
  ungroup() %>%
  mutate(datetime=ymd(paste(year, month, '01', sep='-'))) %>%
  select(-(year:month))
write.csv(sunspot.monthly, '../data/Sunspots/cleaned/sunspots.montlhy.csv')
sunspot.temperature <- sunspot.monthly %>%
  inner_join(temperature, by='datetime') %>%
  select(datetime, LOTI, mean.cnt:sd.density)

sunspot.year.temperature <- sunspot.agg %>%
  group_by(year=year(datetime)) %>%
  summarise(mean.cnt = mean(count),
            sd.cnt = sd(count),
            n.records=n()) %>%
  ungroup() %>%
  inner_join(
    temperature %>%
      group_by(year=year(datetime)) %>% 
      summarise(
        mean.LOTI = mean(LOTI, na.rm=T),
        sd.LOTI = sd(LOTI, na.rm=T),
        n.LOTI = n()),
    by='year')
write.csv(sunspot.year.temperature, '../data/Sunspots/cleaned/sunspot.year.temperature.csv')


ggplot(sunspot.temperature, aes(x=datetime)) + 
  geom_line(aes(y=mean.cnt*10), colour="blue") +
  geom_smooth(aes(y=mean.cnt*10), method=lm) +
  geom_line(aes(y=LOTI), colour="red") +
  geom_smooth(aes(y=LOTI), method=lm)

qplot(sd.cnt, LOTI, data=sunspot.temperature, geom='point') + geom_smooth(method=lm)

sunspot.yearly <- sunspot.agg %>%
  group_by(year=year(datetime)) %>%
  summarise(mean.cnt = mean(count),
            sd.cnt = sd(count),
            mean.mean.dist = mean(mean.dist),
            sd.mean.dist = sd(mean.dist),
            mean.total.area = mean(total.area),
            sd.total.area = sd(total.area),
            mean.ad = mean(area.distance),
            sd.ad = sd(area.distance),
            mean.mean.area=mean(mean.area),
            sd.mean.area=sd(mean.area),
            mean.density=mean(density),
            sd.density=sd(density)) %>%
  ungroup() %>%
  inner_join(temperature %>%
               mutate(year=year(datetime)) %>%
               group_by(year) %>%
               summarise(mean.LOTI=mean(LOTI, na.rm=T), sd.LOTI = sd(LOTI, na.rm=T)), by='year') %>%
  select(year, mean.LOTI, sd.LOTI, mean.cnt:sd.density)

qplot(sunspot.yearly$mean.cnt, sunspot.yearly$mean.LOTI, geom='point') + geom_smooth(method=lm)
xts(sunspot.monthly$datetime, select(sunspot.monthly, -datetime))

