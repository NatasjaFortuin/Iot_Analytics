library(readr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(plotly)

# Load data----
household_power_consumption <- read_delim("household_power_consumption.txt", 
                        delim = ";", escape_double = FALSE, 
                        col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                        Time = col_time(format = "%H:%M:%S")),
                        trim_ws = TRUE)

# Combine date & time----
Power <-  household_power_consumption %>% 
  unite(col = "DateTime", Date, Time, sep = " ", remove = F) %>% 
  mutate(DateTime = as_datetime(DateTime, tz="GMT"))
str(Power)
View(Power)

#### DAY ANALYSIS 9 Jan 2008 ####
## Subset the 9th day of January 2008 - All observations----
Jan_9_2008 <- Power %>% 
  dplyr::filter(year(DateTime) == 2008 & month(DateTime) == 1 & day(DateTime) == 9)
Jan_9_2008

## Plot sub-meter 1----
ggplot(Jan_9_2008, aes(x = Time, Sub_metering_1)) + 
geom_col(color = "blue", size = 3, fill = "white") 


## Plot sub-meter 2----
ggplot(Jan_9_2008, aes(x = Time, y = Sub_metering_2)) + 
  geom_col(color = "yellow", size = 3, fill = "White")

## Plot sub-meter 3----
ggplot(Jan_9_2008, aes(x = Time, y = Sub_metering_3)) + 
  geom_point(color = "red", size = 3, fill = "white")

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(Power, x = ~Power$DateTime, y = ~Power$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Power$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~Power$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

plot_ly(Jan_9_2008, x = ~Jan_9_2008$DateTime, y = ~Jan_9_2008$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Jan_9_2008$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~Jan_9_2008$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset the 9th day of January 2008 - 10 Minute frequency----
Jan_9_2008_Min10 <- filter(Jan_9_2008, year(DateTime) == 2008 & month(DateTime) == 1 
  & day(DateTime) == 9 & minute(DateTime) == 0 | minute(DateTime) == 10 | 
    minute(DateTime) == 20 | minute(DateTime) == 30 | minute(DateTime) == 40 | 
    minute(DateTime) == 50)

plot_ly(Jan_9_2008_Min10, x = ~Jan_9_2008_Min10$DateTime, y = ~Jan_9_2008_Min10$Sub_metering_1, 
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Jan_9_2008_Min10$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~Jan_9_2008_Min10$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#### PARKED - MONTH ANALYSIS ####
## Subset Month 2009 - All observations----
Month_2009 <- Power %>% 
  dplyr::filter(year(DateTime) == 2009 & month(DateTime))
Month_2009

## Plot sub-meter 1----
ggplot(Month_2009, aes(x = DateTime, Sub_metering_1)) + geom_point()
ggplot(Month_2009, aes(x = DateTime, Sub_metering_2)) + geom_point()
ggplot(Month_2009, aes(x = DateTime, Sub_metering_3)) + geom_point()

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plot_ly(Month_2009, x = ~Month_2009$DateTime, y = ~Month_2009$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Month_2009$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~Month_2009$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset Month 2007, 2008, 2009 - All observations----
Month_3yr <- Power %>% 
  filter(between(year(DateTime), 2007, 2009),  
                  month(DateTime))
Month_3yr

plot_ly(Month_3yr, x = ~Month_3yr$DateTime, y = ~Month_3yr$Sub_metering_1, name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Month_3yr$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~Month_3yr$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption 2007_2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))
#use of waterheater/AC was sign more in Jul 2008 and Jul2009 till Jan 2010

Power <- na.omit(Power)
Month_2009_1 <- Power %>% 
  dplyr::filter(year(DateTime) == 2009 & month(DateTime)) %>% 
  summarize(Submetering_1 = mean(Sub_metering_1))
Month_2009_1

Month_2008_1 <- Power %>% 
  dplyr::filter(year(DateTime) == 2008 & month(DateTime)) %>% 
  summarize(Submetering_1 = mean(Sub_metering_1))
Month_2008_1
  

Month_2007_1 <- Power %>% 
dplyr::filter(year(DateTime) == 2007 & month(DateTime)) %>% 
  summarize(Sub_metering_1 = mean(Sub_metering_1))
Month_2007_1

Meter1_3yr <- rbind(Month_3yr$Sub_metering_1)
Meter1_3yr

#Create table based on Month_2007, _2008 & _2009 for Sub_meter 1 sorted per month
ggplot(Month_3yr, aes(x = year(DateTime), y = Sub_metering_1, colour = year)) + 
  ggplot(aes(x = date_x, y = value, colour = year)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_x_date(labels = date_format("%b"))
  
#geom_histogram() + 
  #facet_wrap(Month_2007$DateTime, Month_2008$DateTime, Month_2009$DateTime, 
  #           nrow = NULL, ncol = 3, shrink = TRUE, scales = "free_x", "free_y")

#### PFFFF ####
#summarize values Meter1 by month
library(dplyr)
Power
na.omit(Power)

SumMonth_1 <- Power %>% 
  group_by(month=floor_date(DateTime, "month")) %>%
  summarize(usage=sum(Sub_metering_1))
SumMonth_1
na.omit(SumMonth_1)

#<dttm>              <dbl>
#1 2007-05-01 00:00:00 75737
#2 2008-04-01 00:00:00 47080
#3 2008-09-01 00:00:00 52376
#4 2010-11-01 00:00:00 46162

SumMonth_2 <- Power %>% 
group_by(month=floor_date(DateTime, "month")) %>%
  summarize(usage=sum(Sub_metering_2))
View(SumMonth_2)
na.omit(SumMonth_2)
#month               usage
#<dttm>              <dbl>
#1 2007-05-01 00:00:00 72132
#2 2008-04-01 00:00:00 70871
#3 2008-09-01 00:00:00 44590
#4 2010-11-01 00:00:00 42282


SumMonth_3 <- Power %>% 
  group_by(year(DateTime), 2007, 2009) %>% 
  group_by(month=floor_date(DateTime, "month")) %>%
  summarize(usage=sum(Sub_metering_3))
View(SumMonth_3)
na.omit(SumMonth_3)
#month                usage
#<dttm>               <dbl>
#1 2007-05-01 00:00:00 229448
#2 2008-04-01 00:00:00 295678
#3 2008-09-01 00:00:00 284282
#4 2010-11-01 00:00:00 247113

## Create TimeSeries for Sub Meter 3----
tsSM3_weekly <- ts(Power$Sub_metering_3, frequency=52, start=c(2007,1))
## Decompose Sub Meter 3 - remove seasonal effects
Decomposed_tsSM3_weekly <- ts(Power$Sub_metering_3, frequency=52, start=c(2007,1))
plot(Decomposed_tsSM3_weekly)

tsSM1_weekly <- ts(Power$Sub_metering_1, frequency=52, start=c(2007,1))
## Decompose Sub Meter 3 - remove seasonal effects
Decomposed_tsSM1_weekly <- ts(Power$Sub_metering_1, frequency=52, start=c(2007,1))
plot(Decomposed_tsSM1_weekly,  ylim = c(0, 60), ylab= "Watt-Hours", 
     xlab="Time - Sub-meter 3")


