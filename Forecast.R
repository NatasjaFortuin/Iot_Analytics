library(readr)
library(lubridate)
library(tidyverse)
install.packages("imputeTS")
library(imputeTS)
install.packages("fracdiff")
install.packages("Rtools")
library(forecast)
library(caret)
library(mlbench)
install.packages("prophet")
library(prophet)
library(zoo)

# Load data----
household_power_consumption <- read_delim("household_power_consumption.txt", 
                          delim = ";", escape_double = FALSE, 
                          col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                          Time = col_time(format = "%H:%M:%S")),
                          trim_ws = TRUE)

# Combine date & time----
Usage <-  household_power_consumption %>% 
  unite(col = "DateTime", Date, Time, sep = " ", remove = F) %>% 
  mutate(DateTime = as_datetime(DateTime, tz="GMT"))
str(Usage)


#Fill NA's----
statsNA(Usage$Global_active_power)

head(Usage)

# Last Observartion Carried Forward for NA's----
Workfile <- Usage
Workfile <- na_locf(Workfile,option = "locf")

#check if NA's are replaced with values----
sum(is.na(Workfile))                 
statsNA(Workfile$Global_active_power)
statsNA(Workfile$Sub_metering_2)

# Prepare for forecasting----
 FC_Dec_10 <- Workfile %>% 
  filter(
    # year(DateTime) == 2006 & month(DateTime) == 12 | 
         year(DateTime) == 2007 & month(DateTime) == 12 | 
         year(DateTime) == 2008 & month(DateTime) == 12 | 
         year(DateTime) == 2009 & month(DateTime) == 12)
FC_Dec_10
view(FC_Dec_10)

# Explore data with some plots----
daily <- FC_Dec_10 %>% 
  group_by(Date) %>% 
  summarize(Sub_metering_1 = mean(Sub_metering_1), 
            Sub_metering_2 = mean(Sub_metering_2), 
            Sub_metering_3 = mean(Sub_metering_3)) %>% 
  mutate(wday = wday(Date, label = TRUE))

daily

ggplot(daily, aes(x = wday, y = Sub_metering_1)) + 
  geom_boxplot()

ggplot(daily, aes(x = wday, y = Sub_metering_2)) + 
  geom_boxplot()

ggplot(daily, aes(x = wday, y = Sub_metering_3)) + 
  geom_boxplot()

scatter_violin_1 <- ggplot(data=daily, aes(x=wday, y=Sub_metering_1)) +
  geom_violin(aes(fill=wday, color="Orange")) +
  geom_point(color="black", size=1, position = position_jitter(w=0.05)) +
  theme_minimal()

plot(scatter_violin_1)

scatter_violin_2 <- ggplot(data=daily, aes(x=wday, y=Sub_metering_2)) +
  geom_violin(aes(fill=wday, color="Orange")) +
  geom_point(color="black", size=1, position = position_jitter(w=0.05)) +
  theme_minimal()

print(scatter_violin_2)

scatter_violin_3 <- ggplot(data=daily, aes(x=wday, y=Sub_metering_3)) +
  geom_violin(aes(fill=wday, color="Orange")) +
  geom_point(color="black", size=1, position = position_jitter(w=0.05)) +
  theme_minimal()

print(scatter_violin_3)

####QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQ####----

#### Forecasting DEC 2010 ####

set.seed(7)

#create a 20% sample of the data----
Sample_Dec_10 <- FC_Dec_10[sample(1:nrow(FC_Dec_10), 30000,replace=FALSE),]

## Global Active Power----
# define an 75%/25% train/test split of the dataset----
inTraining <- createDataPartition(FC_Dec_10$Sub_metering_1, 
                                  p = .75, list = FALSE)
training <- FC_Dec_10[inTraining,]
testing <- FC_Dec_10[-inTraining,]

#10 fold cross validation----
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train Forecasting Dec 10
library(forecast)

## Create TS object with Global Active power----
tsGA <- ts(FC_Dec_10$Global_active_power, frequency=60*24*31, start=c(2007))

plot(decompose(tsGA))

fitGA <- tslm(tsGA ~ trend)
fitGA
summary(fitGA)
#Multiple R-squared:  0.00577,	Adjusted R-squared:  0.005762

saveRDS(fitGA, file = "fitGA.rds")

## Create the forecast for sub-meter 1. Forecast ahead 10 time periods----
forecastfitGA <- forecast(fitGA, h=10)

## Plot the forecast for Global Active Power---- 
plot(forecastfitGA)

## Create sub-meter 1 forecast with confidence levels 80 and 90----
forecastfitGAc <- forecast(fitGA, h=20, level=c(80,90))

## Plot sub-meter 1 forecast, limit y and add labels----
plot(forecastfitGA, ylim = c(0, 20), 
     col = "blue", 
     ylab= "Watt-Hours", 
     xlab="Time")

## Create TS object with SubMeter1----
tsSM1 <- ts(FC_Dec_10$Sub_metering_1, frequency=60*24*31, start=c(2007))

plot(decompose(tsSM1))

fitSM1 <- tslm(tsSM1 ~ trend)
fitSM1
summary(fitSM1)
#Multiple R-squared:  0.0003629,	Adjusted R-squared:  0.0003555

saveRDS(fitSM1, file = "fitSM1.rds")

## Create the forecast for sub-meter 1. Forecast ahead 10 time periods----
forecastfitSM1 <- forecast(fitSM1, h=10)

## Plot the forecast for sub-meter 1---- 
plot(forecastfitSM1)

## Create sub-meter 1 forecast with confidence levels 80 and 90----
forecastfitSM1c <- forecast(fitSM1, h=20, level=c(80,90))

## Plot sub-meter 1 forecast, limit y and add labels----
plot(forecastfitSM1c, ylim = c(0, 20), 
     col = "blue", 
     ylab= "Watt-Hours", 
     xlab="Time")

## Sub Meter 2----
# define an 75%/25% train/test split of the dataset----
inTraining <- createDataPartition(FC_Dec_10$Sub_metering_2, 
                                  p = .75, list = FALSE)
training <- FC_Dec_10[inTraining,]
testing <- FC_Dec_10[-inTraining,]

#10 fold cross validation----
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train Forecasting Dec 10
library(forecast)

## Create TS object with SubMeter2----
tsSM2 <- ts(FC_Dec_10$Sub_metering_2, frequency=60*24*31, start=c(2007))

plot(decompose(tsSM2))

fitSM2 <- tslm(tsSM2 ~ trend)
fitSM2
summary(fitSM2)
#Multiple R-squared:  0.001132,	Adjusted R-squared:  0.001125 

saveRDS(fitSM2, file = "fitSM2.rds")

forecastfitSM2 <- forecast(fitSM2, h=10)

## Plot the forecast for sub-meter 2---- 
plot(forecastfitSM2)

## Create sub-meter 2 forecast with confidence levels 80 and 90----
forecastfitSM2c <- forecast(fitSM2, h=20, level=c(80,90))

## Plot sub-meter 2 forecast, limit y and add labels----
plot(forecastfitSM2c, ylim = c(0, 20), 
     col = "orange", 
     ylab= "Watt-Hours", 
     xlab="Time")

## Sub Meter 3----
# define an 75%/25% train/test split of the dataset----
inTraining <- createDataPartition(FC_Dec_10$Sub_metering_3, 
                                  p = .75, list = FALSE)
training <- FC_Dec_10[inTraining,]
testing <- FC_Dec_10[-inTraining,]

#10 fold cross validation----
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#train Forecasting Dec 10
library(forecast)

## Create TS object with SubMeter2----
tsSM3 <- ts(FC_Dec_10$Sub_metering_3, frequency=60*24*31, start=c(2007))

fitSM3 <- tslm(tsSM3 ~ trend)
fitSM3
summary(fitSM3)
#Multiple R-squared:  0.0007671,	Adjusted R-squared:  0.0007596 

saveRDS(fitSM3, file = "fitSM3.rds")

forecastfitSM3 <- forecast(fitSM3, h=10)

## Plot the forecast for sub-meter 2---- 
plot(forecastfitSM3)

## Create sub-meter 3 forecast with confidence levels 80 and 90----
forecastfitSM3c <- forecast(fitSM3, h=20, level=c(80,90))

## Plot sub-meter 3 forecast, limit y and add labels----
plot(forecastfitSM3c, ylim = c(0, 20), 
     col = "orange", 
     ylab= "Watt-Hours", 
     xlab="Time")

####XXXX FORECAST TOTAL SUM XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX####----
## predict > 12mnth - Group_by almost 4yrs & create a time series----
# save a numeric vector containing 47 monthly observations
# from Jan 2007 to Dec 2010 as a time series object

# Subset Global Active Power and create timeseries df----
#create a 20% sample of the data----
Workfile <-Workfile[sample(1:nrow(Workfile), 400000,replace=FALSE),]

## Global Active Power----
# define an 75%/25% train/test split of the dataset----
inTraining <- createDataPartition(Workfile$Sub_metering_1, 
                                  p = .75, list = FALSE)
training <- Workfile[inTraining,]
testing <- Workfile[-inTraining,]

#10 fold cross validation----
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

UsageGA <- Workfile %>% 
  group_by(year = year(DateTime), month = month(DateTime)) %>% 
  summarise(sum = sum(Global_active_power)) %>% 
  filter(year != 2006)

myts <- ts(UsageGA$sum, start=c(2007, 1), frequency=13)

df_new_GA <- data.frame(year = c(2010, rep(2011,12)) ,
                         month  = c(12, 1:12),
                         sum = df_arimaGA$`Point Forecast`)
df_new_GA
UsageGA

df_new_GA_total <- bind_rows(df_new_GA, UsageGA)
df_new_GA_total
ts_df_new_GA_total <- ts(df_new_GA_total$sum, 
                         start=c(2007, 1), 
                         end=c(2011, 12), 
                         frequency=12)

# plot series GA and seasonal decomposition----
autoplot(decompose(myts))

Fit3yr <- stl(myts, s.window = "periodic")

# forecasting GA and autoplot ----
forecastfitGA4yr <- forecast(Fit3yr, h=12)
plot(forecastfitGA4yr, main = "Forecast GA", 
                        xlab = "Date", 
                        ylab = "Amount", 
                        lwd = 3)
autoplot(forecastfitGA4yr)

# Subset Sub Meter 1 and create timeseries df----
UsageSM1 <- Workfile %>% 
  group_by(year = year(DateTime), month = month(DateTime)) %>% 
  summarise(sum = sum(Sub_metering_1)) %>% 
  filter(year != 2006)

mytsSM1 <- ts(Usage$sum, start=c(2007, 1), frequency=13)

df_new_SM1 <- data.frame(year = c(2010, rep(2011,12)) ,
                        month  = c(12, 1:12),
                        sum = df_arimaSM1$`Point Forecast`)
df_new_SM1
UsageSM1

df_new_SM1_total <- bind_rows(df_new_SM1, UsageSM1)
df_new_SM1_total
ts_df_new_SM1_total <- ts(df_new_SM1_total$sum, 
                         start=c(2007, 1), 
                         end=c(2011, 12), 
                         frequency=12)

# plot series SM1 and seasonal decomposition----
autoplot(decompose(mytsSM1))

Fit4yrSM1 <- stl(mytsSM1, s.window = "periodic")

# forecasting SM1 and autoplot----
forecastfit4yrSM1 <- forecast(Fit4yrSM1, h=12)
autoplot(forecastfit4yrSM1)
plot(forecastfit4yrSM1, main = "Forecast SM1", 
                        xlab = "Date", 
                        ylab = "Amount", 
                        lwd = 3)

# Subset Sub Meter 2 and create timeseries df----
UsageSM2 <- Workfile %>% 
  group_by(year = year(DateTime), month = month(DateTime)) %>% 
  summarise(sum = sum(Sub_metering_1)) %>% 
  filter(year != 2006)

mytsSM2 <- ts(UsageSM2$sum, start=c(2007, 1), frequency=13)

df_new_SM2 <- data.frame(year = c(2010, rep(2011,12)) ,
                         month  = c(12, 1:12),
                         sum = df_arimaSM2$`Point Forecast`)
df_new_SM2
UsageSM2

df_new_SM2_total <- bind_rows(df_new_SM2, UsageSM2)
df_new_SM2_total
ts_df_new_SM2_total <- ts(df_new_SM2_total$sum, 
                          start=c(2007, 1), 
                          end=c(2011, 12), 
                          frequency=12)

# plot series SM2 and seasonal decomposition----
autoplot(decompose(mytsSM2))

Fit4yrSM2 <- stl(mytsSM2, s.window = "periodic")

# forecasting SM2 and autoplot----
forecastfit4yrSM2 <- forecast(Fit4yrSM2, h=12)
autoplot(forecastfit4yrSM2)
plot(forecastfit4yrSM2, main = "Forecast SM2", 
                        xlab = "Date", 
                        ylab = "Amount", 
                        lwd = 3)

#Subset Sub Meter 3 and create timeseries df----
UsageSM3 <- Workfile %>% 
  group_by(year = year(DateTime), month = month(DateTime)) %>% 
  summarise(sum = sum(Sub_metering_1)) %>% 
  filter(year != 2006)

mytsSM3 <- ts(UsageSM3$sum, start=c(2007, 1), frequency=13)

df_new_SM3 <- data.frame(year = c(2010, rep(2011,12)) ,
                         month  = c(12, 1:12),
                         sum = df_arimaSM3$`Point Forecast`)
df_new_SM3
UsageSM3

df_new_SM3_total <- bind_rows(df_new_SM3, UsageSM3)
df_new_SM3_total
ts_df_new_SM3_total <- ts(df_new_SM3_total$sum, 
                          start=c(2007, 1), 
                          end=c(2011, 12), 
                          frequency=12)

# plot series SM3 and seasonal decomposition----
autoplot(decompose(mytsSM3))

Fit4yrSM3 <- stl(mytsSM3, s.window = "periodic")

# forecasting SM3 and autoplot----
forecastfit4yrSM3 <- forecast(Fit4yrSM3, h=12)
plot(forecastfit4yrSM3, main = "Forecast SM3", 
                        xlab = "Date", 
                        ylab = "Amount", 
                        lwd = 3)
autoplot(forecastfit4yrSM3)

#### %%%% WEEKDAY %%%%%%%%%%%%%%%%%%%%%%%%%%%% ####----
Usage_wday_GA <- Workfile %>% 
  group_by(year = year(DateTime), month = month(DateTime), 
           wday = wday(DateTime)) %>% 
  summarise(sum = sum(Global_active_power))

myts_wday_GA <- ts(Usage_wday$sum, start=c(2007), frequency=52)

# plot series GA and seasonal decomposition----
autoplot(decompose(myts_wday_GA))

FitGA_wday <- stl(myts_wday_GA, s.window = "periodic")

# forecasting GA and autoplot ----
forecastFitGA_wday <- forecast(FitGA_wday, h=52)
autoplot(forecastFitGA_wday)
plot(forecastFitGA_wday, main = "Forecast GA-wday", 
                          xlab = "Date", 
                          ylab = "Amount", 
                          lwd = 3)
summary(forecastFitGA_wday)

# Subset Sub Meter 1 and create timeseries df----
Usage_wday_SM1 <- Workfile %>% 
  group_by(year = year(DateTime), month = month(DateTime), 
           wday = wday(DateTime)) %>% 
  summarise(sum = sum(Sub_metering_1))

myts_wday_SM1 <- ts(Usage_wday_SM1$sum, start=c(2007), frequency=52)

# plot series SM1 and seasonal decomposition----
autoplot(decompose(myts_wday_SM1))

FitSM1_wday <- stl(myts_wday_SM1, s.window = "periodic")

# forecasting SM1 and autoplot ----
forecastFitSM1_wday <- forecast(FitSM1_wday, h=52)
autoplot(forecastFitSM1_wday)
plot(forecastFitSM1_wday, main = "Forecast SM1-wday", 
                          xlab = "Date", 
                          ylab = "Amount", 
                          lwd = 3)
summary(FitSM1_wday)

# Subset Sub Meter 2 and create timeseries df----
Usage_wday_SM2 <- Workfile %>% 
  group_by(year = year(DateTime), month = month(DateTime), 
           wday = wday(DateTime)) %>% 
  summarise(sum = sum(Sub_metering_2))

myts_wday_SM2 <- ts(Usage_wday_SM2$sum, start=c(2007), frequency=52)

# plot series SM2 and seasonal decomposition----
autoplot(decompose(myts_wday_SM2))

FitSM2_wday <- stl(myts_wday_SM2, s.window = "periodic")

# forecasting SM2 and autoplot ----
forecastFitSM2_wday <- forecast(FitSM2_wday, h=52)
autoplot(forecastFitSM2_wday)
plot(forecastFitSM2_wday, main = "Forecast SM2-wday", 
                          xlab = "Date", 
                          ylab = "Amount", 
                          lwd = 3)
summary(FitSM2_wday)

# Subset Sub Meter 3 and create timeseries df----
Usage_wday_SM3 <- Workfile %>% 
  group_by(year = year(DateTime), month = month(DateTime), 
           wday = wday(DateTime)) %>% 
  summarise(sum = sum(Sub_metering_3))

myts_wday_SM3 <- ts(Usage_wday_SM3$sum, start=c(2007), frequency=52)

# plot series SM3 and seasonal decomposition----
autoplot(decompose(myts_wday_SM3))

FitSM3_wday <- stl(myts_wday_SM3, s.window = "periodic")

# forecasting SM3 and autoplot ----
forecastFitSM3_wday <- forecast(FitSM2_wday, h=52)
autoplot(forecastFitSM3_wday)
plot(forecastFitSM3_wday, main = "Forecast SM3-wday", 
                          xlab = "Date", 
                          ylab = "Amount", 
                          lwd = 3)
summary(FitSM3_wday)

#### @@@ SEASONAL PLOTS ----

#### CURRENT TOTALS ####

Seasonal_total <- ggseasonplot(myts, year.labels=TRUE, year.labels.left=TRUE) +
                             geom_line(size=2) +
                             geom_point(size=6) +
                             ylab("Amount consumed GA") +
                             ggtitle("Seasonal plot: Energy consumption GA yoy")

plot(Seasonal_total, main = "GA", 
                      xlab = "Date", 
                      ylab = "Amount", 
                      lwd = 3)

Seasonal_total_SM1 <- ggseasonplot(mytsSM1, 
                                   year.labels=TRUE, 
                                   year.labels.left=TRUE) +
                                    geom_line(size=2) +
                                    geom_point(size=6) +
                                    ylab("Amount consumed SM1") +
                        ggtitle("Seasonal plot: Energy consumption SM1 yoy")

plot(Seasonal_total_SM1, 
                         main = "SM1", 
                         xlab = "Date", 
                         ylab = "Amount", 
                         lwd = 3)

Seasonal_total_SM2 <- ggseasonplot(
                            mytsSM2, 
                             year.labels=TRUE, 
                            year.labels.left=TRUE) +
                            geom_line(size=2) +
                            geom_point(size=6) +
                            ylab("Amount consumed SM2") +
                            ggtitle("Seasonal plot: Energy consumption SM2 yoy")

plot(Seasonal_total_SM2, main = "SM2", 
                          xlab = "Date", 
                          ylab = "Amount", 
                          lwd = 3)

Seasonal_total_SM3 <- ggseasonplot(mytsSM3, year.labels=TRUE, 
                                            year.labels.left=TRUE) +
                            geom_line(size=2) +
                            geom_point(size=6) +
                            ylab("Amount consumed SM3") +
                            ggtitle("Seasonal plot: Energy consumption SM3 yoy")

plot(Seasonal_total_SM3, main = "SM3", 
                         xlab = "Date", 
                         ylab = "Amount", 
                         lwd = 3)

#### FORECAST ALL ####

## INCL FORECAST - 2011----

## GA----
Future_season_GA <- ggseasonplot(
  ts_df_new_GA_total, 
  year.labels=TRUE, 
  year.labels.left=TRUE) +
  geom_line(size=1) +
  geom_point(size=3) +
  ylab("Predicted consumption GA_total") +
  ggtitle("YOY monthly trend TOTAL power usage incl pred till 2012_SUM")

plot(Future_season_GA, 
     main = "Forecast GA", 
     xlab = "Date", 
     ylab = "Amount", 
     lwd = 3)
saveRDS(Future_season_GA, file = "PlotGA.rds")

## SM1----
Future_season_SM1 <- ggseasonplot(
  ts_df_new_SM1_total, 
  year.labels=TRUE, 
  year.labels.left=TRUE) +
  geom_line(size=1) +
  geom_point(size=3) +
  # ylab("Predicted consumption KITCHEN") +
  ggtitle("YOY trend SM1 incl pred till 2012_SUM")

plot(Future_season_SM1, 
     main = "Forecast SM1", 
     xlab = "Date", 
     ylab = "Amount", 
     lwd = 3)
saveRDS(Future_season_SM1, file = "PlotSM1.rds")

## SM2----
Future_season_SM2 <- ggseasonplot(
  ts_df_new_SM2_total, 
  year.labels=TRUE, 
  year.labels.left=TRUE) +
  geom_line(size=1) +
  geom_point(size=3) +
  ylab("Predicted consumption LAUNDRY") +
  ggtitle("YOY trend SM2 incl pred till 2012_SUM")

plot(Future_season_SM2, 
     main = "Forecast SM2", 
     xlab = "Date", 
     ylab = "Amount", 
     lwd = 3)
saveRDS(Future_season_SM2, file = "PlotGSM2.rds")

## SM3----
Future_season_SM3 <- ggseasonplot(
  ts_df_new_SM3_total, 
  year.labels=TRUE, 
  year.labels.left=TRUE) +
  geom_line(size=1) +
  geom_point(size=3) +
  ylab("Predicted consumption CLIMATE area") +
  ggtitle("YOY trend SM3 incl pred till 2012_SUM")

plot(Future_season_SM3, 
     main = "Forecast SM3", 
     xlab = "Date", 
     ylab = "Amount", 
     lwd = 3)
saveRDS(Future_season_SM3, file = "PlotGSM3.rds")

## EX FORECAST----
Seasonal_total_GA_forecast_total <- ggseasonplot(
                          myts_wday_GA, 
                          year.labels=TRUE, 
                          year.labels.left=TRUE) +
                          geom_line(size=1) +
                          geom_point(size=3) +
                          ylab("Predicted consumption GA_total") +
                          ggtitle("Seasonal plot: Energy consumption GA_total")

plot(Seasonal_total_GA_forecast_total, 
                                     main = "Forecast GA", 
                                     xlab = "Date", 
                                     ylab = "Amount", 
                                     lwd = 3)

Seasonal_total_SM1_forecast_total <- ggseasonplot(
                      myts_wday_SM1, year.labels=TRUE, 
                      year.labels.left=TRUE) +
                      geom_line(size=1) +
                      geom_point(size=3) +
                      ylab("Predicted consumption SM1_total") +
                      ggtitle("Seasonal plot: Energy consumption SM1_total")

plot(Seasonal_total_SM1_forecast_total, 
                                       main = "Forecast SM1", 
                                       xlab = "Date", 
                                       ylab = "Amount", 
                                       lwd = 3)

Seasonal_total_SM2_forecast_total <- ggseasonplot(
                          myts_wday_SM2, 
                          year.labels=TRUE, 
                          year.labels.left=TRUE) +
                          geom_line(size=1) +
                          geom_point(size=3) +
                          ylab("Predicted consumption SM2_total") +
                          ggtitle("Seasonal plot: Energy consumption SM2_total")

plot(Seasonal_total_SM2_forecast_total, 
                                       main = "Forecast SM2", 
                                       xlab = "Date", 
                                       ylab = "Amount", 
                                       lwd = 3)

Seasonal_total_SM3_forecast_total <- ggseasonplot(
                          myts_wday_SM3, 
                          year.labels=TRUE, 
                          year.labels.left=TRUE) +
                          geom_line(size=1) +
                          geom_point(size=3) +
                          ylab("Predicted consumption SM3_total") +
                          ggtitle("Seasonal plot: Energy consumption SM3_total")

plot(Seasonal_total_SM3_forecast_total, 
                                       main = "Forecast SM3", 
                                       xlab = "Date", 
                                       ylab = "Amount", 
                                       lwd = 3)

#### FORECAST WDAY ####

Seasonal_total_GA_forecast_wday <- ggseasonplot(
                            myts_wday_GA, 
                            year.labels=TRUE, 
                            year.labels.left=TRUE) +
                            geom_line(size=1) +
                            geom_point(size=3) +
                            ylab("Predicted consumption GA_wday") +
                            ggtitle("Seasonal plot: Energy consumption GA_wday")

plot(Seasonal_total_GA_forecast_wday, 
                                     main = "Forecast GA", 
                                     xlab = "Date", 
                                     ylab = "Amount", 
                                     lwd = 3)

Seasonal_total_SM1_forecast_wday <- ggseasonplot(
                          myts_wday_SM1, 
                          year.labels=TRUE, year.labels.left=TRUE) +
                          geom_line(size=1) +
                          geom_point(size=3) +
                          ylab("Predicted consumption SM1_wday") +
                          ggtitle("Seasonal plot: Energy consumption SM1_wday")

plot(Seasonal_total_SM1_forecast_wday, 
                                     main = "Forecast SM1", 
                                     xlab = "Date", 
                                     ylab = "Amount", 
                                     lwd = 3)

Seasonal_total_SM2_forecast_wday <- ggseasonplot(
                           myts_wday_SM2, 
                           year.labels=TRUE, 
                           year.labels.left=TRUE) +
                           geom_line(size=1) +
                           geom_point(size=3) +
                           ylab("Predicted consumption SM2_wday") +
                           ggtitle("Seasonal plot: Energy consumption SM2_wday")

plot(Seasonal_total_SM2_forecast_wday, 
                                       main = "Forecast SM2", 
                                       xlab = "Date", 
                                       ylab = "Amount", 
                                       lwd = 3)

Seasonal_total_SM3_forecast_wday <- ggseasonplot(
                          myts_wday_SM3, 
                          year.labels=TRUE, 
                          year.labels.left=TRUE) +
                          geom_line(size=1) +
                          geom_point(size=3) +
                          ylab("Predicted consumption SM3_wday") +
                          ggtitle("Seasonal plot: Energy consumption SM3_wday")

plot(Seasonal_total_SM3_forecast_wday, 
                                       main = "Forecast SM3", 
                                       xlab = "Date", 
                                       ylab = "Amount", 
                                       lwd = 3)

## Day 1----

Usage_wday_GA %>% 
  filter(wday == "1") %>% 
  ggplot(aes(x = year, y = sum)) +
  geom_point() + 
  geom_line()

Usage_wday_SM1 %>% 
  filter(wday == "1") %>% 
  ggplot(aes(x = year, y = sum)) +
  geom_point() + 
  geom_line()

Usage_wday_SM2 %>% 
  filter(wday == "1") %>% 
  ggplot(aes(x = year, y = sum)) +
  geom_point() + 
  geom_line()

Usage_wday_SM2 %>% 
  filter(wday == "1") %>% 
  ggplot(aes(x = year, y = sum)) +
  geom_point() + 
  geom_line()

####****CREATE DF FORECASTS************ ####

## GA----
tsGA
forecastfitGA4yr
last_date = index(tsGA)[length(tsGA)]
data.frame(GA_predicted=forecastfitGA4yr,
           date=last_date + seq(1/12, 2, by=1/12)) %>%
            mutate(year=floor(date)) %>%
            mutate(month=round(((date %% 1) * 12) + 1)) -> forecast

last_date = index(tsGA)[length(tsGA)]
data.frame(GA_predicted=forecastfitGA4yr,
           date=last_date + seq(1/12, 2, by=1/12)) %>%
              mutate(year=floor(date)) %>%
              mutate(month=round(((date %% 1) * 12) + 1)) -> forecast
forecast
head(forecast)
Forecast_GAforecasts = as.data.frame(forecast)

ggplot(forecast,
aes(x = year, y = GA_predicted.Point.Forecast)) + 
         geom_point() + 
         geom_line()

## SM1----
tsSM1
forecastfitSM1c
last_date = index(tsSM1)[length(tsSM1)]
data.frame(SM1_predicted=forecastfit4yrSM1,
           date=last_date + seq(1/12, 2, by=1/12)) %>%
  mutate(year=floor(date)) %>%
  mutate(month=round(((date %% 1) * 12) + 1)) -> forecastSM1

forecastSM1
summary(forecastfitSM1)
head(forecastSM1)
Forecast_SM1forecasts = as.data.frame(forecastSM1)

ggplot(forecastSM1,
       aes(x = year, y = SM1_predicted.Point.Forecast)) + 
                                    geom_point() + 
                                    geom_line()
## SM2----
tsSM2
summary(forecastfitSM2c)
last_date = index(tsSM2)[length(tsSM2)]
data.frame(SM2_predicted=forecastfit4yrSM2,
           date=last_date + seq(1/12, 2, by=1/12)) %>%
            mutate(year=floor(date)) %>%
            mutate(month=round(((date %% 1) * 12) + 1)) -> forecastSM2

forecastSM2
head(forecastSM2)
Forecast_SM2forecasts = as.data.frame(forecastSM2)

ggplot(forecastSM2,
       aes(x = year, y = SM2_predicted.Point.Forecast)) + 
                                    geom_point() + 
                                    geom_line()

## SM3----
tsSM3
summary (forecastfitSM3c)
last_date = index(tsSM3)[length(tsSM3)]
data.frame(SM3_predicted=forecastfit4yrSM3,
           date=last_date + seq(1/12, 2, by=1/12)) %>%
              mutate(year=floor(date)) %>%
              mutate(month=round(((date %% 1) * 12) + 1)) -> forecastSM3


forecastSM3
head(forecastSM3)
Forecast_SM3forecasts = as.data.frame(forecastSM3)

ggplot(forecastSM3,
       aes(x = year, y = SM3_predicted.Point.Forecast)) + 
  geom_point() + 
  geom_line()

####++++OTHER FORECAST MODELS++++++++++++#####

## NAIVE----

## GA----
naive_modGA <- naive(myts, h = 12)
summary(naive_modGA)
autoplot(naive_modGA, 
                     main = "Naive Forecast GA", 
                     xlab = "Date", 
                     ylab = "Amount", 
                     lwd = 3)

Naive_GAforecasts = as.data.frame(naive_modGA)

## SM1----
naive_modSM1 <- naive(mytsSM1, h = 12)
summary(naive_modSM1)
autoplot(naive_modSM1, 
                     main = "Naive Forecast SM1", 
                     xlab = "Date", 
                     ylab = "Amount", 
                     lwd = 3)

Naive_SM1forecasts = as.data.frame(naive_modSM1)

## SM2----
naive_modSM2 <- naive(mytsSM2, h = 12)
summary(naive_modSM2)
autoplot(naive_modSM2, 
                     main = "Naive Forecast SM2", 
                     xlab = "Date", 
                     ylab = "Amount", 
                     lwd = 3)
Naive_SM2forecasts = as.data.frame(naive_modSM2)

## SM3----
naive_modSM3 <- naive(mytsSM3, h = 12)
summary(naive_modSM3)
autoplot(naive_modSM3, 
                     main = "Naive Forecast SM3", 
                     xlab = "Date", 
                     ylab = "Amount", 
                     lwd = 3)

Naive_SM13forecasts = as.data.frame(naive_modSM3)

## EXPO SMOOTHING----

# GA----
se_modelGA <- ses(myts, h = 12)
summary(se_modelGA)
autoplot(se_modelGA, main = "SE Forecast SMGA", 
                     xlab = "Date", 
                     ylab = "Amount", 
                     lwd = 3)
# SM1----
se_modSM1 <- ses(mytsSM1, h = 12)
summary(se_modSM1)
autoplot(naive_modSM1, 
                     main = "SE Forecast SM1", 
                     xlab = "Date", 
                     ylab = "Amount", 
                     lwd = 3)
# SM21----
se_modSM2 <- ses(mytsSM2, h = 12)
summary(se_modSM2)
autoplot(naive_modSM2, 
                     main = "SE Forecast SM2", 
                     xlab = "Date", 
                     ylab = "Amount", 
                     lwd = 3)
# SM3----
se_modSM3 <- ses(mytsSM3, h = 12)
summary(se_modSM3)
autoplot(naive_modSM3, 
                     main = "SE Forecast SM3", 
                     xlab = "Date", 
                     ylab = "Amount", 
                     lwd = 3)

# CREATE DF for models----
se_GAforecasts = as.data.frame(se_modelGA)
se_SM1forecasts = as.data.frame(se_modSM1)
se_SM2forecasts = as.data.frame(se_modSM2)
se_SM3forecasts = as.data.frame(se_modSM3)

## ARIMA----

# GA----
arima_modelGA <- auto.arima(myts)
summary(arima_modelGA)
arima_forecastsGA = forecast::forecast(arima_modelGA, h=13)
plot(forecast::forecast(arima_modelGA, h=13))
autoplot(arima_modelGA, 
                       main = "Arima Forecast GA", 
                       xlab = "Date", 
                       ylab = "Amount", 
                       lwd = 3)
df_arimaGA = as.data.frame(arima_forecastsGA)
saveRDS(df_arimaGA, file = "dataGA.rds")


#SM1----
arima_modelSM1 <- auto.arima(mytsSM1)
summary(arima_modelSM1)
arima_forecastsSM1 = forecast::forecast(arima_modelSM1, h=13)
plot(forecast::forecast(arima_modelSM1, h=13))
autoplot(arima_modelSM1, 
                       main = "Arima Forecast SM1", 
                       xlab = "Date", 
                       ylab = "Amount", 
                       lwd = 3)
df_arimaSM1 = as.data.frame(arima_forecastsSM1)
saveRDS(df_arimaSM1, file = "dataSM1.rds")

#SM2----
arima_modelSM2 <- auto.arima(mytsSM2)
summary(arima_modelSM2)
arima_forecastsSM2 = forecast::forecast(arima_modelSM2, h=13)
plot(forecast::forecast(arima_modelSM2, h=13))
autoplot(arima_modelSM2, 
                       main = "Arima Forecast SM2", 
                       xlab = "Date", 
                       ylab = "Amount", 
                       lwd = 3)
df_arimaSM2 = as.data.frame(arima_forecastsSM2)
saveRDS(df_arimaSM2, file = "dataSM2.rds")

#SM3----
arima_modelSM3 <- auto.arima(mytsSM3)
summary(arima_modelSM3)
arima_forecastsSM3 = forecast::forecast(arima_modelSM3, h=13)
plot(forecast::forecast(arima_modelSM3, h=13))
autoplot(arima_modelSM3, 
                       main = "Arima Forecast SM3", 
                       xlab = "Date", 
                       ylab = "Amount", 
                       lwd = 3)
df_arimaSM3 = as.data.frame(arima_forecastsSM3)
saveRDS(df_arimaSM3, file = "dataSM3.rds")


#### CREATE 1 DF FOR ALL FORECASTS ####
## rename columns----

# Armima
df_arimaGA 
names(df_arimaGA) <- c("Point Forecast", "Lo80", "Hi80", "Lo95", "Hi95")

# Smooth expo
se_GAforecasts
names(se_GAforecasts) <- c("Point Forecast", "Lo80", "Hi80", "Lo95", "Hi95")

# Naive
Naive_GAforecasts
names(Naive_GAforecasts) <- c("Point Forecast", "Lo80", "Hi80", "Lo95", "Hi95")

# Forecast
Forecast_GAforecasts
names(Forecast_GAforecasts) <- c("Point Forecast", "Lo80", "Hi80", "Lo95", 
                                 "Hi95", "date", "year", "month")
