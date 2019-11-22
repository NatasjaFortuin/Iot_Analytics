library(readr)
library(lubridate)
library(tidyverse)
library(imputeTS)
library(forecast)

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

# Fill NA's----
statsNA(Usage$Global_active_power)

head(Usage)

# Last Observartion Carried Forward for NA's----
Workfile <- Usage
Workfile <- na_locf(Workfile,option = "locf")

# Check if NA's are replaced with values----
sum(is.na(Workfile))                 
statsNA(Workfile$Global_active_power)
statsNA(Workfile$Sub_metering_2)

## SM1----
# Subset Sub Meter 1 and create timeseries df----
DDSM1 <- Workfile %>% 
  group_by(year = year(DateTime), month = month(DateTime), 
           wday = wday(DateTime)) %>% 
  summarise(sum = sum(Sub_metering_1)) %>% 
              filter(year != 2006)
dim(DDSM1)
head(DDSM1)

# Convert dataframe to timeseries----
ts_DDSM1 <- ts(DDSM1$sum, start=c(2007), frequency= 7 * 12)
str(DDSM1)
autoplot(decompose(ts_DDSM1))

# Prepare for forecasting----
FitDDSM1 <- stl(ts_DDSM1, s.window = "periodic")

# Forecasting SM1 and autoplot ----
FC_DDSM1 <- forecast(FitDDSM1, h= 7 + 7 * 12)
autoplot(FC_DDSM1)
plot(FC_DDSM1, main = "Forecast SM1-wday", 
     xlab = "Date", 
     ylab = "Amount", 
     lwd = 3)
summary(FC_DDSM1)

# add predictions to data
temp_pred <- as.vector(FC_DDSM1$mean)
temp_pred <- as_tibble(temp_pred)

temp_df <- as_tibble(seq(as.Date('2010-12-1'),as.Date('2011-12-31'),by = 1))

temp_df <- temp_df %>% 
  group_by(year = year(value), month = month(value), wday = wday(value)) %>% 
  count()

temp_df$pred <- temp_pred$value
temp_df$n <- NULL

# VIOLIN PLOT----
DDSM1 %>% 
  bind_rows(temp_df) %>% 
  ggplot(aes(x = wday, group = wday)) +
    geom_violin(aes(y = sum), fill = "blue", alpha = 0.6) + 
    geom_violin(aes(y = pred), fill = "red", alpha = 0.6)

## SM2----
# Subset Sub Meter 2 and create timeseries df----
DDSM2 <- Workfile %>% 
  group_by(year = year(DateTime), month = month(DateTime), 
           wday = wday(DateTime)) %>% 
  summarise(sum = sum(Sub_metering_2)) %>% 
  filter(year != 2006)
dim(DDSM2)
head(DDSM2)

# Convert dataframe to timeseries----
ts_DDSM2 <- ts(DDSM2$sum, start=c(2007), frequency= 7 * 12)
str(DDSM2)
autoplot(decompose(ts_DDSM2))

# Prepare for forecasting----
FitDDSM2 <- stl(ts_DDSM2, s.window = "periodic")

# Forecasting SM1 and autoplot ----
FC_DDSM2 <- forecast(FitDDSM2, h= 7 + 7 * 12)
autoplot(FC_DDSM2)
plot(FC_DDSM2, main = "Forecast SM2-wday", 
     xlab = "Date", 
     ylab = "Amount", 
     lwd = 3)
summary(FC_DDSM2)

# add predictions to data
temp_pred_SM2 <- as.vector(FC_DDSM2$mean)
temp_pred_SM2 <- as_tibble(temp_pred_SM2)

temp_df_SM2 <- as_tibble(seq(as.Date('2010-12-1'),as.Date('2011-12-31'),by = 1))

temp_df_SM2 <- temp_df_SM2 %>% 
  group_by(year = year(value), month = month(value), wday = wday(value)) %>% 
  count()

temp_df_SM2$pred <- temp_pred_SM2$value
temp_df_SM2$n <- NULL

# VIOLIN PLOT----
DDSM2 %>% 
  bind_rows(temp_df_SM2) %>% 
  ggplot(aes(x = wday, group = wday)) +
  geom_violin(aes(y = sum), fill = "orange", alpha = 0.6) + 
  geom_violin(aes(y = pred), fill = "yellow", alpha = 0.6)

## SM3----
# Subset Sub Meter 3 and create timeseries df----
DDSM3 <- Workfile %>% 
  group_by(year = year(DateTime), month = month(DateTime), 
           wday = wday(DateTime)) %>% 
  summarise(sum = sum(Sub_metering_3)) %>% 
  filter(year != 2006)
dim(DDSM3)
head(DDSM3)

# Convert dataframe to timeseries----
ts_DDSM3 <- ts(DDSM3$sum, start=c(2007), frequency= 7 * 12)
str(DDSM3)
autoplot(decompose(ts_DDSM3))

# Prepare for forecasting----
FitDDSM3 <- stl(ts_DDSM3, s.window = "periodic")

# Forecasting SM1 and autoplot ----
FC_DDSM3 <- forecast(FitDDSM3, h= 7 + 7 * 12)
autoplot(FC_DDSM3)
plot(FC_DDSM3, main = "Forecast SM3-wday", 
     xlab = "Date", 
     ylab = "Amount", 
     lwd = 3)
summary(FC_DDSM3)

# add predictions to data
temp_pred_SM3 <- as.vector(FC_DDSM3$mean)
temp_pred_SM3 <- as_tibble(temp_pred_SM3)

temp_df_SM3 <- as_tibble(seq(as.Date('2010-12-1'),as.Date('2011-12-31'),by = 1))

temp_df_SM3 <- temp_df_SM3 %>% 
  group_by(year = year(value), month = month(value), wday = wday(value)) %>% 
  count()

temp_df_SM3$pred <- temp_pred_SM3$value
temp_df_SM3$n <- NULL

# VIOLIN PLOT----
DDSM3 %>% 
  bind_rows(temp_df_SM3) %>% 
  ggplot(aes(x = wday, group = wday)) +
  geom_violin(aes(y = sum), fill = "green", alpha = 0.6) + 
  geom_violin(aes(y = pred), fill = "turquoise", alpha = 0.6)

#### YOY SEASON PLOTS ####
## SM1----
New_Total_SM1 <-  DDSM1 %>% 
  bind_rows(temp_df)
New_Total_SM1
ts_New_Total_SM1 <- ts(New_Total_SM1$sum,
                       New_Total_SM1$pred,
                       start=c(2007, 1), 
                       end=c(2011, 12),
                       frequency= 7)

SeasonplotSM1 <- ts_New_Total_SM1 %>% 
  ggseasonplot(
    ts_New_Total_SM1, 
    year.labels=TRUE, 
    year.labels.left=TRUE) +
  geom_line(size=1) +
  geom_point(size=3) +
  ylab("Usage SM1_total") +
  ggtitle("YOY trend & predictions -> 2012_SUM")

plot(SeasonplotSM1, 
     main = "YoY SubMeter1", 
     xlab = "Date", 
     ylab = "Amount", 
     lwd = 3)

## SM2----
New_Total_SM2 <-  DDSM2 %>% 
  bind_rows(temp_df_SM2)
New_Total_SM2
ts_New_Total_SM2 <- ts(New_Total_SM2$sum,
                       New_Total_SM2$pred,
                       start=c(2007, 1), 
                       end=c(2011, 12),
                       frequency= 7)

SeasonplotSM2 <- ts_New_Total_SM2 %>% 
  ggseasonplot(
    ts_New_Total_SM2, 
    year.labels=TRUE, 
    year.labels.left=TRUE) +
  geom_line(size=1) +
  geom_point(size=3) +
  ylab("Usage SM2") +
  ggtitle("YOY trend & predictions -> 2012_SUM")

plot(SeasonplotSM2, 
     main = "YoY SM2", 
     xlab = "Date", 
     ylab = "Amount", 
     lwd = 3)

## SM3----
New_Total_SM3 <-  DDSM3 %>% 
  bind_rows(temp_df_SM3)
New_Total_SM3
ts_New_Total_SM3 <- ts(New_Total_SM3$sum,
                       New_Total_SM3$pred,
                       start=c(2007, 1), 
                       end=c(2011, 12),
                       frequency= 7)

SeasonplotSM3 <- ts_New_Total_SM3 %>% 
  ggseasonplot(
    ts_New_Total_SM3, 
    year.labels=TRUE, 
    year.labels.left=TRUE) +
  geom_line(size=1) +
  geom_point(size=3) +
  ylab("Usage SM3") +
  ggtitle("YOY trend & predictions -> 2012_SUM")

plot(SeasonplotSM3, 
     main = "YoY SM3", 
     xlab = "Date", 
     ylab = "Amount", 
     lwd = 3)


#### @@@@ SAME BASED ON MEAN @@@@ ####


## SM1----
# Subset Sub Meter 1 and create timeseries df----
DDSM1_mean <- Workfile %>% 
  group_by(year = year(DateTime), month = month(DateTime), 
           wday = wday(DateTime)) %>% 
  summarise(mean = mean(Sub_metering_1)) %>% 
  filter(year != 2006)
dim(DDSM1_mean)
head(DDSM1_mean)

# Convert dataframe to timeseries----
ts_DDSM1_mean <- ts(DDSM1_mean$mean, start=c(2007), frequency= 7 * 12)
str(DDSM1_mean)
autoplot(decompose(ts_DDSM1_mean))

# Prepare for forecasting----
FitDDSM1_mean <- stl(ts_DDSM1_mean, s.window = "periodic")

# Forecasting SM1 and autoplot ----
FC_DDSM1_mean <- forecast(FitDDSM1_mean, h= 7 + 7 * 12)
autoplot(FC_DDSM1_mean)
plot(FC_DDSM1_mean, main = "Forecast SM1-wday", 
     xlab = "Date", 
     ylab = "Amount", 
     lwd = 3)
summary(FC_DDSM1_mean)

# add predictions to data
temp_pred_SM1_mean <- as.vector(FC_DDSM1_mean$mean)
temp_pred_SM1_mean <- as_tibble(temp_pred_SM1_mean)

temp_df_SM1_mean <- as_tibble(seq(as.Date('2010-12-1'),as.Date('2011-12-31'),by = 1))

temp_df_SM1_mean <- temp_df_SM1_mean %>% 
  group_by(year = year(value), month = month(value), wday = wday(value)) %>% 
  count()

temp_df_SM1_mean$pred <- temp_pred_SM1_mean$value
temp_df_SM1_mean$n <- NULL

# VIOLIN PLOT----
Violin_1_mean <- New_Total_SM1_mean %>% 
  ggplot(aes(x = wday, group = wday)) +
  geom_violin(aes(y = mean), fill = "blue", alpha = 0.6) + 
  geom_violin(aes(y = pred), fill = "red", alpha = 0.6)
print(Violin_1_mean)

## SM2----
# Subset Sub Meter 2 and create timeseries df----
DDSM2_mean <- Workfile %>% 
  group_by(year = year(DateTime), month = month(DateTime), 
           wday = wday(DateTime)) %>% 
  summarise(mean = mean(Sub_metering_2)) %>% 
  filter(year != 2006)
dim(DDSM2_mean)
head(DDSM2_mean)

# Convert dataframe to timeseries----
ts_DDSM2_mean <- ts(DDSM2_mean$mean, start=c(2007), frequency= 7 * 12)
str(DDSM2_mean)
autoplot(decompose(ts_DDSM2_mean))

# Prepare for forecasting----
FitDDSM2_mean <- stl(ts_DDSM2_mean, s.window = "periodic")

# Forecasting SM1 and autoplot ----
FC_DDSM2_mean <- forecast(FitDDSM2_mean, h= 7 + 7 * 12)
autoplot(FC_DDSM2_mean)
plot(FC_DDSM2_mean, main = "Forecast SM2-wday", 
     xlab = "Date", 
     ylab = "Amount", 
     lwd = 3)
summary(FC_DDSM2_mean)

# add predictions to data
temp_pred_SM2_mean <- as.vector(FC_DDSM2_mean$mean)
temp_pred_SM2_mean <- as_tibble(temp_pred_SM2_mean)

temp_df_SM2_mean <- as_tibble(seq(as.Date('2010-12-1'),as.Date('2011-12-31'),by = 1))

temp_df_SM2_mean <- temp_df_SM2_mean %>% 
  group_by(year = year(value), month = month(value), wday = wday(value)) %>% 
  count()

temp_df_SM2_mean$pred <- temp_pred_SM2_mean$value
temp_df_SM2_mean$n <- NULL

# VIOLIN PLOT----
Violin_2_mean <- New_Total_SM2_mean %>% 
  bind_rows(temp_df_SM2_mean) %>% 
  ggplot(aes(x = wday, group = wday)) +
  geom_violin(aes(y = mean), fill = "orange", alpha = 0.6) + 
  geom_violin(aes(y = pred), fill = "yellow", alpha = 0.6)
print(Violin_2_mean)

## SM3----
# Subset Sub Meter 3 and create timeseries df----
DDSM3_mean <- Workfile %>% 
  group_by(year = year(DateTime), month = month(DateTime), 
           wday = wday(DateTime)) %>% 
  summarise(mean = mean(Sub_metering_3)) %>% 
  filter(year != 2006)
dim(DDSM3_mean)
head(DDSM3_mean)

# Convert dataframe to timeseries----
ts_DDSM3_mean <- ts(DDSM3_mean$mean, start=c(2007), frequency= 7 * 12)
str(DDSM3_mean)
autoplot(decompose(ts_DDSM3_mean))

# Prepare for forecasting----
FitDDSM3_mean <- stl(ts_DDSM3_mean, s.window = "periodic")

# Forecasting SM1 and autoplot ----
FC_DDSM3_mean <- forecast(FitDDSM3_mean, h= 7 + 7 * 12)
autoplot(FC_DDSM3_mean)
plot(FC_DDSM3_mean, main = "Forecast SM3-wday", 
     xlab = "Date", 
     ylab = "Amount", 
     lwd = 3)
summary(FC_DDSM3)

# add predictions to data
temp_pred_SM3_mean <- as.vector(FC_DDSM3_mean$mean)
temp_pred_SM3_mean <- as_tibble(temp_pred_SM3_mean)

temp_df_SM3_mean <- as_tibble(seq(as.Date('2010-12-1'),as.Date('2011-12-31'),by = 1))

temp_df_SM3_mean <- temp_df_SM3_mean %>% 
  group_by(year = year(value), month = month(value), wday = wday(value)) %>% 
  count()

temp_df_SM3_mean$pred <- temp_pred_SM3_mean$value
temp_df_SM3_mean$n <- NULL

# VIOLIN PLOT----
Violin_3_mean <- New_Total_SM3_mean %>% 
  bind_rows(temp_df_SM3_mean) %>% 
  ggplot(aes(x = wday, group = wday)) +
  geom_violin(aes(y = mean), fill = "green", alpha = 0.6) + 
  geom_violin(aes(y = pred), fill = "turquoise", alpha = 0.6)
print(Violin_3_mean)

#### YOY SEASON PLOTS ####

## SM1----
New_Total_SM1_mean <-  DDSM1_mean %>% 
  bind_rows(temp_df_SM1_mean)
New_Total_SM1_mean
ts_New_Total_SM1_mean <- ts(New_Total_SM1_mean$mean,
                            New_Total_SM1_mean$pred,
                            start=c(2007, 1), 
                            end=c(2011, 12),
                            frequency= 7)

SeasonplotSM1_mean <- ts_New_Total_SM1_mean %>% 
  ggseasonplot(
    ts_New_Total_SM1_mean, 
    year.labels=TRUE, 
    year.labels.left=TRUE) +
  geom_line(size=1) +
  geom_point(size=3) +
  ylab("Usage SM1_total") +
  ggtitle("Weekday YoY trend & predictions KITCHEN")

plot(SeasonplotSM1_mean, 
     main = "YoY SubMeter1", 
     xlab = "Date", 
     ylab = "Amount", 
     lwd = 3)

## SM2----
New_Total_SM2_mean <-  DDSM2_mean %>% 
  bind_rows(temp_df_SM2_mean)
New_Total_SM2_mean
ts_New_Total_SM2_mean <- ts(New_Total_SM2_mean$mean,
                            New_Total_SM2_mean$pred,
                            start=c(2007, 1), 
                            end=c(2011, 12),
                            frequency= 7)

SeasonplotSM2_mean <- ts_New_Total_SM2_mean %>% 
  ggseasonplot(
    ts_New_Total_SM2_mean, 
    year.labels=TRUE, 
    year.labels.left=TRUE) +
  geom_line(size=1) +
  geom_point(size=3) +
  ylab("Usage SM2") +
  ggtitle("Weekday YoY trend & predictions LAUNDRY")

plot(SeasonplotSM2_mean, 
     main = "YoY SM2", 
     xlab = "Date", 
     ylab = "Amount", 
     lwd = 3)

## SM3----
New_Total_SM3_mean <-  DDSM3_mean %>% 
  bind_rows(temp_df_SM3_mean)
New_Total_SM3_mean
ts_New_Total_SM3_mean <- ts(New_Total_SM3_mean$mean,
                            New_Total_SM3_mean$pred,
                            start=c(2007, 1), 
                            end=c(2011, 12),
                            frequency= 7)

SeasonplotSM3_mean <- ts_New_Total_SM3_mean %>% 
  ggseasonplot(
    ts_New_Total_SM3_mean, 
    year.labels=TRUE, 
    year.labels.left=TRUE) +
  geom_line(size=1) +
  geom_point(size=3) +
  ylab("Usage SM3") +
  ggtitle("Weekday YoY trend & predictions CLIMATE CONTROL")

plot(SeasonplotSM3_mean, 
     main = "YoY SM3", 
     xlab = "Date", 
     ylab = "Amount", 
     lwd = 3)
