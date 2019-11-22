library(readr)
library(mice)
library(tidyr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(caret)
library(ggplot2)

#load data----
household_power_consumption <- read_delim("household_power_consumption.txt", 
                                          delim = ";", escape_double = FALSE, 
                        col_types = cols(Date = col_date(format = "%d/%m/%Y"), 
                                        Time = col_time(format = "%H:%M:%S")),
                                          trim_ws = TRUE)
#Deal with NA's----
is.na(household_power_consumption)
sum(is.na(household_power_consumption))
Clean_df <- na.omit(household_power_consumption)
View(Clean_df)

#combine date & time----
# with dplyr and tidyr
Clean_df <- Clean_df %>% 
  unite(col = "DateTime", Date, Time, sep = " ", remove = F) %>% 
  mutate(DateTime = as_datetime(DateTime, tz="GMT"))
str(Clean_df)

#EDA ----
glimpse(Clean_df)
dim(Clean_df)
summarize(Clean_df)
mean(Clean_df$Sub_metering_1)
summary(Clean_df)
#Date               DateTime                       Time          Global_active_power Global_reactive_power    Voltage     
#Min.   :2006-12-16   Min.   :2006-12-16 17:24:00   Length:2049280    Min.   : 0.076      Min.   :0.0000        Min.   :223.2  
#1st Qu.:2007-12-10   1st Qu.:2007-12-10 05:37:45   Class1:hms        1st Qu.: 0.308      1st Qu.:0.0480        1st Qu.:239.0  
#Median :2008-11-30   Median :2008-11-30 01:22:30   Class2:difftime   Median : 0.602      Median :0.1000        Median :241.0  
#Mean   :2008-12-01   Mean   :2008-12-02 00:59:44   Mode  :numeric    Mean   : 1.092      Mean   :0.1237        Mean   :240.8  
#3rd Qu.:2009-11-23   3rd Qu.:2009-11-23 20:31:15                     3rd Qu.: 1.528      3rd Qu.:0.1940        3rd Qu.:242.9  
#Max.   :2010-11-26   Max.   :2010-11-26 21:02:00                     Max.   :11.122      Max.   :1.3900        Max.   :254.2  
#Global_intensity Sub_metering_1   Sub_metering_2   Sub_metering_3  
#Min.   : 0.200   Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
#1st Qu.: 1.400   1st Qu.: 0.000   1st Qu.: 0.000   1st Qu.: 0.000  
#Median : 2.600   Median : 0.000   Median : 0.000   Median : 1.000  
#Mean   : 4.628   Mean   : 1.122   Mean   : 1.299   Mean   : 6.458  
#3rd Qu.: 6.400   3rd Qu.: 0.000   3rd Qu.: 1.000   3rd Qu.:17.000  
#Max.   :48.400   Max.   :88.000   Max.   :80.000   Max.   :31.000 

#EDA conclusions----
#Sub_met_1 uses most AND least power in absolute figures/mean (88.000/1.122)
#min and 1st Quartile is not calculated or 0 does this mean there is min amount
#of use that needs to be exceeded (dont think so...) Why is it not calculated
#with Submeter 2 the 3rd Quartile is lower than the mean. I would expect outliers
#based on a max of 80.000 and a mean of .1299. Why is Median not filled. 
#We don't deal with normal distribution but with a lot of values around mean 
#(see sd and var) and the maximum values. 

#standard variation = de variantie berekenen vs standaard deviatie
var(Clean_df$Sub_metering_1) #37.86
var(Clean_df$Sub_metering_2) #33.90
var(Clean_df$Sub_metering_3) #71.19
sd(Clean_df$Sub_metering_1) #6.15
sd(Clean_df$Sub_metering_2) #5.82
sd(Clean_df$Sub_metering_3) #8.44

#DONT USE DumifyVars----
#dit is in dit databestand niet nodig
DummyVarsClean_df <- dummyVars(" ~ .", data = Clean_df)
DummyVarsClean_df <- data.frame(predict(DummyVarsClean_df, newdata = Clean_df))

#### SUB METERS ####
#Count per date, sum should be 1440 being the minutes in a day----
daily <- Clean_df %>% 
  group_by(Date) %>% 
  summarize(n = n()) %>% 
  filter(n != 1440)
daily #view results and see that ca. 75 rijen niet iedere minuut meetdata heeft

#Plotting submeters----
ggplot(daily, aes(Date, n)) +
    geom_line()
#dit moet dan een rechte lijn geven omdat er iedere minuut een meetwaarde is (of 0)
#plot laat duidelijk zien dat er vooral in 2007 en na 20009 meetwaarden missen = 0

#Plot submeters per weekday----
#add weekdays----
weekday <- weekday %>% 
  mutate(wday = wday(Date, label = TRUE))

#first create a wday object to work with based on Date attribute
daily <- Clean_df %>% 
  mutate(wday = wday(Date, label = TRUE))

#create frame with the three submeters for plotting
daily <- daily %>% 
  group_by(Date) %>% 
  summarize(Sub_metering_1 = mean(Sub_metering_1), 
            Sub_metering_2 = mean(Sub_metering_2), 
            Sub_metering_3 = mean(Sub_metering_3)) %>% 
            mutate(wday = wday(Date, label = TRUE))
daily 

ggplot(daily, aes(wday, Sub_metering_1)) + geom_boxplot(col="Red", fill="Red") + 
  geom_crossbar(stat="summary", fun.y=mean, fun.ymax=mean, fun.ymin=mean, fatten=2, width=.5)
ggplot(daily, aes(wday, Sub_metering_2)) + geom_boxplot(col="Yellow", fill="Yellow") + 
  geom_crossbar(stat="summary", fun.y=mean, fun.ymax=mean, fun.ymin=mean, fatten=2, width=.5)
ggplot(daily, aes(wday, Sub_metering_3)) + geom_boxplot(col="blue", fill="blue") +   
  geom_crossbar(stat="summary", fun.y=mean, fun.ymax=mean, fun.ymin=mean, fatten=2, width=.5)


#Violin plot mean and standard deviation----
#Meter 1----
daily %>%
  group_by(wday) %>%
  summarise(gem = mean(Sub_metering_1), sd = sd(Sub_metering_1))

ggplot(daily, aes(x = wday, y = Sub_metering_1)) +
  geom_violin(aes(fill=wday, color=wday)) + 
  geom_crossbar(stat="summary", fun.y=mean, fun.ymax=mean, fun.ymin=mean, fatten=2, width=.5) +
  geom_point(color="black", size=1, position = position_jitter(w=0.05))

Violin_1 <- group_by(daily, Sub_metering_1) %>% 
  group_by(wday) %>% 
  summarize(mean = mean(Sub_metering_1), se =sd(Sub_metering_1)/sqrt(n()), 
            upper=mean+(2*se), lower=mean-(2*se))

scatter_violin_1 <- ggplot(data=Violin_1, aes(x=wday, y=se)) +
  geom_violin(aes(fill=wday, color="Orange")) +
  geom_crossbar(stat="summary", fun.y=mean, fun.ymax=mean, fun.ymin=mean, fatten=2, width=.5) +
  geom_point(color="black", size=1, position = position_jitter(w=0.05)) +
  theme_minimal()

print(scatter_violin_1)


#Meter 2----
daily %>%
  group_by(wday) %>%
  summarise(gem = mean(Sub_metering_2), sd = sd(Sub_metering_2))

ggplot(daily, aes(x = wday, y = Sub_metering_2)) +
  geom_violin(aes(fill=wday, color=wday)) + 
  geom_crossbar(stat="summary", fun.y=mean, fun.ymax=mean, fun.ymin=mean, fatten=2, width=.5) +
  geom_point(color="black", size=1, position = position_jitter(w=0.05))

Violin_2 <- group_by(daily, Sub_metering_2) %>% 
  group_by(wday) %>% 
  summarize(mean = mean(Sub_metering_2), se =sd(Sub_metering_2)/sqrt(n()), 
            upper=mean+(2*se), lower=mean-(2*se))

scatter_violin_2 <- ggplot(data=Violin_2, aes(x=wday, y=se)) +
  geom_violin(aes(fill=wday, color="Orange")) +
  geom_crossbar(stat="summary", fun.y=mean, fun.ymax=mean, fun.ymin=mean, fatten=2, width=.5) +
  geom_point(color="black", size=1, position = position_jitter(w=0.05)) +
  theme_minimal()

print(scatter_violin_2)

#Meter 3----
daily %>%
  group_by(wday) %>%
  summarise(gem = mean(Sub_metering_3), sd = sd(Sub_metering_3))

ggplot(daily, aes(x = wday, y = Sub_metering_3)) +
  geom_violin(aes(fill=wday, color=wday)) + 
  geom_crossbar(stat="summary", fun.y=mean, fun.ymax=mean, fun.ymin=mean, fatten=2, width=.5) +
  geom_point(color="black", size=1, position = position_jitter(w=0.05))

Violin_3 <- group_by(daily, Sub_metering_3) %>% 
  group_by(wday) %>% 
  summarize(mean = mean(Sub_metering_3), se =sd(Sub_metering_3)/sqrt(n()), 
            upper=mean+(2*se), lower=mean-(2*se))

scatter_violin_3 <- ggplot(data=Violin_3, aes(x=wday, y=se)) +
  geom_violin(aes(fill=wday, color="Orange")) +
  geom_crossbar(stat="summary", fun.y=mean, fun.ymax=mean, fun.ymin=mean, fatten=2, width=.5) +
  geom_point(color="black", size=1, position = position_jitter(w=0.05)) +
  theme_minimal()

print(scatter_violin_3)

#Boxplot of submeters----

#determine on basis of what value you want to summarize the boxplot = mean
#and add the weekday to the object

#Boxplot for meter1----
weekday_meter_1 <- Clean_df %>% 
  group_by(Date) %>% 
  summarize(Sub_metering_1 = mean(Sub_metering_1)) %>% 
  mutate(wday = wday(Date, label = TRUE))
weekday_meter_1

ggplot(weekday_meter_1, aes(x = wday, y = Sub_metering_1)) + 
  geom_boxplot()
#Boxplot1 shows higher mean and distr for za/zo but not big diff with weekdays
#gues is that this is the kitchen

#Boxplot for meter2----
weekday_meter_2 <- Clean_df %>% 
  group_by(Date) %>% 
  summarize(Sub_metering_2 = mean(Sub_metering_2)) %>% 
  mutate(wday = wday(Date, label = TRUE))
weekday_meter_2

ggplot(weekday_meter_2, aes(x = wday, y = Sub_metering_2)) + 
  geom_boxplot()
#Boxplot2 shows lower mean than 1 esp on ma/do &di/vr
#with largest dist on zo/wo & di/za and highest usage on ma
#also lot of outliers for most days except zo

#Boxplot for meter3----
weekday_meter_3 <- Clean_df %>% 
  group_by(Date) %>% 
  summarize(Sub_metering_3 = mean(Sub_metering_3)) %>% 
  mutate(wday = wday(Date, label = TRUE))
weekday_meter_3

ggplot(weekday_meter_3, aes(x = wday, y = Sub_metering_3)) + 
  geom_boxplot()
#Boxplot3 has the highest mean, equal on almost all days with little larger
#distr in weekend and few outliers from zo to do. This must be heat/aircondit.

View(Clean_df)

#Submetering of Power months----

#first create a wday object to work with based on Date attribute
month <- Clean_df %>% 
  mutate(month = month(Date, label = TRUE))
View(month)
plot(month$month, month$Sub_metering_1, type="l", 
     main = "Sub-metering of Power Over Months", 
     ylab="Energy sub metering", xlab="")
lines(month$month, month$Sub_metering_1, col="yellow")
lines(month$month, month$Sub_metering_2, col="red")
lines(month$month, month$Sub_metering_3, col="blue")
legend("topright", col=c("yellow", "red", "blue"), lty=1, lwd=2, 
       legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

#### OTHER POWER ATTR ####
Clean_df

#Histogram Global Active Power----
hist(Clean_df$Global_active_power, main = paste("Global Active Power"), 
     col="red", xlab="Global Active Power (kilowatts)")


plot(Clean_df$Global_reactive_power,
     type='l',ylab="Global Active Power (Kilowatts)", xlab="")

plot(Clean_df$Global_active_power,
     type='l',ylab="Global Active Power (Kilowatts)", xlab="")

plot(Clean_df$Global_intensity,
     type='l',ylab="Global Intensity (Ampere)", xlab="")

#histogram Global Reactive Power----
hist(Clean_df$Global_reactive_power, main = paste("Global Re-active Power"), 
     col="green", xlab="Global Re-active Power (kilowatts)")

#histogram Voltage----
hist(Clean_df$Voltage, main = paste("Voltage (minute averaged)"), col="Blue", 
     xlab="Voltage(Volt)")

#histogram GLobal Insity----
hist(Clean_df$Global_intensity, main = paste("Global Intensity (Ampere)"), col="Yellow", 
     xlab="Ampere(Amp)")

#Linear Models----
LM1 <- lm(Global_active_power ~ Sub_metering_3, data = Clean_df)
plot(LM1)
summary(LM1)
#Call:
 # lm(formula = Global_active_power ~ Sub_metering_3, data = Clean_df)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-1.1494 -0.4108 -0.2708  0.0312  9.5872 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    5.748e-01  7.158e-04     803   <2e-16 ***
#  Sub_metering_3 8.002e-02  6.737e-05    1188   <2e-16 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.8137 on 2049278 degrees of freedom
#Multiple R-squared:  0.4078,	Adjusted R-squared:  0.4078 
#F-statistic: 1.411e+06 on 1 and 2049278 DF,  p-value: < 2.2e-16

#### QUESTIONS ####

#1 Do household electric appliances have a specific “load signature” 
#that can be identified in the various consumption curves?

#2 How is the trend if you compare year to year usage per month

#3 predicting future use based on trend ytd versus last year + 

#4 Show usage per time of the day and compare with previous week to show trend

#convert to KWh or the other way around and why would you compare individual
#submeters with global active or Volt*Ampere is Global ACtive. And what about
#Re-active how does that contribute to Energy consumption and/or trends

    
#### Month_year plotting ####

#now need for years to compare year to year by month
temp <- month %>% mutate(year = year(Date))
View(temp)

# Filter July 2008 - All Observations----
temp
head(temp)

July08 <- temp %>% 
  filter(year == 2008, 
  month == "jul")

## Plot subset july 2008 by weekday
view(July08)
  
July08byday <- July08 %>% 
  group_by(Date) %>% 
  mutate(wday = wday(Date, label = TRUE))

ggplot(July08byday, aes(x = wday, y = Sub_metering_3)) + 
  geom_boxplot()

ggplot(July08byday, aes(x = wday, y = Sub_metering_3)) + 
  geom_violin()

# Filter July 2007 - All Observations----
temp
head(temp)

July07 <- temp %>% 
  filter(year == 2007, 
         month == "jul")
  
## Plot subset july 2007 by weekday
July07byday <- July07 %>% 
  group_by(Date) %>% 
  mutate(wday = wday(Date, label = TRUE))

ggplot(July07byday, aes(x = wday, y = Sub_metering_3)) + 
  geom_boxplot()

ggplot(July08byday, aes(x = wday, y = Sub_metering_3)) + 
  geom_violin()

#Filter July 2009 - All observations----
temp
view(temp)
July09 <- temp %>% 
  filter(year == 2009, 
         month == "jul")
  
## Plot subset July 2009 by weekday
July09byday <- July09 %>% 
  group_by(Date) %>% 
  mutate(wday = wday(Date, label = TRUE))

ggplot(July09byday, aes(x = wday, y = Sub_metering_3)) + 
  geom_boxplot()

ggplot(July09byday, aes(x = wday, y = Sub_metering_3)) + 
  geom_violin()

#Filter July 2010 - All observations----
July10 <- temp %>% 
  filter(year == 2010, 
         month == "jul")

#Plot subset July 2010 by weekday
July10byday <- July10 %>% 
  group_by(Date) %>% 
  mutate(wday = wday(Date, label = TRUE))

ggplot(July10byday, aes(x = wday, y = Sub_metering_3)) + 
  geom_boxplot()

ggplot(July10byday, aes(x = wday, y = Sub_metering_3)) + 
  geom_violin()

#Filter Dec 2006 - All observations----
Dec06 <- temp %>% 
  filter(year == 2006,
         month == "dec")

#Plot subset Dec 2006 by weekday
Dec06yday <- Dec06 %>% 
  group_by(Date) %>% 
  mutate(wday = wday(Date, label = TRUE))

ggplot(Dec06yday, aes(x = wday, y = Sub_metering_3)) + 
  geom_boxplot()

ggplot(Dec06yday, aes(x = wday, y = Sub_metering_3)) + 
  geom_violin()

#December shows total different usage than July for all years. Check dec years
temp
view(temp)

#Dec2007----
Dec07 <- temp %>% 
  filter(year == 2007, 
         month == "dec")

Dec07byday <- Dec07 %>% 
  group_by(Date) %>% 
  mutate(wday = wday(Date, label = TRUE))

ggplot(Dec07byday, aes(x = wday, y = Sub_metering_3)) + 
  geom_boxplot()

ggplot(Dec07byday, aes(x = wday, y = Sub_metering_3)) + 
  geom_violin()

#Dec2008----
Dec08 <- temp %>% 
  filter(year == 2008, 
         month == "dec")

Dec08byday <- Dec08 %>% 
  group_by(Date) %>% 
  mutate(wday = wday(Date, label = TRUE))

ggplot(Dec08byday, aes(x = wday, y = Sub_metering_3)) + 
  geom_boxplot()

ggplot(Dec08byday, aes(x = wday, y = Sub_metering_3)) + 
  geom_violin()

#Dec09----
Dec09 <- temp %>% 
  filter(year == 2009, 
         month == "dec")

Dec09byday <- Dec09 %>% 
  group_by(Date) %>% 
  mutate(wday = wday(Date, label = TRUE))

ggplot(Dec09byday, aes(x = wday, y = Sub_metering_3)) + 
  geom_boxplot()

ggplot(Dec09byday, aes(x = wday, y = Sub_metering_3)) + 
  geom_violin()

#Nov10----
Nov10 <- temp %>% 
  filter(year == 2010, 
         month == "nov")

Nov10byday <- Nov10 %>% 
  group_by(Date) %>% 
  mutate(wday = wday(Date, label = TRUE))

ggplot(Nov10byday, aes(x = wday, y = Sub_metering_3)) + 
  geom_boxplot()

ggplot(Nov10byday, aes(x = wday, y = Sub_metering_3)) + 
  geom_violin()

#Year by Year comparison
yoy <- daily %>% 
group_by(year = year(Date)) %>% 
summarize(Sub_metering_1 = mean(Sub_metering_1), 
          Sub_metering_2 = mean(Sub_metering_2), 
          Sub_metering_3 = mean(Sub_metering_3))
yoy
#Reshape the values with gather----
#data, than gather(a key that we call "key", a value that we call "value" for...)
yoy_reshaped <- yoy %>% gather(key = "key", value = "value", Sub_metering_1, Sub_metering_2, Sub_metering_3)

#now we can plot because we have reshaped the data in a new object 
ggplot(yoy_reshaped, aes(x = year, y = value, fill = key)) + geom_col(position = "dodge")

#Yoy trend meter1
ggplot(yoy, aes(x = year)) + geom_col(aes(y = Sub_metering_1), fill = "red")

#Yoy trend meter2
ggplot(yoy, aes(x = year)) + geom_col(aes(y = Sub_metering_2), fill = "yellow")

#Yoy trend meter3
ggplot(yoy, aes(x = year)) + geom_col(aes(y = Sub_metering_3), fill = "blue")
