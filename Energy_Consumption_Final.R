#### 0. Load Libraries and Import Dataset ####

if(require("pacman")=="FALSE"){
  install.packages("pacman")
}

pacman::p_load(plyr, dplyr, tidyr, readr, lubridate, ggplot2, reshape, forecast, zoo, tseries, chron)

setwd("/Users/PROUD/Dropbox/ubiqum/ubiqum_Part_3/Task_3.1_Define_Data_Science_Process")
HouseholdPower <- read_delim("household_power_consumption.txt", ";", 
                   escape_double = FALSE, trim_ws = TRUE)

#### 1. Preprocess Electrical Data ####

HHPC <- HouseholdPower

## 1.1 Rename Variables
HHPC <- HHPC %>%
  dplyr::rename(Active = Global_active_power, Reactive = Global_reactive_power,
                Intensity = Global_intensity, Kitchen = Sub_metering_1, 
                Laundry = Sub_metering_2, WHAC = Sub_metering_3)

## 1.2 Convert Units to kWh
HHPC <- HHPC %>%
  mutate(Active = Active/60,
         Reactive = Reactive/60,
         Kitchen = Kitchen/1000,
         Laundry = Laundry/1000, 
         WHAC = WHAC/1000)

## 1.3 Calculate 'Unregistered' Household Energy Usage 
HHPC <- HHPC %>%
  mutate(Unregistered=Active-Kitchen-Laundry-WHAC)
# fix negative numbers #

## 1.4 Calculate Efficiency
HHPC <- HHPC %>%
  mutate(Efficiency = Active/(Voltage*Intensity))

#### 2. Preprocess Time Data ####

## 2.1 Create 'Date_Time' and Change 'Date' to Date
HHPC$Date_Time <- dmy_hms(paste(HHPC$Date, HHPC$Time))
HHPC$Date <- as.Date(HHPC$Date, "%d/%m/%Y")
HHPC <- HHPC[,c(ncol(HHPC), 1:(ncol(HHPC)-1))]

## 2.2 Daylight Savings

# Create Start and End Dates for Daylight Savings
Daylight <- data.frame(Start = c("2007-3-25 02:00:00", "2008-3-30 02:00:00", 
                                 "2009-3-29 02:00:00", "2010-3-28 02:00:00"), 
                       End = c("2007-10-28 1:59:00", "2008-10-26 1:59:00", 
                               "2009-10-25 1:59:00", "2010-10-31 1:59:00"), 
                       stringsAsFactors = FALSE)

# Convert to DateTime
Daylight$Start <- as_datetime(Daylight$Start)
Daylight$End <- as_datetime(Daylight$End)

# Add Daylight Saving Hour to Dates in Data Frame
HHPC <- HHPC %>%
  mutate(Date_Time = ifelse(HHPC$Date_Time %in% unlist(Map(`:`,Daylight$Start, Daylight$End)), 
                           Date_Time +3600, Date_Time))

# Convert New Time to DateTime
HHPC$Date_Time <- as_datetime(HHPC$Date_Time, origin= "1970-01-01", tz="UTC")

## 2.3 Create Weekday
HHPC$Weekday <- weekdays(as.Date(HHPC$Date_Time))

## 2.4 Create Season
# winter solstice #
Winter <- as_date("2008-12-21")
# spring equinox #
Spring <- as_date("2008-03-20")
# summer solstice #
Summer <- as_date("2008-06-21")
# autumn equinox #
Autumn <- as_date("2008-09-22")

Seasons <- as_date(format(HHPC$Date_Time, "%2008-%m-%d"))

# Create a 'Season' Column
HHPC$Season <- ifelse(Seasons >= as_date(Autumn) & Seasons <= as_date(Winter), "Autumn",
                      ifelse(Seasons >= Spring & Seasons <= Summer, "Spring",
                             ifelse(Seasons >= Summer & Seasons < Autumn, "Summer",
                                    "Winter")))
HHPC$Season <- as.factor(HHPC$Season)

#### 3. Missing and Negative Values ####

## 3.1 Change Negative 'Unregistered' Values to 0
HHPC$Unregistered <- ifelse(HHPC$Unregistered<0, 0, HHPC$Unregistered)

## 3.2 Missing Data
summary(HHPC) #25,979 NA's

# count missing values by date #
missing_calendar <- HHPC
missing_calendar$d_miss <- ifelse(is.na(missing_calendar$Active), 1, 0)
missing_calendar <- missing_calendar %>% 
  group_by(Date) %>%
  dplyr::summarize(missing = sum(d_miss))

# missing data calendar heat map #
library("chron")
source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")
calendarHeat1 <- calendarHeat(HHPC$Date, HHPC$Active, 
                              varname="Global Active Power (kWh)",
                              color="r2b")

# replace missing values #
HHPC <- na.locf(HHPC, na.rm = FALSE, maxgap = 1440)

# count missing values by date after replacing NA's with 0 (maxgap=1,440) #
missing_calendar2 <- HHPC
missing_calendar2$d_miss <- ifelse(is.na(missing_calendar2$Active), 1, 0)
missing_calendar2 <- missing_calendar2 %>% 
  group_by(Date) %>%
  dplyr::summarize(missing = sum(d_miss))

# missing data calendar heat map 2 #
calendarHeat2 <- calendarHeat(HHPC$Date, HHPC$Active, 
                              varname="Global Active Power (kWh)",
                              color="r2b")

# replace the remaining NA's with 0 #
HHPC[is.na(HHPC)] <- 0

# recalculate 'Unregistered' #
HHPC <- HHPC %>%
  mutate(Unregistered = Active-Kitchen-Laundry-WHAC)

# fix negative values #
HHPC$Unregistered <- ifelse(HHPC$Unregistered<0, 0, HHPC$Unregistered)

# recalculate 'Efficiency' #
HHPC <- HHPC %>%
  mutate(Efficiency = Active/(Voltage*Intensity))

#### 4. Data Preparation for Predictions ####

# select variables #
vars <- c("Active", "Reactive", "Voltage", "Intensity", "Kitchen", "Laundry", "WHAC", "Unregistered")

# create a new dataset for time-series #
HHPC$Hour <- hour(HHPC$Date_Time)
HHPC$Day <- day(HHPC$Date_Time)
HHPC$Week <- week(HHPC$Date_Time)
HHPC$Month <- month(HHPC$Date_Time)
HHPC$Year <- year(HHPC$Date_Time)

HHPC_ts <- HHPC %>%
  select(Date_Time, Date, Year, Month, Day, Weekday, Season, Hour, Week, everything())

## 4.1 Data by Hour
HHPC_hour <- HHPC_ts %>%
  filter(Year != 2006) %>%
  group_by(Year, Month, Day, Hour) %>%
  summarise_at(vars(Active, Reactive, Voltage, Intensity, Kitchen, Laundry, WHAC, Unregistered), 
               funs(sum))

## 4.2 Data by Day
HHPC_day <- HHPC_ts %>%
  filter(Year != 2006) %>%
  group_by(Year, Month, Day) %>%
  summarise_at(vars(Active, Reactive, Voltage, Intensity, Kitchen, Laundry, WHAC, Unregistered), 
                 funs(sum))

## 4.3 Data by Week
HHPC_week <- HHPC_ts %>%
  filter(Year != 2006) %>%
  group_by(Year, Month, Week) %>%
  summarise_at(vars(Active, Reactive, Voltage, Intensity, Kitchen, Laundry, WHAC, Unregistered), 
               funs(sum))

## 4.4 Data by Month
HHPC_month <- HHPC_ts %>%
  filter(Year != 2006) %>%
  group_by(Year, Month) %>%
  summarise_at(vars(Active, Reactive, Voltage, Intensity, Kitchen, Laundry, WHAC, Unregistered), 
               funs(sum))

## 4.5 Data by Season
HHPC_season <- HHPC_ts %>%
  filter(Year != 2006) %>%
  group_by(Year, Month, Season) %>%
  summarise_at(vars(Active, Reactive, Voltage, Intensity, Kitchen, Laundry, WHAC, Unregistered), 
               funs(sum))

## 4.6 Data by Year
HHPC_year <- HHPC_ts %>%
  filter(Year != 2006) %>%
  group_by(Year) %>%
  summarise_at(vars(Active, Reactive, Voltage, Intensity, Kitchen, Laundry, WHAC, Unregistered), 
               funs(sum))

#### 5. Time-Series and Predictions ####

# download packages #
if(require("forecast")=="FALSE"){
  install.packages("forecast")
}
library("forecast")

# Monthly TS #

ts_month <- ts(HHPC_month$Active, frequency = 12, start = c(2007, 1), end = c(2010, 20))
ts_month
plot(ts_month)
plot(decompose(ts_month))

ts_monthCom <- decompose(ts_month)
ts_monthAdj <- ts_month - ts_monthCom$seasonal
plot(ts_monthAdj)

adf.test(ts_month)

ts_monthdf <- data.frame(Active = ts_month, as.numeric(time(ts_month)))
names(ts_monthdf) <- c("Active", "Time")

tsoutliers(ts_month)

# Weekly TS #

ts_week <- ts(HHPC_week$Active, frequency = 52, start= c(2007, 1), end = c(2010, 48))
ts_week
plot(ts_week)
plot(decompose(ts_week))

ts_weekCom <- decompose(ts_week)
ts_weekAdj <- ts_week - ts_weekCom$seasonal
plot(ts_weekAdj)

adf.test(ts_week)

ts_weekdf <- data.frame(Active = ts_week, as.numeric(time(ts_week)))
names(ts_monthdf) <- c("Active", "Time")

tsoutliers(ts_week)

## 5.1 Holt-Winters

# HW Month #
HW_month <- HoltWinters(x = ts_month)
HW_monthPred <- forecast(HW_month, h = 15)
autoplot(HW_monthPred)
summary(HW_monthPred)

# HW Week #
HW_week <- HoltWinters(x = ts_week, seasonal = "additive")
HW_weekPred <- forecast(HW_week, h = 50)
autoplot(HW_weekPred, ylim = c(0, 4000))
summary(HW_weekPred)

## 5.2 ARIMA

# ARIMA Month #
auto.arima(ts_month)

arima_month <- arima(ts_month, order = c(0,0,0), 
                     seasonal = list(order = c(1,1,0), period = 12))
arima_month.pre <- forecast(arima_month, h=20)
plot(arima_month.pre)
summary(arima_month.pre)

# ARIMA Week #
auto.arima(ts_week)
arima_week <- arima(ts_week, order = c(1, 0, 2),
                    seasonal = list(order = c(1, 0, 0), period = 52))
arima_weekPred <- forecast(arima_week, h = 20)
plot(arima_weekPred)

## 5.3 Naiïve

# Naiïve Month #
snaive_month <- snaive(ts_month, h = 20)
plot(snaive_month)
summary(snaive_month)

# Naiïve Week #
snaive_week <- snaive(ts_week, h = 20)
plot(snaive_week)
summary(snaive_week)
