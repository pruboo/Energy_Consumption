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
HHPC$Season <- ifelse(Seasons >= as_date(Spring) & Seasons < as_date(Summer), "Spring",
                      ifelse(Seasons >= Summer & Seasons < Autumn, "Summer",
                             ifelse(Seasons >= Autumn & Seasons < Winter, "Autumn",
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

# recalculate 'Unregistered' #
HHPC <- HHPC %>%
  mutate(Unregistered = Active-Kitchen-Laundry-WHAC)

# fix negative values #
HHPC$Unregistered <- ifelse(HHPC$Unregistered<0, 0, HHPC$Unregistered)

# recalculate 'Efficiency' #
HHPC <- HHPC %>%
  mutate(Efficiency = Active/(Voltage*Intensity))

#### 4. Data Preparation for Predictions ####

HHPC_ts <- HHPC %>%
  mutate(Hour = hour(Date_Time), 
         Day = day(Date_Time), 
         Month = month(Date_Time), 
         Year = year(Date_Time)) %>%
  dplyr::select(Date_Time, Date, Year, Month, Day, Weekday, Season, Hour, everything())

vars <- c("Active", "Reactive", "Voltage", "Intensity", "Kitchen", "Laundry", "WHAC", "Unregistered")

## 4.1 Data by Hour
HHPC_hour <- HHPC_ts %>%
  group_by(Date, Year, Month, Hour) %>%
  summarise_at(vars(vars), funs(sum, mean)) %>%
  dplyr::select(Date, Year, Month, Time = Hour, Active = Active_sum, Reactive = Reactive_mean, 
                Voltage = Voltage_mean, Intensity = Intensity_mean, 
                Kitchen = Kitchen_sum, Laundry = Laundry_sum, WHAC = WHAC_sum)

## 4.2 Data by Day
## 4.3 Data by Week
## 4.4 Data by Month
## 4.5 Data by Season
## 4.6 Data by Year

# N #
if(require("Forecast")=="FALSE"){
  install.packages("Forecast")
}
library(forecast)

HPC_TS <- ts(HPC_monthly, frequency=12, start=c(2007,1))
HPC_TS
plot(HPC_TS)

# P #
####TimeSeries####

install.packages("forecast")
library("forecast")
####MONTH####
HPCMONTHLY <- ts(HPC_my$Global_active_powerKWh,frequency = 12, start=c(2007,1),end=c(2010,12))
HPCMONTHLY
plot(HPCMONTHLY)
DF_TSLM<- data.frame(Global_active_powerKWh = HPCMONTHLY, as.numeric(time(HPCMONTHLY)))
names(DF_TSLM)<-c("Global_active_powerKWh","time")
####MODELS####
####tslm MONTH####
TSLM <- tslm(Global_active_powerKWh~season + trend,DF_TSLM)
FC_TSLM <- forecast(TSLM, h=36)
autoplot(FC_TSLM)
summary(FC_TSLM)


