#### Install Packages ####

#pacman::p_load(
if(!require("caret")){install.packages("caret", dependencies = c("Depends", "Suggests"))}
install.packages(c("tidyr", "dplyr", "devtools"))
devtools::install_github("garrettgman/DSR")
if(!require("magrittr")){install.packages("magrittr")}
if(!require("lubridate")){install.packages("lubridate")}
if(!require("RColorBrewer")){install.packages("RColorBrewer")}

install.packages("chron")
install.packages("bdvis")

if(!require("imputeTS")){install.packages("imputeTS", dependencies = c("Depends", "Suggests"))}
install.packages("uroot")

#### Load Data ####

library(caret)
library(tidyr)
library(dplyr)
library(magrittr)
library(lubridate)
library(readr)
library(RColorBrewer)
library(chron)
library(bdvis)
library(imputeTS)
library(fracdiff)
library(uroot)
source("https://raw.githubusercontent.com/iascchen/VisHealth/master/R/calendarHeat.R")

setwd("/Users/PROUD/Dropbox/ubiqum/ubiqum_Part_3/Task_3.1_Define_Data_Science_Process")
householdpower <- read_delim("household_power_consumption.txt", ";", 
                             escape_double = FALSE, trim_ws = TRUE)

head(householdpower)
View(householdpower)
summary(householdpower)
str(householdpower)

#### Format Data ####

## Combine 'Date' and 'Time' columns and change 'Date' type to Date
householdpower$Date_Time <- dmy_hms(paste(householdpower$Date, householdpower$Time))
householdpower$Date <- as.Date(householdpower$Date, "%d/%m/%Y")

## Move 'Date_Time' to the first column
householdpower <- householdpower[,c(ncol(householdpower), 1:(ncol(householdpower)-1))]

head(householdpower)
str(householdpower)
summary(householdpower)

## Convert 'Active' and 'Reactive' to watt-hour
#householdpower <- householdpower %>% 
#mutate(Global_active_consumption = ((householdpower$Global_active_power*1000)/60))

#householdpower <- householdpower %>% 
#mutate(Global_reactive_consumption = ((householdpower$Global_reactive_power*1000)/60))

## Convert 'Sub-meters' to kWh
householdpower <- householdpower %>%
  mutate(Sub_metering_1kWh = ((householdpower$Sub_metering_1/1000))) %>%
  mutate(Sub_metering_2kWh = ((householdpower$Sub_metering_2/1000))) %>%
  mutate(Sub_metering_3kWh = ((householdpower$Sub_metering_3/1000))) %>%
  mutate(Global_active_powerkWh = ((householdpower$Global_active_power/60))) %>%
  mutate(Global_reactive_powerkWh = ((householdpower$Global_reactive_power/60)))

## Energy consumed not measured by sub-meters
householdpower <- householdpower %>%
  mutate(Household_consumption_kWh = 
           householdpower$Global_active_powerkWh-
           householdpower$Sub_metering_1kWh-
           householdpower$Sub_metering_2kWh-
           householdpower$Sub_metering_3kWh)

## Create 'Month' and 'Year' columns
householdpower$Month <- month(householdpower$Date_Time)
householdpower <- householdpower[,c(ncol(householdpower), 1:(ncol(householdpower)-1))]

householdpower$Year <- year(householdpower$Date_Time)
householdpower <- householdpower[,c(ncol(householdpower), 1:(ncol(householdpower)-1))]

## Create 'Year_Month'
householdpower <- householdpower %>% 
  unite(Year_Month, Year, Month, sep="-", remove=FALSE)

#### 2006 info (December 16th to 31st) ####
hpc2006 <- householdpower %>%
  filter(Year=="2006") %>%
  select(Year, Month, Date_Time, Global_active_powerkWh, Global_reactive_powerkWh,
         Sub_metering_1kWh, Sub_metering_2kWh, Sub_metering_3kWh, Household_consumption_kWh)
head(hpc2006)

hpc2006[is.na(hpc2006)] <- 0

hpc2006_month <- hpc2006 %>% 
  select(Year, Month, Date_Time, Global_active_powerkWh, Global_reactive_powerkWh,
         Sub_metering_1kWh, Sub_metering_2kWh, Sub_metering_3kWh, Household_consumption_kWh) %>%
  group_by(Month) %>%
  summarise_at(vars(Global_active_powerkWh, Global_reactive_powerkWh,
                    Sub_metering_1kWh, Sub_metering_2kWh, Sub_metering_3kWh, Household_consumption_kWh),
               funs(sum))
head(hpc2006_month)

#### 2007 info ####
hpc2007 <- householdpower %>%
  filter(Year=="2007") %>%
  select(Year, Month, Date_Time, Global_active_powerkWh, Global_reactive_powerkWh, 
         Sub_metering_1kWh, Sub_metering_2kWh, Sub_metering_3kWh, Household_consumption_kWh)
head(hpc2007)

hpc2007[is.na(hpc2007)] <- 0

hpc2007_month <- hpc2007 %>% 
  select(Year, Month, Date_Time, Global_active_powerkWh, Global_reactive_powerkWh, 
         Sub_metering_1kWh, Sub_metering_2kWh, Sub_metering_3kWh, Household_consumption_kWh) %>%
  group_by(Year, Month) %>%
  summarise_at(vars(Global_active_powerkWh, Global_reactive_powerkWh, 
                    Sub_metering_1kWh, Sub_metering_2kWh, Sub_metering_3kWh, Household_consumption_kWh),
               funs(sum))
head(hpc2007_month)

## Create 'Sub-meters' vs 'Time' plot for 2007
plot(x = hpc2007_month$Month, y = hpc2007_month$Sub_metering_1kWh, type = "n", 
     xlab = "", ylab = "Energy Sub Metering")
lines(x = hpc2007_month$Month, y = hpc2007_month$Sub_metering_1kWh)
lines(x = hpc2007_month$Month, y = hpc2007_month$Sub_metering_2kWh, col = "red")
lines(x = hpc2007_month$Month, y = hpc2007_month$Sub_metering_3kWh, col = "blue")
legend("topright", legend = c("Kitchen", "Laundry", "Heating_Cooling"), 
       lty = 1, col = c("black", "red", "blue"))

#### 2008 info ####
hpc2008 <- householdpower %>%
  filter(Year=="2008") %>%
  select(Year, Month, Date_Time, Global_active_powerkWh, Global_reactive_powerkWh, 
         Sub_metering_1kWh, Sub_metering_2kWh, Sub_metering_3kWh, Household_consumption_kWh)
head(hpc2008)

hpc2008[is.na(hpc2008)] <- 0

hpc2008_month <- hpc2008 %>% 
  select(Year, Month, Date_Time, Global_active_powerkWh, Global_reactive_powerkWh, 
         Sub_metering_1kWh, Sub_metering_2kWh, Sub_metering_3kWh, Household_consumption_kWh) %>%
  group_by(Year, Month) %>%
  summarise_at(vars(Global_active_powerkWh, Global_reactive_powerkWh, 
                    Sub_metering_1kWh, Sub_metering_2kWh, Sub_metering_3kWh, Household_consumption_kWh),
               funs(sum))
head(hpc2008_month)

#### 2009 info ####
hpc2009 <- householdpower %>%
  filter(Year=="2009") %>%
  select(Year, Month, Date_Time, Global_active_powerkWh, Global_reactive_powerkWh,
         Sub_metering_1kWh, Sub_metering_2kWh, Sub_metering_3kWh, Household_consumption_kWh)
head(hpc2009)

hpc2009[is.na(hpc2009)] <- 0

hpc2009_month <- hpc2009 %>% 
  select(Year, Month, Date_Time, Global_active_powerkWh, Global_reactive_powerkWh,
         Sub_metering_1kWh, Sub_metering_2kWh, Sub_metering_3kWh, Household_consumption_kWh) %>%
  group_by(Month) %>%
  summarise_at(vars(Global_active_powerkWh, Global_reactive_powerkWh,
                    Sub_metering_1kWh, Sub_metering_2kWh, Sub_metering_3kWh, Household_consumption_kWh),
               funs(sum))
head(hpc2009_month)

#### 2010 info (until November 26th) ####
hpc2010 <- householdpower %>%
  filter(Year=="2010") %>%
  select(Year, Month, Date_Time, Global_active_powerkWh, Global_reactive_powerkWh,
         Sub_metering_1kWh, Sub_metering_2kWh, Sub_metering_3kWh, Household_consumption_kWh)
head(hpc2010)

hpc2010[is.na(hpc2010)] <- 0

hpc2010_month <- hpc2010 %>% 
  select(Year, Month, Date_Time, Global_active_powerkWh, Global_reactive_powerkWh,
         Sub_metering_1kWh, Sub_metering_2kWh, Sub_metering_3kWh, Household_consumption_kWh) %>%
  group_by(Month) %>%
  summarise_at(vars(Global_active_powerkWh, Global_reactive_powerkWh,
                    Sub_metering_1kWh, Sub_metering_2kWh, Sub_metering_3kWh, Household_consumption_kWh),
               funs(sum))
head(hpc2010_month)

#### Monthly for all years 2007-2010

householdpower_na <- householdpower

householdpower_na[is.na(householdpower_na)] <- 0

hpc_monthly <- householdpower_na %>%
  filter(Year!=2006) %>%
  select(Year, Month, Date_Time, Global_active_powerkWh, Global_reactive_powerkWh, 
         Sub_metering_1kWh, Sub_metering_2kWh, Sub_metering_3kWh, Household_consumption_kWh) %>%
  group_by(Year, Month) %>%
  summarise_at(vars(Global_active_powerkWh, Global_reactive_powerkWh, 
                    Sub_metering_1kWh, Sub_metering_2kWh, Sub_metering_3kWh, Household_consumption_kWh),
               funs(sum))
hpc_monthly <- transform(hpc_monthly, Month_Abb = month.abb[Month])

head(hpc_monthly)

#### Data Overview ####

## Heat maps
calendarHeat1 <- calendarHeat(householdpower$Date, householdpower$Global_active_powerkWh, 
                              varname="Global Active Power (kWh)",
                              color="r2b")
calendarHeat2 <- calendarHeat(householdpower$Date, householdpower$Global_reactive_powerkWh,
                              varname="Global Reactive Power (kWh)",
                              color="r2b")

## Monthly consumption summary by year
hpc_monthly$Month_Abb <- factor(hpc_monthly$Month_Abb, 
                                levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

ggplot(data = hpc_monthly, aes(x = Month_Abb, y = Global_active_powerkWh, group = Year, colour = Year)) +
  geom_line()+theme_bw() +
  geom_point()+facet_wrap(facets = Year ~ .)

ggplot(data = hpc_monthly, aes(x = Month_Abb, y = Global_active_powerkWh, group = Year, colour = Year)) +
  geom_line()+theme_bw() +
  geom_point()+facet_grid(facets = Year ~ ., margins = FALSE)

ggplot(data = hpc_monthly, aes(x = Month_Abb, y = Global_active_powerkWh, group = Year, colour = Year)) +
  geom_line()+theme_bw() +
  geom_point()+facet_grid(facets = Year ~ ., margins = FALSE)

ggplot(data = hpc_monthly, aes(x = Month_Abb, y = Global_reactive_powerkWh, group = Year, colour = Year)) +
  geom_line()+theme_bw() +
  geom_point()+facet_grid(facets = Year ~ ., margins = FALSE)

## Monthly consumption by year

hpc_monthly$Month_Abb <- factor(hpc_monthly$Month_Abb, 
                                levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

ggplot(data=hpc_monthly, aes(hpc_monthly$Month_Abb,group=1))+
  #geom_line(aes(y = hpc_monthly$Sub_metering_1kWh, color="Kitchen")) +
  #geom_line(aes(y = hpc_monthly$Sub_metering_2kWh, color="Laundry")) +
  #geom_line(aes(y = hpc_monthly$Sub_metering_3kWh, color="Heating/Cooling")) +
  geom_line(aes(y = hpc_monthly$Global_active_powerkWh, color="Active_Power_kWh")) +
  geom_line(aes(y = hpc_monthly$Global_reactive_powerkWh, color="Reactive_Power_kWh")) +
  xlab("Month") +
  ylab("kWh") +
  ggtitle("Global Active and Reactive Power by Month") +
  #scale_x_discrete(labels =  month.abb) +
  #scale_x_date(labels = date_format("%b"))+
  #theme(panel.background = element_rect(fill = rgb(248, 236, 212, maxColorValue = 255)))+
  #theme_bw()+
  scale_y_continuous(labels = function(x) format(x, scientific =FALSE)) +
  #scale_colour_manual(name='',
  #values=c('Active_Power_kWh'="#CC6666", # Kitchen',
  #'Reactive_Power_kWh'="blue"), #Laundry Room'="blue",
  #'Heater'="darkgreen",
  #guide='legend') +
  facet_wrap( ~ Year )
#facet_grid(facets = Year ~ ., margins = FALSE)

## Histograms
options(scipen = 999)
with(householdpower, hist(Global_active_power, main = "Global Active Power", 
                          xlab = "Global Active Power (kilowatts)", 
                          ylab = "Frequency", 
                          col = "lightsalmon"))

with(householdpower, hist(Global_reactive_power, main = "Global Reactive Power", 
                          xlab = "Global Reactive Power (kilowatts)", 
                          ylab = "Frequency", 
                          col = "mintcream"))

## Create 'Global Active Power' vs 'Time' plot
plot(x = householdpower$Date_Time, y = householdpower$Global_active_power, type = "l", 
     xlab = "", ylab = "Global Active Power (kilowatts)")

## Create 'Global Reactive Power' vs 'Time' plot
plot(x = householdpower$Date_Time, y = householdpower$Global_reactive_power, type = "l", 
     xlab = "", ylab = "Global Reactive Power (kilowatts)")

## Create 'Sub-meters' vs 'Time' plot
plot(x = householdpower$Date_Time, y = householdpower$Sub_metering_1, type = "n", 
     xlab = "", ylab = "Energy Sub Metering")
lines(x = householdpower$Date_Time, y = householdpower$Sub_metering_1)
lines(x = householdpower$Date_Time, y = householdpower$Sub_metering_2, col = "red")
lines(x = householdpower$Date_Time, y = householdpower$Sub_metering_3, col = "blue")
legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
       lty = 1, col = c("black", "red", "blue"))

#### Visualize Data ####

## Create a 'Weekday' column

householdpower$Weekday <- weekdays(as.Date(householdpower$Date_Time))

## Create a 'Quarter' column
householdpower$Quarter<- quarter(householdpower$Date_Time)
householdpower <- householdpower[,c(ncol(householdpower), 1:(ncol(householdpower)-1))]

#household$SeasonWNames <-""
#household$SeasonWNames[household$Season == "1"] <- "Winter"
#household$SeasonWNames[household$Season == "2"] <- "Spring"
#household$SeasonWNames[household$Season == "3"] <- "Summer"
#household$SeasonWNames[household$Season == "4"] <- "Autmn"

#### Treating NA's ####

## Visualize distribution of NA gapsizes
householdpower$Global_active_powerkWh %>% plotNA.distributionBar

#plotNA.gapsize(household$Global_active_powerkWh, limit = , byTotalNA = FALSE, legend = TRUE,
#col = c("indianred", "steelblue"),
#xlab = "Ranking of the different gap sizes", ylab = "Number",
#main = "Occurrence of gap sizes (NAs in a row)", cex.names = 0.7,
#horiz = FALSE, axes = TRUE, beside = TRUE, las = 1)

## Count the frequency of missing values
is.na(householdpower$Global_active_powerkWh)
sum(is.na(householdpower$Global_active_powerkWh))

#plotNA.distribution(householdpower$Global_active_powerkWh)
#plotNA.gapsize(householdpower)

#householdpower %>%
#rowwise %>%
#summarise(NA_per_row = sum(is.na(.)))

## New dataset for treating NA's
HPC <- householdpower %>% select(Quarter, Year_Month, Year, Month, Date_Time, Date, Time,
                                 Sub_metering_1kWh, Sub_metering_2kWh, Sub_metering_3kWh, Global_active_powerkWh,
                                 Global_reactive_powerkWh, Household_consumption_kWh, Weekday, Missing_reading)
head(HPC)

HPC$Missing_reading <- is.na(HPC)
HPC %>%
  mutate()



