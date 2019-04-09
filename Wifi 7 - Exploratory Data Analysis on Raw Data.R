### ---------------------- Wifi Locationing ---------------------- ###
### --------------------- by Alican Tanaçan ---------------------- ###
### -------- Version 7: Investigating Patterns on Raw Data ------- ###

### ---- Libraries ----
library(dplyr)
library(tidyverse)
library(caret)
library(lubridate)
library(naniar)
library(devtools)
library(ggplot2)
library(plotly)
library(corrplot)
library(GGally)
library(gridExtra)

### ---- Import Wifi Training and Validation Data ----
WifiTrainingData <- read.csv("WifitrainingData.csv")

WifiValidationData <- read.csv("WifivalidationData.csv")

### ---- Preprocessing ----
## Change the Timestamp Data Type
WifiTrainingData %>%
  mutate_at(c("TIMESTAMP"), as.character) %>% 
  mutate_at(c("TIMESTAMP"), as.numeric) -> WifiTrainingData

WifiValidationData %>% 
  mutate_at(c("TIMESTAMP"), as.character) %>% 
  mutate_at(c("TIMESTAMP"), as.numeric) -> WifiValidationData

## Convert Unix Time
WifiTrainingData$TIMESTAMP <- as.POSIXct((WifiTrainingData$TIMESTAMP),
                                         origin="1970-01-01", 
                                         tz="GMT")

WifiValidationData$TIMESTAMP <- as.POSIXct((WifiValidationData$TIMESTAMP),
                                           origin="1970-01-01", 
                                           tz="GMT")

### ---- Data Exploration & Visualization ----
## Variable Analysis
WAPStrain <- WifiTrainingData[,1:520]
WAPStrain[WAPStrain > -30] <- -105
WAPStrain[WAPStrain < -80] <- -105
WAPSrowmeans <- data.frame(rowMeans(WAPStrain, na.rm = FALSE, dims = 1))
WAPSwithTIME <- cbind(WAPSrowmeans, 
                      WifiTrainingData$USERID,
                      WifiTrainingData$PHONEID,
                      WifiTrainingData$BUILDINGID,
                      WifiTrainingData$FLOOR,
                      WifiTrainingData$TIMESTAMP)
colnames(WAPSwithTIME) <- c("signal", 
                            "user", 
                            "phone",
                            "building",
                            "floor",
                            "time")

WAPSwithTIME %>% 
  group_by(day = day(time), user, phone, building, floor) %>% 
  count(user) -> WAPSwithTIME2

WAPSwithTIME %>% 
  group_by(date = date(time), user, phone, building, floor) %>% 
  count(user) -> WAPSwithTIME3

write.csv(WAPSwithTIME3, file = "wifigroupedwithtime.csv")

WAPSwithTIME %>% 
  filter(day(time) == 30) %>% 
  ggplot(aes(time)) +
  geom_line(aes(y = signal)) +
  labs(title = "30 May 2013 - Signals by Time") -> plot30may

WAPSwithTIME %>% 
  filter(day(time) == 31) %>% 
  ggplot(aes(time)) +
  geom_line(aes(y = signal))  +
  labs(title = "31 May 2013 - Signals by Time") -> plot31may

WAPSwithTIME %>% 
  filter(day(time) == 4) %>% 
  ggplot(aes(time)) +
  geom_line(aes(y = signal)) +
  labs(title = "4 Jun 2013 - Signals by Time") -> plot4jun

WAPSwithTIME %>% 
  filter(day(time) == 10) %>% 
  ggplot(aes(time)) +
  geom_line(aes(y = signal)) +
  labs(title = "10 Jun 2013 - Signals by Time") -> plot10jun

WAPSwithTIME %>% 
  filter(day(time) == 12) %>% 
  ggplot(aes(time)) +
  geom_line(aes(y = signal)) +
  labs(title = "12 Jun 2013 - Signals by Time") -> plot12jun

WAPSwithTIME %>% 
  filter(day(time) == 20) %>% 
  ggplot(aes(time)) +
  geom_line(aes(y = signal)) +
  labs(title = "20 Jun 2013 - Signals by Time") -> plot20jun

grid.arrange(plot30may,
             plot31may,
             plot4jun,
             plot10jun,
             plot12jun,
             plot20jun)


## Floor Analysis
WifiTrainingData %>% 
  filter(BUILDINGID == 0) %>% 
  group_by(FLOOR) %>% 
  ggplot(aes(LONGITUDE, LATITUDE)) +
  geom_point(color = "brown") +
  facet_wrap(~FLOOR) +
  labs(title = "Building 0 by Floor")

WifiTrainingData %>% 
  filter(BUILDINGID == 1) %>% 
  group_by(FLOOR) %>% 
  ggplot(aes(LONGITUDE, LATITUDE)) +
  geom_point(color = "brown") +
  facet_wrap(~FLOOR) +
  labs(title = "Building 1 by Floor")

WifiTrainingData %>% 
  filter(BUILDINGID == 2) %>% 
  group_by(FLOOR) %>% 
  ggplot(aes(LONGITUDE, LATITUDE)) +
  geom_point(color = "brown") +
  facet_wrap(~FLOOR) +
  labs(title = "Building 2 by Floor")

## PhoneID Analysis
WifiTrainingData %>% 
  filter(BUILDINGID == 0, PHONEID == 13) %>%
  group_by(FLOOR) %>% 
  ggplot(aes(LONGITUDE, LATITUDE)) +
  geom_point(color = "brown") +
  facet_wrap(~FLOOR) +
  labs(title = "Building 0 PhoneID 13 by Floor")

WifiTrainingData %>% 
  filter(BUILDINGID == 0, PHONEID == 14) %>%
  group_by(FLOOR) %>% 
  ggplot(aes(LONGITUDE, LATITUDE)) +
  geom_point(color = "brown") +
  facet_wrap(~FLOOR) +
  labs(title = "Building 0 PhoneID 14 by Floor")
  
