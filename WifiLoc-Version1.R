### --------------------- Wifi Locationing --------------------- ###
### -------------------- by Alican Tanaçan --------------------- ###
### ----------- Version 1: Initial Data Exploration ------------ ###

### Libraries ----
library(dplyr)
library(tidyverse)
library(caret)
library(lubridate)
library(naniar)
library(ggplot2)

### Import Wifi Training and Validation Data ----
WifiTrainingData <- read.csv("WifitrainingData.csv")

WifiValidationData <- read.csv("WifivalidationData.csv")

## Inspect the Data Types
summary(WifiTrainingData[,520:529])
str(WifiTrainingData[,520:529])

summary(WifiValidationData[,520:529])
str(WifiValidationData[,520:529])

## What happens if we plot longitude to latitude?
plot(WifiTrainingData$LONGITUDE, WifiTrainingData$LATITUDE)
plot(WifiValidationData$LONGITUDE, WifiValidationData$LATITUDE)

### Preprocessing ----
## Change the Data Types
WifiTrainingData %>%
  mutate_at(c("FLOOR",
              "BUILDINGID",
              "SPACEID",
              "RELATIVEPOSITION",
              "USERID",
              "PHONEID"), as.factor) %>% 
  mutate_at(c("TIMESTAMP"), as.character) %>% 
  mutate_at(c("TIMESTAMP"), as.numeric) -> trainingdata

WifiValidationData %>%
  mutate_at(c("FLOOR",
              "BUILDINGID",
              "SPACEID",
              "RELATIVEPOSITION",
              "USERID",
              "PHONEID"), as.factor) %>% 
  mutate_at(c("TIMESTAMP"), as.character) %>% 
  mutate_at(c("TIMESTAMP"), as.numeric) -> validationdata

## Convert Unix Time
trainingdata$TIMESTAMP <- as.POSIXct((trainingdata$TIMESTAMP),
                                     origin="1970-01-01", 
                                     tz="GMT")

validationdata$TIMESTAMP <- as.POSIXct((validationdata$TIMESTAMP),
                                       origin="1970-01-01", 
                                       tz="GMT")

## Remove Unnecessary Variables
traindata <- subset(trainingdata, 
                    select = -c(USERID, 
                                PHONEID, 
                                TIMESTAMP))

validdata <- subset(validationdata, 
                    select = -c(USERID, 
                                PHONEID, 
                                TIMESTAMP))

### Subsetting ----
## Sampling Data by Building ID
traindata %>% filter(BUILDINGID == 0) -> Building0

traindata %>% filter(BUILDINGID == 1) -> Building1

traindata %>% filter(BUILDINGID == 2) -> Building2

## Modelization ----
## Creating Data Partition for Building "0"
set.seed(79)
intrain <- createDataPartition(y = Building0$LONGITUDE, 
                               p = 0.75, 
                               list = FALSE)
trainbuild0long <- Building0[intrain,]
testbuild0long <-Building0[-intrain,]

## Random Forest (Auto Grid) Mode
RFControl0Long <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
RFBuild0Long <- train(LONGITUDE ~ ., 
                      trainbuild0long,
                      method = "rf",
                      trControl = RFControl0Long,
                      tunelength = 2) # DO NOT RUN! takes too much effort and time.
RFBuild0Long

RFPred0Long <- predict(RFBuild0Long, testbuild0long)

RFPred0Long

postResample(RFPred0Long, testbuild0long$LONGITUDE)