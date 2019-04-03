### ---------------------- Wifi Locationing ---------------------- ###
### --------------------- by Alican Tanaçan ---------------------- ###
### ----- Version 4: Improving Modelization and Performance ------ ###

## In this version, we are trying to find the best model that accurately 
# finds the location of a user/phone by the preprocessed WAPS and related
# variables that we picked in the previous version. In order to find the 
# highest performed model, we have to adjust signal quality and change 
# variables constantly as well as the iteration of models.
# With signal quality is set to means of waps equal and bigger than 99.8.

### ---- Libraries ----
library(dplyr)
library(tidyverse)
library(caret)
library(lubridate)
library(naniar)
library(devtools)
library(ggplot2)
library(plotly)

### ---- Import the Preprocessed Training and Validation Data ----
CleanWifiTrainData <- readRDS(file = "PreprocessedWifiTraining.rds")

CleanWifiValidData <- readRDS(file = "PreprocessedWifiValidation.rds")

### ---- Subsetting & Sampling ----
set.seed(1020)
## Building ID
CleanWifiTrainData %>%
  select(starts_with("WAP"), BUILDINGID) -> TrainBuilding

SampleTrainBuilding <- TrainBuilding[sample(1:nrow(TrainBuilding), 1000, replace = F),]

SampleTrainBuilding$BUILDINGID <- as.factor(SampleTrainBuilding$BUILDINGID)

CleanWifiValidData %>%
  select(starts_with("WAP"), BUILDINGID) -> ValidBuilding

ValidBuilding$BUILDINGID <- as.factor(ValidBuilding$BUILDINGID)

## Longitude
CleanWifiTrainData %>%
  select(starts_with("WAP"), LONGITUDE) -> TrainLong

SampleTrainLong <- TrainLong[sample(1:nrow(TrainLong), 1000, replace = F),]

CleanWifiValidData %>%
  select(starts_with("WAP"), LONGITUDE) -> ValidLong

## Latitude
CleanWifiTrainData %>%
  select(starts_with("WAP"), LATITUDE) -> TrainLat

SampleTrainLat <- TrainLat[sample(1:nrow(TrainLat), 1000, replace = F),]

CleanWifiValidData %>%
  select(starts_with("WAP"), LATITUDE) -> ValidLat

## Floor
CleanWifiTrainData %>%
  select(starts_with("WAP"), FLOOR) -> TrainFloor

SampleTrainFloor <- TrainFloor[sample(1:nrow(TrainFloor), 1000, replace = F),]

SampleTrainFloor$FLOOR <- as.factor(SampleTrainFloor$FLOOR)

CleanWifiValidData %>%
  select(starts_with("WAP"), FLOOR) -> ValidFloor

ValidFloor$FLOOR <- as.factor(ValidFloor$FLOOR)

### ---- Random Forest Modelization ----
set.seed(2030)
## Building ID
RFGrid <- expand.grid(mtry=c(1:5))
RFTrainBuilding <- train(BUILDINGID ~ ., 
                         SampleTrainBuilding,
                         method = "rf",
                         trControl = trainControl(method = "repeatedcv",
                                                  number = 5,
                                                  repeats = 2),
                         tuneGrid = RFGrid,
                         tuneLenght = 2,
                         preProcess = "zv")

predRFTrainBuilding <- predict(RFTrainBuilding, ValidBuilding)

postResample(predRFTrainBuilding, ValidBuilding$BUILDINGID) -> RFTrainBuildingMetrics
RFTrainBuildingMetrics
# Accuracy: 0.999
# Kappa: 0.998

## Longitude
RFTrainLong <- train(LONGITUDE ~ ., 
                 SampleTrainLong,
                 method = "rf",
                 trControl = trainControl(method = "repeatedcv",
                                          number = 5,
                                          repeats = 2),
                 tuneGrid = RFGrid,
                 tuneLenght = 2,
                 preProcess = "zv")

predRFTrainLong <- predict(RFTrainLong, ValidLong)

postResample(predRFTrainLong, ValidLong$LONGITUDE) -> RFTrainLongMetrics
RFTrainLongMetrics
# RMSE: 19.167
# Rsquared: 0.977

## Latitude
RFTrainLat <- train(LATITUDE ~ ., 
                    SampleTrainLat,
                    method = "rf",
                    trControl = trainControl(method = "repeatedcv",
                                             number = 5,
                                             repeats = 2),
                    tuneGrid = RFGrid,
                    tuneLenght = 2,
                    preProcess = "zv")

predRFTrainLat <- predict(RFTrainLat, ValidLat)

postResample(predRFTrainLat, ValidLat$LATITUDE) -> RFTrainLatMetrics
RFTrainLatMetrics
# RMSE: 16.545
# Rsquared: 0.951

## Floor
RFTrainFloor <- train(FLOOR ~ ., 
                      SampleTrainFloor,
                      method = "rf",
                      trControl = trainControl(method = "repeatedcv",
                                           number = 5,
                                           repeats = 2),
                      tuneGrid = RFGrid,
                      tuneLenght = 2,
                      preProcess = "zv")

predRFTrainFloor <- predict(RFTrainFloor, ValidFloor)

postResample(predRFTrainFloor, ValidFloor$FLOOR) -> RFTrainFloorMetrics
RFTrainFloorMetrics
# Accuracy: 0.686
# Kappa: 0.581

### ---- Error Check ----
# Creating data frames for random forest predictions
RFpreddata <- data.frame(rf.buildingid = predRFTrainBuilding,
                         rf.longitude = predRFTrainLong, 
                         rf.latitude = predRFTrainLat, 
                         rf.floor = predRFTrainFloor)

# Combining the data frames
RFerrorvaliddata <- cbind(RFpreddata, 
                          CleanWifiValidData$BUILDINGID,
                          CleanWifiValidData$LONGITUDE, 
                          CleanWifiValidData$LATITUDE,
                          CleanWifiValidData$FLOOR)

# Error Visualization
RFerrorvaliddata %>% 
  ggplot(aes(x = rf.longitude, y = rf.latitude)) +
  geom_point(aes(x = rf.longitude, 
                 y = rf.latitude), 
             color = "blue") +
  labs(title = "Random Forest Predicted Longitude vs Predicted Latitude") +
  ylab("Latitude") + 
  xlab("Longitude")

RFerrorvaliddata %>% 
  ggplot(aes(x = CleanWifiValidData$LONGITUDE, y = CleanWifiValidData$LATITUDE)) +
  geom_point(aes(x = CleanWifiValidData$LONGITUDE, 
                 y = CleanWifiValidData$LATITUDE), 
             color = "red") +
  geom_point(aes(x = rf.longitude, 
                 y = rf.latitude), 
             color = "blue") +
  geom_label(aes(x = -7400, y = 4864960, label = "Actual"), 
             color = "red", 
             size = 4) +
  geom_label(aes(x = -7400, y = 4864930, label = "Pred"), 
             color = "blue", 
             size = 4) +
  labs(title = "Actual Data on Validation vs Random Forest Predictions") + 
  ylab("Latitude") + 
  xlab("Longitude") + 
  theme_light()

## We see that we have many errors on locating the user. This might be because of 
## the means of WAPS. Maybe introducing best signal range (-30 dBm to -67 dBm) or,   
## eliminating columns with the variance close to zero would be more logical.
