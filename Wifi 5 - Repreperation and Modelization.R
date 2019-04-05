### ---------------------- Wifi Locationing ---------------------- ###
### --------------------- by Alican Tanaçan ---------------------- ###
### ---- Version 5: Re-preparation of Data and Model Building ---- ###

## In this version, we will try to define range of values for good 
# and bad signals. Later we will try to take mean and variance to 
# eliminate poor signal WAPS; then we will rerun the models.

### ---- Libraries ----
library(dplyr)
library(tidyverse)
library(caret)
library(lubridate)
library(naniar)
library(devtools)
library(ggplot2)
library(plotly)

### ---- Working Directory ----
setwd("D:/RStudio/R Studio Working Directory")

### ---- Import Wifi Training and Validation Data ----
WifiTrainingData <- read.csv("WifitrainingData.csv")

WifiValidationData <- read.csv("WifivalidationData.csv")

### ---- Preprocessing ----
## Create a data frame for WAPS and rest of the VARIABLES
WAPStrain <- WifiTrainingData[,1:520]
wapstry <- WifiTrainingData[,1:520]

VARIABLEStrain <- WifiTrainingData[, 521:529]

WAPSvalid <- WifiValidationData[,1:520]

VARIABLESvalid <- WifiValidationData[, 521:529]

## Revaluation of WAPS with poor signal
# Train
WAPStrain[WAPStrain > -30] <- -105
WAPStrain[WAPStrain < -90] <- -105

wapsdata <- data.frame(variance = apply(WAPStrain, 2, var),
                       mean = apply(WAPStrain, 2, mean),
                       median = apply(WAPStrain, 2, median))
summary(wapsdata)

wapstry <- data.frame(variance = apply(wapstry, 2, var),
                      mean = apply(wapstry, 2, mean),
                      median = apply(wapstry, 2, median))

PoorWAPStrainCol <- apply(WAPStrain, 2, var) <= 0.05

PoorWAPStrainRow <- apply(WAPStrain, 1, var) <= 0.05

GoodWAPStrain <- WAPStrain[!PoorWAPStrainRow,
                           !PoorWAPStrainCol]

# Validation
WAPSvalid[WAPSvalid > -30] <- -105
WAPSvalid[WAPSvalid < -90] <- -105

PoorWAPSvalidCol <- apply(WAPSvalid, 2, var) <= 0.05

PoorWAPSvalidRow <- apply(WAPSvalid, 1, var) <= 0.05

GoodWAPSvalid <- WAPSvalid[!PoorWAPSvalidRow,
                           !PoorWAPSvalidCol]

## Equalizing the column amount in both WAPS data sets
WAPStrain2 <- GoodWAPStrain[, which(colnames(GoodWAPStrain) %in%
                                      colnames(GoodWAPSvalid))]

WAPSvalid2 <- GoodWAPSvalid[, which(colnames(GoodWAPSvalid) %in% 
                                      colnames(GoodWAPStrain))]

## Equalizing the row amount in both VARIABLE data sets
VARIABLEStrain2 <- VARIABLEStrain[!PoorWAPStrainRow, ]

VARIABLESvalid2 <- VARIABLESvalid[!PoorWAPSvalidRow, ]

## Combining the WAPS and VARIABLES data
AdjTrainingData <- cbind(WAPStrain2, VARIABLEStrain2)

AdjValidationData <- cbind(WAPSvalid2, VARIABLESvalid2)

### ---- Subsetting & Sampling ----
set.seed(1020)
## Longitude
AdjTrainingData %>%
  select(starts_with("WAP"), LONGITUDE) -> TrainLong

SampleTrainLong <- TrainLong[sample(1:nrow(TrainLong), 500, replace = F),]

AdjValidationData %>%
  select(starts_with("WAP"), LONGITUDE) -> ValidLong

## Latitude
AdjTrainingData %>%
  select(starts_with("WAP"), LATITUDE) -> TrainLat

SampleTrainLat <- TrainLat[sample(1:nrow(TrainLat), 500, replace = F),]

AdjValidationData %>%
  select(starts_with("WAP"), LATITUDE) -> ValidLat

## Floor
AdjTrainingData %>%
  select(starts_with("WAP"), FLOOR) -> TrainFloor

SampleTrainFloor <- TrainFloor[sample(1:nrow(TrainFloor), 500, replace = F),]

SampleTrainFloor$FLOOR <- as.factor(SampleTrainFloor$FLOOR)

AdjValidationData %>%
  select(starts_with("WAP"), FLOOR) -> ValidFloor

ValidFloor$FLOOR <- as.factor(ValidFloor$FLOOR)

### ---- Modelization ----
set.seed(2030)
## Longitude
RFGrid <- expand.grid(mtry=c(1:5))
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
# RMSE: 27.490
# Rsquared: 0.959

## Latitude
kNNTrainLat <- train(LATITUDE ~ ., 
                     SampleTrainLat,
                     method = "knn",
                     trControl = trainControl(method = "cv",
                                            number = 5,
                                            verboseIter = TRUE),
                     preProcess = c("zv", "medianImpute"))

predkNNTrainLat <- predict(kNNTrainLat, ValidLat)

postResample(predkNNTrainLat, ValidLat$LATITUDE) -> kNNTrainLatMetrics

kNNTrainLatMetrics  
# RMSE: 16.522
# Rsquared: 0.946

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
# Accuracy: 0.785
# Kappa: 0.699

### ---- Error Check ----
# Creating data frames for random forest predictions
RFpreddata <- data.frame(rf.longitude = predRFTrainLong, 
                         rf.latitude = predRFTrainLat, 
                         rf.floor = predRFTrainFloor)

# Combining the data frames
RFerrorvaliddata <- cbind(RFpreddata, 
                          AdjValidationData$LONGITUDE, 
                          AdjValidationData$LATITUDE,
                          AdjValidationData$FLOOR)

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
  ggplot(aes(x = AdjValidationData$LONGITUDE, y = AdjValidationData$LATITUDE)) +
  geom_point(aes(x = AdjValidationData$LONGITUDE, 
                 y = AdjValidationData$LATITUDE), 
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

# We realize that we need different preprocesses and different models for each
# building, because they have different shapes, and there is not one model that
# predicts all buildings with the same accuracy.