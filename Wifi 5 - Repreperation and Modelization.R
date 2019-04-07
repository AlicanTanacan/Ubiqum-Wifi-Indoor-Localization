### ---------------------- Wifi Locationing ---------------------- ###
### --------------------- by Alican Tanaçan ---------------------- ###
### ---- Version 5: Re-preparation of Data and Model Building ---- ###

## In this version, we will try to define range of values for good 
# and bad signals. Later we will try to take mean and variance to 
# eliminate poor signal WAPS; then we will subset for each building and
# try different models with different preprocesses.

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

VARIABLEStrain <- WifiTrainingData[, 521:529]

WAPSvalid <- WifiValidationData[,1:520]

VARIABLESvalid <- WifiValidationData[, 521:529]

## Revaluation of WAPS with poor signal
## Train
WAPStrain[WAPStrain > -30] <- -105
# WAPStrain[WAPStrain < -70] <- -105

## Check interval of variance, mean and median of columns to eliminate 
wapsdata <- data.frame(variance = apply(WAPStrain, 2, var),
                       mean = apply(WAPStrain, 2, mean),
                       median = apply(WAPStrain, 2, median))
summary(wapsdata)

PoorWAPStrainCol <- apply(WAPStrain, 2, var) <= 0.03

PoorWAPStrainRow <- apply(WAPStrain, 1, var) <= 0.03

GoodWAPStrain <- WAPStrain[!PoorWAPStrainRow,
                           !PoorWAPStrainCol]

## Validation
WAPSvalid[WAPSvalid > -30] <- -105
# WAPSvalid[WAPSvalid < -70] <- -105

PoorWAPSvalidCol <- apply(WAPSvalid, 2, var) <= 0.03

PoorWAPSvalidRow <- apply(WAPSvalid, 1, var) <= 0.03

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
## Building 0 
## Longitude
AdjTrainingData %>%
  filter(BUILDINGID == 0) %>% 
  select(starts_with("WAP"), LONGITUDE) -> Train0Long

SampleTrain0Long <- Train0Long[sample(1:nrow(Train0Long), 1000, replace = F),]

AdjValidationData %>%
  filter(BUILDINGID == 0) %>% 
  select(starts_with("WAP"), LONGITUDE) -> Valid0Long

## Latitude
AdjTrainingData %>%
  filter(BUILDINGID == 0) %>% 
  select(starts_with("WAP"), LATITUDE) -> Train0Lat

SampleTrain0Lat <- Train0Lat[sample(1:nrow(Train0Lat), 1000, replace = F),]

AdjValidationData %>%
  filter(BUILDINGID == 0) %>% 
  select(starts_with("WAP"), LATITUDE) -> Valid0Lat

## Floor
AdjTrainingData %>%
  filter(BUILDINGID == 0) %>% 
  select(starts_with("WAP"), FLOOR) -> Train0Floor

SampleTrain0Floor <- Train0Floor[sample(1:nrow(Train0Floor), 1000, replace = F),]

SampleTrain0Floor$FLOOR <- as.factor(SampleTrain0Floor$FLOOR)

AdjValidationData %>%
  filter(BUILDINGID == 0) %>% 
  select(starts_with("WAP"), FLOOR) -> Valid0Floor

Valid0Floor$FLOOR <- as.factor(Valid0Floor$FLOOR)

## Building 1 
## Longitude
AdjTrainingData %>%
  filter(BUILDINGID == 1) %>% 
  select(starts_with("WAP"), LONGITUDE) -> Train1Long

SampleTrain1Long <- Train1Long[sample(1:nrow(Train1Long), 1000, replace = F),]

AdjValidationData %>%
  filter(BUILDINGID == 1) %>% 
  select(starts_with("WAP"), LONGITUDE) -> Valid1Long

## Latitude
AdjTrainingData %>%
  filter(BUILDINGID == 1) %>% 
  select(starts_with("WAP"), LATITUDE) -> Train1Lat

SampleTrain1Lat <- Train1Lat[sample(1:nrow(Train1Lat), 1000, replace = F),]

AdjValidationData %>%
  filter(BUILDINGID == 1) %>% 
  select(starts_with("WAP"), LATITUDE) -> Valid1Lat

## Floor
AdjTrainingData %>%
  filter(BUILDINGID == 1) %>% 
  select(starts_with("WAP"), FLOOR) -> Train1Floor

SampleTrain1Floor <- Train1Floor[sample(1:nrow(Train1Floor), 1000, replace = F),]

SampleTrain1Floor$FLOOR <- as.factor(SampleTrain1Floor$FLOOR)

AdjValidationData %>%
  filter(BUILDINGID == 1) %>% 
  select(starts_with("WAP"), FLOOR) -> Valid1Floor

Valid1Floor$FLOOR <- as.factor(Valid1Floor$FLOOR)

## Building 2 
## Longitude
AdjTrainingData %>%
  filter(BUILDINGID == 2) %>% 
  select(starts_with("WAP"), LONGITUDE) -> Train2Long

SampleTrain2Long <- Train2Long[sample(1:nrow(Train2Long), 1000, replace = F),]

AdjValidationData %>%
  filter(BUILDINGID == 2) %>% 
  select(starts_with("WAP"), LONGITUDE) -> Valid2Long

## Latitude
AdjTrainingData %>%
  filter(BUILDINGID == 2) %>% 
  select(starts_with("WAP"), LATITUDE) -> Train2Lat

SampleTrain2Lat <- Train2Lat[sample(1:nrow(Train2Lat), 1000, replace = F),]

AdjValidationData %>%
  filter(BUILDINGID == 2) %>% 
  select(starts_with("WAP"), LATITUDE) -> Valid2Lat

## Floor
AdjTrainingData %>%
  filter(BUILDINGID == 2) %>% 
  select(starts_with("WAP"), FLOOR) -> Train2Floor

SampleTrain2Floor <- Train2Floor[sample(1:nrow(Train2Floor), 1000, replace = F),]

SampleTrain2Floor$FLOOR <- as.factor(SampleTrain2Floor$FLOOR)

AdjValidationData %>%
  filter(BUILDINGID == 2) %>% 
  select(starts_with("WAP"), FLOOR) -> Valid2Floor

Valid2Floor$FLOOR <- as.factor(Valid2Floor$FLOOR)

### ---- kNN Modelization ----
set.seed(2040)
## kNN Train Control
kNNcontrol <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 2,
                           preProc = c("center", "scale", "range"))

## Building 0 Longitude
kNN0Long <- train(LONGITUDE ~ ., 
                  SampleTrain0Long,
                  method = "knn",
                  trControl = kNNcontrol,
                  preProcess = "zv")

predkNN0Long <- predict(kNN0Long, Valid0Long)

postResample(predkNN0Long, Valid0Long$LONGITUDE) -> kNN0LongMetrics
kNN0LongMetrics
# RMSE: 5.957
# Rsquared: 0.949
# MAE: 3.994

## Building 0 Latitude
kNN0Lat <- train(LATITUDE ~ ., 
                 SampleTrain0Lat,
                 method = "knn",
                 trControl = kNNcontrol,
                 preProcess = "zv")

predkNN0Lat <- predict(kNN0Lat, Valid0Lat)

postResample(predkNN0Lat, Valid0Lat$LATITUDE) -> kNN0LatMetrics
kNN0LatMetrics
# RMSE: 5.144
# Rsquared: 0.974
# MAE: 3.499

## Building 0 Floor
kNN0Floor <- train(FLOOR ~ ., 
                   SampleTrain0Floor,
                   method = "knn",
                   trControl = kNNcontrol,
                   preProcess = "zv")

predkNN0Floor <- predict(kNN0Floor, Valid0Floor)

postResample(predkNN0Floor, Valid0Floor$FLOOR) -> kNN0FloorMetrics
kNN0FloorMetrics
# Accuracy: 0.957
# Kappa: 0.939

## Building 1 Longitude
kNN1Long <- train(LONGITUDE ~ ., 
                  SampleTrain1Long,
                  method = "knn",
                  trControl = kNNcontrol,
                  preProcess = "zv")

predkNN1Long <- predict(kNN1Long, Valid1Long)

postResample(predkNN1Long, Valid1Long$LONGITUDE) -> kNN1LongMetrics
kNN1LongMetrics
# RMSE: 9.120
# Rsquared: 0.960
# MAE: 6.316

## Building 1 Latitude
kNN1Lat <- train(LATITUDE ~ ., 
                 SampleTrain1Lat,
                 method = "knn",
                 trControl = kNNcontrol,
                 preProcess = "zv")

predkNN1Lat <- predict(kNN1Lat, Valid1Lat)

postResample(predkNN1Lat, Valid1Lat$LATITUDE) -> kNN1LatMetrics
kNN1LatMetrics
# RMSE: 11.157
# Rsquared: 0.901
# MAE: 7.207

## Building 1 Floor
kNN1Floor <- train(FLOOR ~ ., 
                   SampleTrain1Floor,
                   method = "knn",
                   trControl = kNNcontrol,
                   preProcess = "zv")

predkNN1Floor <- predict(kNN1Floor, Valid1Floor)

postResample(predkNN1Floor, Valid1Floor$FLOOR) -> kNN1FloorMetrics
kNN1FloorMetrics
# Accuracy: 0.768
# Kappa: 0.669

## Building 2 Longitude
kNN2Long <- train(LONGITUDE ~ ., 
                  SampleTrain2Long,
                  method = "knn",
                  trControl = kNNcontrol,
                  preProcess = "zv")

predkNN2Long <- predict(kNN2Long, Valid2Long)

postResample(predkNN2Long, Valid2Long$LONGITUDE) -> kNN2LongMetrics
kNN2LongMetrics
# RMSE: 11.605
# Rsquared: 0.864
# MAE: 7.507

## Building 2 Latitude
kNN2Lat <- train(LATITUDE ~ ., 
                 SampleTrain2Lat,
                 method = "knn",
                 trControl = kNNcontrol,
                 preProcess = "zv")

predkNN2Lat <- predict(kNN2Lat, Valid2Lat)

postResample(predkNN2Lat, Valid2Lat$LATITUDE) -> kNN2LatMetrics
kNN2LatMetrics
# RMSE: 9.902
# Rsquared: 0.886
# MAE: 6.325

## Building 2 Floor
kNN2Floor <- train(FLOOR ~ ., 
                   SampleTrain2Floor,
                   method = "knn",
                   trControl = kNNcontrol,
                   preProcess = "zv")

predkNN2Floor <- predict(kNN2Floor, Valid2Floor)

postResample(predkNN2Floor, Valid2Floor$FLOOR) -> kNN2FloorMetrics
kNN2FloorMetrics
# Accuracy: 0.891
# Kappa: 0.852

### ---- SVM Modelization ----
set.seed(3060)
## SVM Train Control
SVMcontrol <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 2,
                           preProc = "center")

## Building 0 Longitude
SVM0Long <- train(LONGITUDE ~ ., 
                  SampleTrain0Long,
                  method = "svmLinear",
                  trControl = SVMcontrol,
                  tuneLenght = 2,
                  preProcess = "zv")

predSVM0Long <- predict(SVM0Long, Valid0Long)

postResample(predSVM0Long, Valid0Long$LONGITUDE) -> SVM0LongMetrics
SVM0LongMetrics
# RMSE: 12.097
# Rsquared: 0.789
# MAE: 9.017

## Building 0 Latitude
SVM0Lat <- train(LATITUDE ~ ., 
                 SampleTrain0Lat,
                 method = "svmLinear",
                 trControl = SVMcontrol,
                 tuneLenght = 2,
                 preProcess = "zv")

predSVM0Lat <- predict(SVM0Lat, Valid0Lat)

postResample(predSVM0Lat, Valid0Lat$LATITUDE) -> SVM0LatMetrics
SVM0LatMetrics
# RMSE: 13.379
# Rsquared: 0.834
# MAE: 9.991

## Building 0 Floor
SVM0Floor <- train(FLOOR ~ ., 
                   SampleTrain0Floor,
                   method = "svmLinear",
                   trControl = SVMcontrol,
                   tuneLenght = 2,
                   preProcess = "zv")

predSVM0Floor <- predict(SVM0Floor, Valid0Floor)

postResample(predSVM0Floor, Valid0Floor$FLOOR) -> SVM0FloorMetrics
SVM0FloorMetrics
# Accuracy: 0.940
# Kappa: 0.915

## Building 1 Longitude
SVM1Long <- train(LONGITUDE ~ ., 
                  SampleTrain1Long,
                  method = "svmLinear",
                  trControl = SVMcontrol,
                  tuneLenght = 2,
                  preProcess = "zv")

predSVM1Long <- predict(SVM1Long, Valid1Long)

postResample(predSVM1Long, Valid1Long$LONGITUDE) -> SVM1LongMetrics
SVM1LongMetrics
# RMSE: 25.124
# Rsquared: 0.770
# MAE: 18.811

## Building 1 Latitude
SVM1Lat <- train(LATITUDE ~ ., 
                 SampleTrain1Lat,
                 method = "svmLinear",
                 trControl = SVMcontrol,
                 tuneLenght = 2,
                 preProcess = "zv")

predSVM1Lat <- predict(SVM1Lat, Valid1Lat)

postResample(predSVM1Lat, Valid1Lat$LATITUDE) -> SVM1LatMetrics
SVM1LatMetrics
# RMSE: 19.508
# Rsquared: 0.718
# MAE: 14.472

## Building 1 Floor
SVM1Floor <- train(FLOOR ~ ., 
                   SampleTrain1Floor,
                   method = "svmLinear",
                   trControl = SVMcontrol,
                   tuneLenght = 2,
                   preProcess = "zv")

predSVM1Floor <- predict(SVM1Floor, Valid1Floor)

postResample(predSVM1Floor, Valid1Floor$FLOOR) -> SVM1FloorMetrics
SVM1FloorMetrics
# Accuracy: 0.785
# Kappa: 0.702

## Building 2 Longitude
SVM2Long <- train(LONGITUDE ~ ., 
                  SampleTrain2Long,
                  method = "svmLinear",
                  trControl = SVMcontrol,
                  tuneLenght = 2,
                  preProcess = "zv")

predSVM2Long <- predict(SVM2Long, Valid2Long)

postResample(predSVM2Long, Valid2Long$LONGITUDE) -> SVM2LongMetrics
SVM2LongMetrics
# RMSE: 18.631
# Rsquared: 0.658
# MAE: 13.657

## Building 2 Latitude
SVM2Lat <- train(LATITUDE ~ ., 
                 SampleTrain2Lat,
                 method = "svmLinear",
                 trControl = SVMcontrol,
                 tuneLenght = 2,
                 preProcess = "zv")

predSVM2Lat <- predict(SVM2Lat, Valid2Lat)

postResample(predSVM2Lat, Valid2Lat$LATITUDE) -> SVM2LatMetrics
SVM2LatMetrics
# RMSE: 17.231
# Rsquared: 0.685
# MAE: 12.929

## Building 2 Floor
SVM2Floor <- train(FLOOR ~ ., 
                   SampleTrain2Floor,
                   method = "svmLinear",
                   trControl = SVMcontrol,
                   tuneLenght = 2,
                   preProcess = "zv")

predSVM2Floor <- predict(SVM2Floor, Valid2Floor)

postResample(predSVM2Floor, Valid2Floor$FLOOR) -> SVM2FloorMetrics
SVM2FloorMetrics
# Accuracy: 0.910
# Kappa: 0.877

### ---- RF Modelization ----
set.seed(4080)
##↕ Building 0 Longitude
RFGrid <- expand.grid(mtry=27)
RF0Long <- train(LONGITUDE ~ ., 
                 SampleTrain0Long,
                 method = "rf",
                 trControl = trainControl(method = "repeatedcv",
                                          number = 5,
                                          repeats = 2),
                 tuneGrid = RFGrid,
                 tuneLenght = 2,
                 preProcess = "zv")

predRF0Long <- predict(RF0Long, Valid0Long)

postResample(predRF0Long, Valid0Long$LONGITUDE) -> RF0LongMetrics
RF0LongMetrics
# RMSE: 7.965
# Rsquared: 0.916
# MAE: 5.567

## Building 0 Latitude
RF0Lat <- train(LATITUDE ~ ., 
                SampleTrain0Lat,
                method = "rf",
                trControl = trainControl(method = "repeatedcv",
                                         number = 5,
                                         repeats = 2),
                tuneGrid = RFGrid,
                tuneLenght = 2,
                preProcess = "zv")

predRF0Lat <- predict(RF0Lat, Valid0Lat)

postResample(predRF0Lat, Valid0Lat$LATITUDE) -> RF0LatMetrics
RF0LatMetrics
# RMSE: 5.968
# Rsquared: 0.967
# MAE: 4/249

## Building 0 Floor
RF0Floor <- train(FLOOR ~ ., 
                  SampleTrain0Floor,
                  method = "rf",
                  trControl = trainControl(method = "repeatedcv",
                                           number = 5,
                                           repeats = 2),
                  tuneGrid = RFGrid,
                  tuneLenght = 2,
                  preProcess = "zv")

predRF0Floor <- predict(RF0Floor, Valid0Floor)

postResample(predRF0Floor, Valid0Floor$FLOOR) -> RF0FloorMetrics
RF0FloorMetrics
# Accuracy: 0.962
# Kappa: 0.947

## Building 1 Longitude
RF1Long <- train(LONGITUDE ~ ., 
                 SampleTrain1Long,
                 method = "rf",
                 trControl = trainControl(method = "repeatedcv",
                                          number = 5,
                                          repeats = 2),
                 tuneGrid = RFGrid,
                 tuneLenght = 2,
                 preProcess = "zv")

predRF1Long <- predict(RF1Long, Valid1Long)

postResample(predRF1Long, Valid1Long$LONGITUDE) -> RF1LongMetrics
RF1LongMetrics
# RMSE: 10.035
# Rsquared: 0.954
# MAE: 7.112

## Building 1 Latitude
RF1Lat <- train(LATITUDE ~ ., 
                SampleTrain1Lat,
                method = "rf",
                trControl = trainControl(method = "repeatedcv",
                                         number = 5,
                                         repeats = 2),
                tuneGrid = RFGrid,
                tuneLenght = 2,
                preProcess = "zv")

predRF1Lat <- predict(RF1Lat, Valid1Lat)

postResample(predRF1Lat, Valid1Lat$LATITUDE) -> RF1LatMetrics
RF1LatMetrics
# RMSE: 11.116
# Rsquared: 0.903
# MAE: 8.209

## Building 1 Floor
RF1Floor <- train(FLOOR ~ ., 
                  SampleTrain1Floor,
                  method = "rf",
                  trControl = trainControl(method = "repeatedcv",
                                           number = 5,
                                           repeats = 2),
                  tuneGrid = RFGrid,
                  tuneLenght = 2,
                  preProcess = "zv")

predRF1Floor <- predict(RF1Floor, Valid1Floor)

postResample(predRF1Floor, Valid1Floor$FLOOR) -> RF1FloorMetrics
RF1FloorMetrics
# Accuracy: 0.791
# Kappa: 0.698

## Building 2 Longitude
RF2Long <- train(LONGITUDE ~ ., 
                 SampleTrain2Long,
                 method = "rf",
                 trControl = trainControl(method = "repeatedcv",
                                          number = 5,
                                          repeats = 2),
                 tuneGrid = RFGrid,
                 tuneLenght = 2,
                 preProcess = "zv")

predRF2Long <- predict(RF2Long, Valid2Long)

postResample(predRF2Long, Valid2Long$LONGITUDE) -> RF2LongMetrics
RF2LongMetrics
# RMSE: 11.536
# Rsquared: 0.874
# MAE: 8.465

## Building 2 Latitude
RF2Lat <- train(LATITUDE ~ ., 
                SampleTrain2Lat,
                method = "rf",
                trControl = trainControl(method = "repeatedcv",
                                         number = 5,
                                         repeats = 2),
                tuneGrid = RFGrid,
                tuneLenght = 2,
                preProcess = "zv")

predRF2Lat <- predict(RF2Lat, Valid2Lat)

postResample(predRF2Lat, Valid2Lat$LATITUDE) -> RF2LatMetrics
RF2LatMetrics
# RMSE: 10.407
# Rsquared: 0.876
# MAE: 7.282

## Building 2 Floor
RF2Floor <- train(FLOOR ~ ., 
                  SampleTrain2Floor,
                  method = "rf",
                  trControl = trainControl(method = "repeatedcv",
                                           number = 5,
                                           repeats = 2),
                  tuneGrid = RFGrid,
                  tuneLenght = 2,
                  preProcess = "zv")

predRF2Floor <- predict(RF2Floor, Valid2Floor)

postResample(predRF2Floor, Valid2Floor$FLOOR) -> RF2FloorMetrics
RF2FloorMetrics
# Accuracy: 0.861
# Kappa: 0.811

### ---- Model Comparison ----
## Creating data frames for performance and accuracy metrics
PerformanceMetrics <- data.frame(kNN0LongMetrics, 
                                 kNN0LatMetrics,
                                 kNN1LongMetrics,
                                 kNN1LatMetrics,
                                 kNN2LongMetrics,
                                 kNN2LatMetrics,
                                 SVM0LongMetrics,
                                 SVM0LatMetrics,
                                 SVM1LongMetrics,
                                 SVM1LatMetrics,
                                 SVM2LongMetrics,
                                 SVM2LatMetrics,
                                 RF0LongMetrics,
                                 RF0LatMetrics,
                                 RF1LongMetrics,
                                 RF1LatMetrics,
                                 RF2LongMetrics,
                                 RF2LatMetrics)

AccuracyMetrics <- data.frame(kNN0FloorMetrics,
                              kNN1FloorMetrics,
                              kNN2FloorMetrics,
                              SVM0FloorMetrics,
                              SVM1FloorMetrics,
                              SVM2FloorMetrics,
                              RF0FloorMetrics,
                              RF1FloorMetrics,
                              RF2FloorMetrics)

## Transposing the data frame
PerformanceMetrics <- data.frame(t(PerformanceMetrics))

AccuracyMetrics <- data.frame(t(AccuracyMetrics))

## Naming the rows
PerformanceMetrics$Algorithms <- rownames(PerformanceMetrics)

AccuracyMetrics$Algorithms <- rownames(AccuracyMetrics)

## Reordering the columns
PerformanceMetrics <- PerformanceMetrics[, c(4,1,2,3)]

AccuracyMetrics <- AccuracyMetrics[, c(3,1,2)]

## Arranging by Rsquared, MAE and Accuracy to see the best models
PerformanceMetrics %>% 
  arrange(desc(Rsquared)) 

PerformanceMetrics %>% 
  arrange(MAE) 

AccuracyMetrics %>% 
  arrange(desc(Accuracy))

### ---- Error Check ----
## Creating data frames for random forest predictions
Building0pred <- data.frame(pred.longitude = predkNN0Long, 
                            pred.latitude = predkNN0Lat, 
                            pred.floor = predRF0Floor)

Building1pred <- data.frame(pred.longitude = predkNN1Long, 
                            pred.latitude = predkNN1Lat, 
                            pred.floor = predRF1Floor)

Building2pred <- data.frame(pred.longitude = predkNN2Long, 
                            pred.latitude = predkNN2Lat, 
                            pred.floor = predSVM2Floor)

## Combining the data frames
ErrorData <- rbind(Building0pred, Building1pred, Building2pred)

ErrorValidData <- cbind(ErrorData, 
                        WifiValidationData$LONGITUDE, 
                        WifiValidationData$LATITUDE,
                        WifiValidationData$FLOOR)

ErrorValidData %>% 
  rename(valid.long = "WifiValidationData$LONGITUDE",
         valid.lat = "WifiValidationData$LATITUDE",
         valid.floor = "WifiValidationData$FLOOR") -> ErrorValidData

ErrorValidData$valid.floor <- as.factor(ErrorValidData$valid.floor)

## Error Visualization
## Longitude & Latitude
ErrorValidData %>% 
  ggplot(aes(x = valid.long, y = valid.lat)) +
  geom_point(aes(x = valid.long, 
                 y = valid.lat), 
             color = "red") +
  geom_point(aes(x = pred.longitude, 
                 y = pred.latitude), 
             color = "blue") +
  geom_label(aes(x = -7400, y = 4864960, label = "Actual"), 
             color = "red", 
             size = 4) +
  geom_label(aes(x = -7400, y = 4864930, label = "Pred"), 
             color = "blue", 
             size = 4) +
  labs(title = "Actual Data on Validation vs Predictions") + 
  ylab("Latitude") + 
  xlab("Longitude") + 
  theme_light()

## Floor
confusionMatrix(ErrorValidData$valid.floor, ErrorValidData$pred.floor)
