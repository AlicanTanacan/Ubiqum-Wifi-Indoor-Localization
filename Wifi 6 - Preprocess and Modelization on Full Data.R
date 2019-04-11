### ---------------------- Wifi Locationing ---------------------- ###
### --------------------- by Alican Tanaçan ---------------------- ###
### ----- Version 6: Preprocess & Modelization on Full Data ------ ###

### ---- Libraries ----
if(require("pacman") == "FALSE"){
  install.packages("pacman")
}
p_load(shiny, shinydashboard, dplyr, ggplot2, plotly, lubridate,
       naniar, devtools, corrplot, GGally, caret, tidyverse, e1071,
       kernlab, randomForest, gridExtra, caTools)

### ---- Working Directory ----
setwd("D:/RStudio/R Studio Working Directory")

### ---- Import Wifi Training and Validation Data ----
WifiTrainingData <- read.csv("WifitrainingData.csv")

WifiValidationData <- read.csv("WifivalidationData.csv")

WifiData <- rbind(WifiTrainingData, WifiValidationData)

### ---- Preprocessing ----
## Create a data frame for WAPS and rest of the VARIABLES
WAPS <- WifiData[,1:520]

VARIABLES <- WifiData[, 521:529]

## Checking correlations
ggcorr(VARIABLES, label = T)

## Revaluation of WAPS with poor signal
WAPS[WAPS > -30] <- -105

## Check interval of variance, mean and median of columns to eliminate 
wapsdata <- data.frame(variance = apply(WAPS, 2, var),
                       mean = apply(WAPS, 2, mean),
                       median = apply(WAPS, 2, median))
summary(wapsdata)

PoorWAPSCol <- apply(WAPS, 2, var) <= 0.05

PoorWAPSRow <- apply(WAPS, 1, var) <= 0.05

GoodWAPS <- WAPS[!PoorWAPSRow,
                 !PoorWAPSCol]

## Equalizing the row amount in both VARIABLE data sets
VARIABLES2 <- VARIABLES[!PoorWAPSRow, ]

## Combining the WAPS and VARIABLES data
AdjWifiData <- cbind(GoodWAPS, VARIABLES2)

### ---- Subsetting & Sampling ----
set.seed(1020)
## Data Partition
sample <- sample.split(AdjWifiData, SplitRatio = .80)
AdjTrainingData <- subset(AdjWifiData, sample == TRUE)
AdjValidationData <- subset(AdjWifiData, sample == FALSE)

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
# RMSE: 4.930
# Rsquared: 0.962
# MAE: 3.232

## Building 0 Latitude
kNN0Lat <- train(LATITUDE ~ ., 
                 SampleTrain0Lat,
                 method = "knn",
                 trControl = kNNcontrol,
                 preProcess = "zv")

predkNN0Lat <- predict(kNN0Lat, Valid0Lat)

postResample(predkNN0Lat, Valid0Lat$LATITUDE) -> kNN0LatMetrics
kNN0LatMetrics
# RMSE: 4.131
# Rsquared: 0.984
# MAE: 2.869

## Building 0 Floor
kNN0Floor <- train(FLOOR ~ ., 
                   SampleTrain0Floor,
                   method = "knn",
                   trControl = kNNcontrol,
                   preProcess = "zv")

predkNN0Floor <- predict(kNN0Floor, Valid0Floor)

postResample(predkNN0Floor, Valid0Floor$FLOOR) -> kNN0FloorMetrics
kNN0FloorMetrics
# Accuracy: 0.988
# Kappa: 0.984

## Building 1 Longitude
kNN1Long <- train(LONGITUDE ~ ., 
                  SampleTrain1Long,
                  method = "knn",
                  trControl = kNNcontrol,
                  preProcess = "zv")

predkNN1Long <- predict(kNN1Long, Valid1Long)

postResample(predkNN1Long, Valid1Long$LONGITUDE) -> kNN1LongMetrics
kNN1LongMetrics
# RMSE: 7.152
# Rsquared: 0.978
# MAE: 4.604

## Building 1 Latitude
kNN1Lat <- train(LATITUDE ~ ., 
                 SampleTrain1Lat,
                 method = "knn",
                 trControl = kNNcontrol,
                 preProcess = "zv")

predkNN1Lat <- predict(kNN1Lat, Valid1Lat)

postResample(predkNN1Lat, Valid1Lat$LATITUDE) -> kNN1LatMetrics
kNN1LatMetrics
# RMSE: 7.048
# Rsquared: 0.961
# MAE: 4.443

## Building 1 Floor
kNN1Floor <- train(FLOOR ~ ., 
                   SampleTrain1Floor,
                   method = "knn",
                   trControl = kNNcontrol,
                   preProcess = "zv")

predkNN1Floor <- predict(kNN1Floor, Valid1Floor)

postResample(predkNN1Floor, Valid1Floor$FLOOR) -> kNN1FloorMetrics
kNN1FloorMetrics
# Accuracy: 0.965
# Kappa: 0.953

## Building 2 Longitude
kNN2Long <- train(LONGITUDE ~ ., 
                  SampleTrain2Long,
                  method = "knn",
                  trControl = kNNcontrol,
                  preProcess = "zv")

predkNN2Long <- predict(kNN2Long, Valid2Long)

postResample(predkNN2Long, Valid2Long$LONGITUDE) -> kNN2LongMetrics
kNN2LongMetrics
# RMSE: 9.153
# Rsquared: 0.906
# MAE: 4.954

## Building 2 Latitude
kNN2Lat <- train(LATITUDE ~ ., 
                 SampleTrain2Lat,
                 method = "knn",
                 trControl = kNNcontrol,
                 preProcess = "zv")

predkNN2Lat <- predict(kNN2Lat, Valid2Lat)

postResample(predkNN2Lat, Valid2Lat$LATITUDE) -> kNN2LatMetrics
kNN2LatMetrics
# RMSE: 5.444
# Rsquared: 0.963
# MAE: 3.563

## Building 2 Floor
kNN2Floor <- train(FLOOR ~ ., 
                   SampleTrain2Floor,
                   method = "knn",
                   trControl = kNNcontrol,
                   preProcess = "zv")

predkNN2Floor <- predict(kNN2Floor, Valid2Floor)

postResample(predkNN2Floor, Valid2Floor$FLOOR) -> kNN2FloorMetrics
kNN2FloorMetrics
# Accuracy: 0.981
# Kappa: 0.975

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
# RMSE: 9.386
# Rsquared: 0.862
# MAE: 6.760

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
# RMSE: 9.315
# Rsquared: 0.918
# MAE: 7.012

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
# Accuracy: 0.977
# Kappa: 0.969

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
# RMSE: 16.136
# Rsquared: 0.893
# MAE: 11.493

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
# RMSE: 11.416
# Rsquared: 0.897
# MAE: 8.333

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
# Accuracy: 0.977
# Kappa: 0.968

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
# RMSE: 18.071
# Rsquared: 0.665
# MAE: 11.393

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
# RMSE: 12.655
# Rsquared: 0.802
# MAE: 9.637

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
# Accuracy: 0.979
# Kappa: 0.974

### ---- RF Modelization ----
set.seed(4080)
## Building 0 Longitude
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
# RMSE: 5.234
# Rsquared: 0.960
# MAE: 3.534

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
# RMSE: 3.904
# Rsquared: 0.986
# MAE: 2.708

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
# Accuracy: 0.992
# Kappa: 0.989

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
# RMSE: 6.778
# Rsquared: 0.981
# MAE: 4.667

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
# RMSE: 6.354
# Rsquared: 0.969
# MAE: 4.321

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
# Accuracy: 0.983
# Kappa: 0.978

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
# RMSE: 8.266
# Rsquared: 0.930
# MAE: 5.429

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
# RMSE: 6.956
# Rsquared: 0.944
# MAE: 4.516

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
# Accuracy: 0.983
# Kappa: 0.978  

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
                            pred.latitude = predRF0Lat, 
                            pred.floor = predRF0Floor,
                            valid.longitude = Valid0Long$LONGITUDE,
                            valid.latitude = Valid0Lat$LATITUDE,
                            valid.floor = Valid0Floor$FLOOR)

Building1pred <- data.frame(pred.longitude = predkNN1Long, 
                            pred.latitude = predRF1Lat, 
                            pred.floor = predRF1Floor,
                            valid.longitude = Valid1Long$LONGITUDE,
                            valid.latitude = Valid1Lat$LATITUDE,
                            valid.floor = Valid1Floor$FLOOR)

Building2pred <- data.frame(pred.longitude = predkNN2Long, 
                            pred.latitude = predkNN2Lat, 
                            pred.floor = predRF1Floor,
                            valid.longitude = Valid2Long$LONGITUDE,
                            valid.latitude = Valid2Lat$LATITUDE,
                            valid.floor = Valid2Floor$FLOOR)

## Combining the data frames
ErrorData <- rbind(Building0pred, Building1pred, Building2pred)

ErrorData$valid.floor <- as.factor(ErrorData$valid.floor)

## Error Visualization
## Longitude & Latitude
ErrorData %>% 
  ggplot(aes(x = valid.longitude, y = valid.latitude)) +
  geom_point(aes(x = valid.longitude, 
                 y = valid.latitude), 
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
confusionMatrix(ErrorData$valid.floor, ErrorData$pred.floor)
