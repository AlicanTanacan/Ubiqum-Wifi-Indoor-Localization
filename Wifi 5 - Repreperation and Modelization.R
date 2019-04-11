### ---------------------- Wifi Locationing ---------------------- ###
### --------------------- by Alican Tanaçan ---------------------- ###
### ---- Version 5: Re-preparation of Data and Model Building ---- ###

## In this version, we will try to define range of values for good 
# and bad signals. Later we will try to take mean and variance to 
# eliminate poor signal WAPS; then we will subset for each building and
# try different models with different preprocesses.

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
WifiTrainingData <- readRDS("CleanWifiTrainData.rds")

WifiValidationData <- read.csv("WifivalidationData.csv")

### ---- Preprocessing ----
## Create a data frame for WAPS and rest of the VARIABLES
WAPStrain <- WifiTrainingData[,1:520]

VARIABLEStrain <- WifiTrainingData[, 521:529]

WAPSvalid <- WifiValidationData[,1:520]

VARIABLESvalid <- WifiValidationData[, 521:529]

## Checking correlations
ggcorr(VARIABLEStrain, label = T)

## Revaluation of WAPS with poor signal
## Train
WAPStrain[WAPStrain < -90] <- -105

## Check interval of variance, mean and median of columns to eliminate 
wapsdata <- data.frame(variance = apply(WAPStrain, 2, var),
                       mean = apply(WAPStrain, 2, mean),
                       median = apply(WAPStrain, 2, median))
summary(wapsdata)

## Define poor signal waps and remove them from the data
PoorWAPStrainCol <- apply(WAPStrain, 2, var) <= 0.05

PoorWAPStrainRow <- apply(WAPStrain, 1, var) <= 0.05

GoodWAPStrain <- WAPStrain[!PoorWAPStrainRow,
                           !PoorWAPStrainCol]

## Validation
WAPSvalid[WAPSvalid == 100] <- -105
WAPSvalid[WAPSvalid < -90] <- -105 

validwapsdata <- data.frame(variance = apply(WAPSvalid, 2, var),
                            mean = apply(WAPSvalid, 2, mean),
                            median = apply(WAPSvalid, 2, median))
summary(validwapsdata)

PoorWAPSvalidCol <- apply(WAPSvalid, 2, var) == 0

PoorWAPSvalidRow <- apply(WAPSvalid, 1, var) == 0

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

SampleTrain2Long <- Train2Long[sample(1:nrow(Train2Long), 2000, replace = F),]

AdjValidationData %>%
  filter(BUILDINGID == 2) %>% 
  select(starts_with("WAP"), LONGITUDE) -> Valid2Long

## Latitude
AdjTrainingData %>%
  filter(BUILDINGID == 2) %>% 
  select(starts_with("WAP"), LATITUDE) -> Train2Lat

SampleTrain2Lat <- Train2Lat[sample(1:nrow(Train2Lat), 2000, replace = F),]

AdjValidationData %>%
  filter(BUILDINGID == 2) %>% 
  select(starts_with("WAP"), LATITUDE) -> Valid2Lat

## Floor
AdjTrainingData %>%
  filter(BUILDINGID == 2) %>% 
  select(starts_with("WAP"), FLOOR) -> Train2Floor

SampleTrain2Floor <- Train2Floor[sample(1:nrow(Train2Floor), 2000, replace = F),]

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
# RMSE: 6.052
# Rsquared: 0.947
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
# RMSE: 5.182
# Rsquared: 0.973
# MAE: 3.536

## Building 0 Floor
kNN0Floor <- train(FLOOR ~ ., 
                   SampleTrain0Floor,
                   method = "knn",
                   trControl = kNNcontrol,
                   preProcess = "zv")

predkNN0Floor <- predict(kNN0Floor, Valid0Floor)

postResample(predkNN0Floor, Valid0Floor$FLOOR) -> kNN0FloorMetrics
kNN0FloorMetrics
# Accuracy: 0.958
# Kappa: 0.942

## Building 1 Longitude
kNN1Long <- train(LONGITUDE ~ ., 
                  SampleTrain1Long,
                  method = "knn",
                  trControl = kNNcontrol,
                  preProcess = "zv")

predkNN1Long <- predict(kNN1Long, Valid1Long)

postResample(predkNN1Long, Valid1Long$LONGITUDE) -> kNN1LongMetrics
kNN1LongMetrics
# RMSE: 9.673
# Rsquared: 0.955
# MAE: 6.470

## Building 1 Latitude
kNN1Lat <- train(LATITUDE ~ ., 
                 SampleTrain1Lat,
                 method = "knn",
                 trControl = kNNcontrol,
                 preProcess = "zv")

predkNN1Lat <- predict(kNN1Lat, Valid1Lat)

postResample(predkNN1Lat, Valid1Lat$LATITUDE) -> kNN1LatMetrics
kNN1LatMetrics
# RMSE: 10.488
# Rsquared: 0.912
# MAE: 6.636

## Building 1 Floor
kNN1Floor <- train(FLOOR ~ ., 
                   SampleTrain1Floor,
                   method = "knn",
                   trControl = kNNcontrol,
                   preProcess = "zv")

predkNN1Floor <- predict(kNN1Floor, Valid1Floor)

postResample(predkNN1Floor, Valid1Floor$FLOOR) -> kNN1FloorMetrics
kNN1FloorMetrics
# Accuracy: 0.780
# Kappa: 0.686

## Building 2 Longitude
kNN2Long <- train(LONGITUDE ~ ., 
                  SampleTrain2Long,
                  method = "knn",
                  trControl = kNNcontrol,
                  preProcess = "zv")

predkNN2Long <- predict(kNN2Long, Valid2Long)

postResample(predkNN2Long, Valid2Long$LONGITUDE) -> kNN2LongMetrics
kNN2LongMetrics
# RMSE: 11.783
# Rsquared: 0.865
# MAE: 7.332

## Building 2 Latitude
kNN2Lat <- train(LATITUDE ~ ., 
                 SampleTrain2Lat,
                 method = "knn",
                 trControl = kNNcontrol,
                 preProcess = "zv")

predkNN2Lat <- predict(kNN2Lat, Valid2Lat)

postResample(predkNN2Lat, Valid2Lat$LATITUDE) -> kNN2LatMetrics
kNN2LatMetrics
# RMSE: 9.396
# Rsquared: 0.898
# MAE: 6.125

## Building 2 Floor
kNN2Floor <- train(FLOOR ~ ., 
                   SampleTrain2Floor,
                   method = "knn",
                   trControl = kNNcontrol,
                   preProcess = "zv")

predkNN2Floor <- predict(kNN2Floor, Valid2Floor)

postResample(predkNN2Floor, Valid2Floor$FLOOR) -> kNN2FloorMetrics
kNN2FloorMetrics
# Accuracy: 0.957
# Kappa: 0.941

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
# RMSE: 11.673
# Rsquared: 0.800
# MAE: 8.592

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
# RMSE: 13.204
# Rsquared: 0.841
# MAE: 10.014

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
# Accuracy: 0.925
# Kappa: 0.895

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
# RMSE: 23.258
# Rsquared: 0.775
# MAE: 17.488

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
# RMSE: 18.628
# Rsquared: 0.756
# MAE: 13.370

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
# Accuracy: 0.803
# Kappa: 0.722

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
# RMSE: 17.468
# Rsquared: 0.691
# MAE: 13.425

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
# RMSE: 15.816
# Rsquared: 0.717
# MAE: 12.098

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
# Accuracy: 0.940
# Kappa: 0.918

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
# RMSE: 7.617
# Rsquared: 0.919
# MAE: 5.333

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
# RMSE: 6.953
# Rsquared: 0.955
# MAE: 4.552

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
# Accuracy: 0.966
# Kappa: 0.952

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
# RMSE: 8.925
# Rsquared: 0.962
# MAE: 6.485

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
# RMSE: 10.745
# Rsquared: 0.909
# MAE: 7.546

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
# Accuracy: 0.809
# Kappa: 0.725

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
# RMSE: 11.626
# Rsquared: 0.866
# MAE: 7.995

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
# RMSE: 10.368
# Rsquared: 0.874
# MAE: 7.077

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
# Accuracy: 0.925
# Kappa: 0.898

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
Building0pred <- data.frame(building = "0",
                            pred.longitude = predkNN0Long, 
                            pred.latitude = predkNN0Lat, 
                            pred.floor = predRF0Floor,
                            valid.longitude = Valid0Long$LONGITUDE,
                            valid.latitude = Valid0Lat$LATITUDE,
                            valid.floor = Valid0Floor$FLOOR)

Building1pred <- data.frame(building = "1",
                            pred.longitude = predRF1Long, 
                            pred.latitude = predkNN1Lat, 
                            pred.floor = predRF1Floor,
                            valid.longitude = Valid1Long$LONGITUDE,
                            valid.latitude = Valid1Lat$LATITUDE,
                            valid.floor = Valid1Floor$FLOOR)

Building2pred <- data.frame(building = "2",
                            pred.longitude = predRF2Long, 
                            pred.latitude = predkNN2Lat, 
                            pred.floor = predSVM2Floor,
                            valid.longitude = Valid2Long$LONGITUDE,
                            valid.latitude = Valid2Lat$LATITUDE,
                            valid.floor = Valid2Floor$FLOOR)

## Combining the data frames
ErrorData <- rbind(Building0pred, Building1pred, Building2pred)

## Calculating Errors
ErrorData$err.long <- abs(ErrorData$valid.longitude - ErrorData$pred.longitude)

ErrorData$err.lat <- abs(ErrorData$valid.latitude - ErrorData$pred.latitude)

ErrorData %>% 
  mutate_at(c("valid.floor", "pred.floor"), as.numeric) -> ErrorData

ErrorData$err.floor <- abs(ErrorData$valid.floor - ErrorData$pred.floor)
ErrorData$err.floor <- as.factor(ErrorData$err.floor)

ErrorData %>% 
  mutate_at(c("valid.floor",
              "pred.floor"),
            as.numeric) %>% 
  mutate(diff.floor = ifelse(valid.floor == pred.floor, 0, 1 )) %>% 
  mutate_at(c("valid.floor",
              "pred.floor"),
            as.factor) -> ErrorData

str(ErrorData)

## Checking Errors

confusionMatrix(ErrorData$pred.floor, ErrorData$valid.floor)

ErrorData %>% 
  filter(building == 0) %>% 
  group_by(diff.floor) %>% 
  count(valid.floor)

ErrorData %>% 
  filter(building == 0) %>% 
  group_by(err.floor) %>% 
  plot_ly(x = ~pred.longitude, 
          y = ~pred.latitude, 
          z = ~pred.floor, 
          colors = c("red","blue")) %>%
  add_markers(color = ~err.floor == 0, size = 1) %>%
  layout(title = "Building 0 Floor Error Check",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

ErrorData %>%  
  filter(building == 1) %>% 
  group_by(diff.floor) %>% 
  count(valid.floor)

ErrorData %>% 
  filter(building == 1) %>% 
  group_by(err.floor) %>% 
  plot_ly(x = ~pred.longitude, 
          y = ~pred.latitude, 
          z = ~pred.floor, 
          colors = c("red","blue")) %>%
  add_markers(color = ~err.floor == 0, size = 1) %>%
  layout(title = "Building 1 Floor Error Check",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

ErrorData %>% 
  filter(building == 2) %>% 
  group_by(diff.floor) %>% 
  count(valid.floor)

ErrorData %>% 
  filter(building == 2) %>% 
  group_by(err.floor) %>% 
  plot_ly(x = ~pred.longitude, 
          y = ~pred.latitude, 
          z = ~pred.floor, 
          colors = c("red","blue")) %>%
  add_markers(color = ~err.floor == 0, size = 1) %>%
  layout(title = "Building 2 Floor Error Check",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

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
