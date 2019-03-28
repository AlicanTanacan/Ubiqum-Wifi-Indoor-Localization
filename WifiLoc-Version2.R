### --------------------- Wifi Locationing --------------------- ###
### -------------------- by Alican Tanaçan --------------------- ###
### ---------- Version 2: Subsetting and Modelization ---------- ###

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

### Subsetting ----
set.seed(753)
## Training Data
# Building 0
WifiTrainingData %>% 
  filter(BUILDINGID == 0) %>%
  select(starts_with("WAP"), LONGITUDE) -> Build0Long

SampleBuild0Long <- Build0Long[sample(1:nrow(Build0Long), 1000, replace = F),]

WifiTrainingData %>% 
  filter(BUILDINGID == 0) %>%
  select(starts_with("WAP"), LATITUDE) -> Build0Lat

SampleBuild0Lat <- Build0Lat[sample(1:nrow(Build0Lat), 1000, replace = F),]

WifiTrainingData %>% 
  filter(BUILDINGID == 0) %>%
  select(starts_with("WAP"), FLOOR) -> Build0Floor

SampleBuild0Floor <- Build0Floor[sample(1:nrow(Build0Floor), 1000, replace = F),]
SampleBuild0Floor$FLOOR <- as.factor(SampleBuild0Floor$FLOOR)

# Building 1
WifiTrainingData %>% 
  filter(BUILDINGID == 1) %>%
  select(starts_with("WAP"), LONGITUDE) -> Build1Long

SampleBuild1Long <- Build1Long[sample(1:nrow(Build1Long), 1000, replace = F),]

WifiTrainingData %>% 
  filter(BUILDINGID == 1) %>%
  select(starts_with("WAP"), LATITUDE) -> Build1Lat

SampleBuild1Lat <- Build1Lat[sample(1:nrow(Build1Lat), 1000, replace = FALSE),]

WifiTrainingData %>% 
  filter(BUILDINGID == 1) %>%
  select(starts_with("WAP"), FLOOR) -> Build1Floor

SampleBuild1Floor <- Build1Floor[sample(1:nrow(Build1Floor), 1000, replace = F),]
SampleBuild1Floor$FLOOR <- as.factor(SampleBuild1Floor$FLOOR)

# Building 2
WifiTrainingData %>% 
  filter(BUILDINGID == 2) %>%
  select(starts_with("WAP"), LONGITUDE) -> Build2Long

SampleBuild2Long <- Build2Long[sample(1:nrow(Build2Long), 1000, replace = F),]

WifiTrainingData %>% 
  filter(BUILDINGID == 2) %>%
  select(starts_with("WAP"), LATITUDE) -> Build2Lat

SampleBuild2Lat <- Build2Lat[sample(1:nrow(Build2Lat), 1000, replace = FALSE),]

WifiTrainingData %>% 
  filter(BUILDINGID == 2) %>%
  select(starts_with("WAP"), FLOOR) -> Build2Floor

SampleBuild2Floor <- Build2Floor[sample(1:nrow(Build2Floor), 1000, replace = F),]
SampleBuild2Floor$FLOOR <- as.factor(SampleBuild2Floor$FLOOR)

## Validation Data
# Building 0
WifiValidationData %>% 
  filter(BUILDINGID == 0) %>%
  select(starts_with("WAP"), LONGITUDE) -> VBuild0Long

WifiValidationData %>% 
  filter(BUILDINGID == 0) %>%
  select(starts_with("WAP"), LATITUDE) -> VBuild0Lat

WifiValidationData %>% 
  filter(BUILDINGID == 0) %>%
  select(starts_with("WAP"), FLOOR) -> VBuild0Floor
VBuild0Floor$FLOOR <- as.factor(VBuild0Floor$FLOOR)

# Building 1
WifiValidationData %>% 
  filter(BUILDINGID == 1) %>%
  select(starts_with("WAP"), LONGITUDE) -> VBuild1Long

WifiValidationData %>% 
  filter(BUILDINGID == 1) %>%
  select(starts_with("WAP"), LATITUDE) -> VBuild1Lat

WifiValidationData %>% 
  filter(BUILDINGID == 1) %>%
  select(starts_with("WAP"), FLOOR) -> VBuild1Floor
VBuild1Floor$FLOOR <- as.factor(VBuild1Floor$FLOOR)

# Building 2
WifiValidationData %>% 
  filter(BUILDINGID == 2) %>%
  select(starts_with("WAP"), LONGITUDE) -> VBuild2Long

WifiValidationData %>% 
  filter(BUILDINGID == 2) %>%
  select(starts_with("WAP"), LATITUDE) -> VBuild2Lat

WifiValidationData %>% 
  filter(BUILDINGID == 2) %>%
  select(starts_with("WAP"), FLOOR) -> VBuild2Floor
VBuild2Floor$FLOOR <- as.factor(VBuild2Floor$FLOOR)

### kNN Modelization ----
# Building 0 Longitude
kNN0Long <- train(LONGITUDE ~ ., 
                  SampleBuild0Long,
                  method = "knn",
                  trControl = trainControl(method = "cv",
                                           number = 5),
                  preProcess = "zv")

predkNN0Long <- predict(kNN0Long, VBuild0Long)

postResample(predkNN0Long, VBuild0Long$LONGITUDE)

# Building 0 Latitude
kNN0Lat <- train(LATITUDE ~ ., 
                  SampleBuild0Lat,
                  method = "knn",
                  trControl = trainControl(method = "cv",
                                           number = 5),
                  preProcess = "zv")

predkNN0Lat <- predict(kNN0Lat, VBuild0Lat)

postResample(predkNN0Lat, VBuild0Lat$LATITUDE)

# Building 0 Floor
kNN0Floor <- train(FLOOR ~ ., 
                   SampleBuild0Floor,
                   method = "knn",
                   trControl = trainControl(method = "cv",
                                            number = 5),
                   preProcess = "zv")

predkNN0Floor <- predict(kNN0Floor, VBuild0Floor)

postResample(predkNN0Floor, VBuild0Floor$FLOOR)
