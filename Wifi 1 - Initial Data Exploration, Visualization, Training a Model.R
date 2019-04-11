### --------------------- Wifi Locationing --------------------- ###
### -------------------- by Alican Tanaçan --------------------- ###
### ----------- Version 1: Initial Data Exploration ------------ ###

### ---- Libraries ----
if(require("pacman") == "FALSE"){
  install.packages("pacman")
}
p_load(shiny, shinydashboard, dplyr, ggplot2, plotly, lubridate,
       naniar, devtools, corrplot, GGally, caret, tidyverse, e1071,
       kernlab, randomForest, gridExtra, caTools)

### ---- Import Wifi Training and Validation Data ----
WifiTrainingData <- read.csv("WifitrainingData.csv")

WifiValidationData <- read.csv("WifivalidationData.csv")

WifiData <- rbind(WifiTrainingData, WifiValidationData)

### ---- Data Exploration ----
## Inspect the Data Types
summary(WifiTrainingData[,520:529])
str(WifiTrainingData[,520:529])

summary(WifiValidationData[,520:529])
str(WifiValidationData[,520:529])

WifiTrainingData %>% 
  group_by(BUILDINGID, FLOOR, USERID, PHONEID) %>% 
  count()-> GroupedData

write.csv(GroupedData, file = "FilteredWifiData.csv")

WifiValidationData %>% 
  group_by(BUILDINGID, FLOOR, USERID, PHONEID) %>% 
  count()-> GroupedValidData

write.csv(GroupedValidData, file = "FilteredWifiValidData.csv")

## Total Number of Observations in each Floor
BuildingFloorObs %>% 
  group_by((BUILDINGID)) %>% 
  ggplot(aes(FLOOR, n)) +
  geom_col(fill = "brown") +
  labs(title = "Total Number of Observations in Each Building & Floor") + 
  ylab("Number of Observations") + 
  xlab("FLOOR") + 
  facet_wrap(~BUILDINGID)

## Building 0 Analysis
WifiTrainingData %>% 
  filter(BUILDINGID == 0) %>% 
  group_by(FLOOR) %>% 
  count(FLOOR)

WifiTrainingData %>% 
  filter(BUILDINGID == 0) %>% 
  group_by(FLOOR, USERID, PHONEID) %>% 
  count(USERID)

## Building 1 Analysis
WifiTrainingData %>% 
  filter(BUILDINGID == 1) %>% 
  group_by(FLOOR) %>% 
  count(FLOOR)

WifiTrainingData %>% 
  filter(BUILDINGID == 1) %>% 
  group_by(FLOOR, USERID, PHONEID) %>% 
  count(USERID)

## Building 2 Analysis
WifiTrainingData %>% 
  filter(BUILDINGID == 2) %>% 
  group_by(FLOOR) %>% 
  count(FLOOR)

WifiTrainingData %>% 
  filter(BUILDINGID == 2) %>% 
  group_by(FLOOR, USERID, PHONEID) %>% 
  count(USERID)

### ---- Data Visualization ----
# What happens if we plot longitude to latitude?
plot(WifiTrainingData$LONGITUDE, WifiTrainingData$LATITUDE)
plot(WifiValidationData$LONGITUDE, WifiValidationData$LATITUDE)

# Longitude vs Latitude
WifiTrainingData %>% 
  ggplot(aes(x = LONGITUDE, y = LATITUDE)) +
  geom_point(color = "brown") +
  labs(title = "Training Data Longitude vs Latitude")

WifiValidationData %>% 
  ggplot(aes(x = LONGITUDE, y = LATITUDE)) +
  geom_point(color = "red") +
  labs(title = "Validation Data Longitude vs Latitude")

WifiData %>% 
  ggplot(aes(x = LONGITUDE, y = LATITUDE)) +
  geom_point(color = "darkred") +
  labs(title = "All Wifi Data Longitude vs Latitude")

# Building 0 Preview
WifiData %>% 
  filter(BUILDINGID == 0) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~as.factor(FLOOR), 
          colors = c("green", "orange", "yellow", "pink", "purple")) %>%
  add_markers() %>%
  layout(title = "Building 0 Preview",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

# Building 1 Preview
WifiData %>% 
  filter(BUILDINGID == 1) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~as.factor(FLOOR), 
          colors = c("green", "orange", "yellow", "pink", "purple")) %>%
  add_markers() %>%
  layout(title = "Building 1 Preview",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

# Building 2 Preview
WifiData %>% 
  filter(BUILDINGID == 2) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~as.factor(FLOOR), 
          colors = c("green", "orange", "yellow", "pink", "purple")) %>%
  add_markers() %>%
  layout(title = "Building 2 Preview",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

## Users' Behaviors Visualization
WifiTrainingData %>% 
  filter(BUILDINGID == 0, PHONEID != 14) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~as.factor(USERID)) %>%
  add_markers() %>%
  layout(title = "User Behavior Building 0",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

WifiTrainingData %>% 
  filter(BUILDINGID == 1) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~as.factor(USERID)) %>%
  add_markers() %>%
  layout(title = "User Behavior Building 1",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

WifiTrainingData %>% 
  filter(BUILDINGID == 2) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~as.factor(USERID)) %>%
  add_markers() %>%
  layout(title = "User Behavior Building 2",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

## Phone ID Patterns
WifiTrainingData %>% 
  filter(BUILDINGID == 0) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~as.factor(PHONEID)) %>%
  add_markers() %>%
  layout(title = "Phone ID Pattern Building 0",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

WifiTrainingData %>% 
  filter(BUILDINGID == 1) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~as.factor(PHONEID)) %>%
  add_markers() %>%
  layout(title = "Phone ID Pattern Building 1",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

WifiTrainingData %>% 
  filter(BUILDINGID == 2) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~as.factor(PHONEID)) %>%
  add_markers() %>%
  layout(title = "Phone ID Pattern Building 2",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

### ---- Preprocessing ----
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

### ---- Subsetting ----
## Sampling Data by Building ID
traindata %>% filter(BUILDINGID == 0) -> Building0

traindata %>% filter(BUILDINGID == 1) -> Building1

traindata %>% filter(BUILDINGID == 2) -> Building2

### ---- Initial Modelization ----
## Creating Data Partition for Building "0"
set.seed(79)
intrain <- createDataPartition(y = Building0$LONGITUDE, 
                               p = 0.75, 
                               list = FALSE)
trainbuild0long <- Building0[intrain,]
testbuild0long <-Building0[-intrain,]

## Random Forest (Auto Grid) Model

# DO NOT RUN! takes too much effort and time. 

# We realize that we need to subset and sample the data more before modelization.

RFControl0Long <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
RFBuild0Long <- train(LONGITUDE ~ ., 
                      trainbuild0long,
                      method = "rf",
                      trControl = RFControl0Long,
                      tunelength = 2) # DO NOT RUN!
RFBuild0Long
RFPred0Long <- predict(RFBuild0Long, testbuild0long)
RFPred0Long
postResample(RFPred0Long, testbuild0long$LONGITUDE)