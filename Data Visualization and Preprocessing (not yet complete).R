### --------------------- Wifi Locationing --------------------- ###
### -------------------- by Alican Tanaçan --------------------- ###
### ----- Version 3: Data Visualization and Preprocessing ------ ###

### ---- Libraries ----
library(dplyr)
library(tidyverse)
library(caret)
library(lubridate)
library(naniar)
library(devtools)
library(ggplot2)
library(plotly)

### ---- Import Wifi Training and Validation Data ----
WifiTrainingData <- read.csv("WifitrainingData.csv")

WifiValidationData <- read.csv("WifivalidationData.csv")

WifiData <- rbind(WifiTrainingData, WifiValidationData)

### ---- Data Visualization ----
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

### ---- Preprocessing ----
# Create a data frame for WAPS
WAPS <- WifiTrainingData[,1:520]

# Take means of all WAPS
WAPSmean <- data.frame(Mean = apply(WAPS, 2, mean))