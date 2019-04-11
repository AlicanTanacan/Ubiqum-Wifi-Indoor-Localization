### ---------------------- Wifi Locationing ---------------------- ###
### --------------------- by Alican Tanaçan ---------------------- ###
### ---- Version 3: Adjusting Signal Quality and Correlations ---- ###

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

### ---- Adjusting Signal Quality ----
## Create a data frame for WAPS and rest of the VARIABLES
WAPStrain <- WifiTrainingData[,1:520]

VARIABLEStrain <- WifiTrainingData[, 521:529]

WAPSvalid <- WifiValidationData[,1:520]

VARIABLESvalid <- WifiValidationData[, 521:529]

## Adjusting signal quality 
WAPStrainPoorSignalCol <- apply(WAPStrain, 2, mean) >= 99.8 
# Decrease this number by decimal to increase signal quality

WAPStrainPoorSignalRow <- apply(WAPStrain, 1, mean) >= 99.8 
# Decrease this number by decimal to increase signal quality

WAPSvalidPoorSignalCol <- apply(WAPSvalid, 2, mean) >= 99.8 
# Decrease this number by decimal to increase signal quality

WAPSvalidPoorSignalRow <- apply(WAPSvalid, 1, mean) >= 99.8 
# Decrease this number by decimal to increase signal quality

## Removing poor signal columns and rows
WAPStrainGoodSignal <- WAPStrain[!WAPStrainPoorSignalRow, 
                                 !WAPStrainPoorSignalCol]

WAPSvalidGoodSignal <- WAPSvalid[!WAPSvalidPoorSignalRow, 
                                 !WAPSvalidPoorSignalCol]

## Equalizing the column amount in both WAPS data sets
WAPStrain2 <- WAPStrainGoodSignal[, which(colnames(WAPStrainGoodSignal) %in% 
                                            colnames(WAPSvalidGoodSignal))]

WAPSvalid2 <- WAPSvalidGoodSignal[, which(colnames(WAPSvalidGoodSignal) %in% 
                                            colnames(WAPStrainGoodSignal))]

## Equalizing the row amount in both VARIABLE data sets
VARIABLEStrain2 <- VARIABLEStrain[!WAPStrainPoorSignalRow, ]

VARIABLESvalid2 <- VARIABLESvalid[!WAPSvalidPoorSignalRow, ]
  
## Combining the WAPS and VARIABLES data
AdjTrainingData <- cbind(WAPStrain2, VARIABLEStrain2)

AdjValidationData <- cbind(WAPSvalid2, VARIABLESvalid2)

### ---- Checking Correlations ----
ggcorr(VARIABLEStrain, label = T)

ggcorr(VARIABLESvalid, label = T)

## Removing unrelated variables
ReadyTrain <- subset(AdjTrainingData, select = -c(SPACEID:PHONEID))

ReadyValid <- subset(AdjValidationData, select = -c(SPACEID:PHONEID))

### ---- Saving the Preprocessed Data Sets ----
saveRDS(ReadyTrain, file = "PreprocessedWifiTraining.rds")

saveRDS(ReadyValid, file = "PreprocessedWifiValidation.rds")
