### ---------------------- Wifi Locationing ---------------------- ###
### --------------------- by Alican Tanaçan ---------------------- ###
### -------- Version 7: Investigating Patterns on Raw Data ------- ###

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

### ---- Preprocessing ----
## Change the Timestamp Data Type
WifiTrainingData %>%
  mutate_at(c("TIMESTAMP"), as.character) %>% 
  mutate_at(c("TIMESTAMP"), as.numeric) -> WifiTrainingData

WifiValidationData %>% 
  mutate_at(c("TIMESTAMP"), as.character) %>% 
  mutate_at(c("TIMESTAMP"), as.numeric) -> WifiValidationData

## Convert Unix Time
WifiTrainingData$TIMESTAMP <- as.POSIXct((WifiTrainingData$TIMESTAMP),
                                         origin="1970-01-01", 
                                         tz="GMT")

WifiValidationData$TIMESTAMP <- as.POSIXct((WifiValidationData$TIMESTAMP),
                                           origin="1970-01-01", 
                                           tz="GMT")

### ---- Data Exploration & Visualization ----
## Variable Analysis
WAPStrain <- WifiTrainingData[,1:520]
WAPStrain[WAPStrain == 100] <- -105
WAPSrowmeans <- data.frame(rowMeans(WAPStrain, na.rm = FALSE, dims = 1))
WAPSwithTIME <- cbind(WAPSrowmeans, 
                      WifiTrainingData$USERID,
                      WifiTrainingData$PHONEID,
                      WifiTrainingData$BUILDINGID,
                      WifiTrainingData$FLOOR,
                      WifiTrainingData$TIMESTAMP)
colnames(WAPSwithTIME) <- c("signal", 
                            "user", 
                            "phone",
                            "building",
                            "floor",
                            "time")

WAPSwithTIME %>% 
  group_by(day = day(time), user, phone, building, floor) %>% 
  count(user) -> WAPSwithTIME2

WAPSwithTIME %>% 
  group_by(date = date(time), user, phone, building, floor) %>% 
  count(user) -> WAPSwithTIME3

write.csv(WAPSwithTIME3, file = "wifigroupedwithtime.csv")

# Signals by Time Visualizations
WAPSwithTIME %>% 
  filter(day(time) == 30) %>% 
  ggplot(aes(time)) +
  geom_line(aes(y = signal)) +
  labs(title = "30 May 2013 Thursday - Signals by Time") -> plot30may

WAPSwithTIME %>% 
  filter(day(time) == 31) %>% 
  ggplot(aes(time)) +
  geom_line(aes(y = signal))  +
  labs(title = "31 May 2013 Friday - Signals by Time") -> plot31may

WAPSwithTIME %>% 
  filter(day(time) == 4) %>% 
  ggplot(aes(time)) +
  geom_line(aes(y = signal)) +
  labs(title = "4 Jun 2013 Thursday - Signals by Time") -> plot4jun

WAPSwithTIME %>% 
  filter(day(time) == 10) %>% 
  ggplot(aes(time)) +
  geom_line(aes(y = signal)) +
  labs(title = "10 Jun 2013 Wednesday - Signals by Time") -> plot10jun

WAPSwithTIME %>% 
  filter(day(time) == 12) %>% 
  ggplot(aes(time)) +
  geom_line(aes(y = signal)) +
  labs(title = "12 Jun 2013 Friday - Signals by Time") -> plot12jun

WAPSwithTIME %>% 
  filter(day(time) == 20) %>% 
  ggplot(aes(time)) +
  geom_line(aes(y = signal)) +
  labs(title = "20 Jun 2013 Saturday - Signals by Time") -> plot20jun

grid.arrange(plot30may,
             plot31may,
             plot4jun,
             plot10jun,
             plot12jun,
             plot20jun)


## Floor Analysis
WifiTrainingData %>% 
  filter(BUILDINGID == 0) %>% 
  group_by(FLOOR) %>% 
  ggplot(aes(LONGITUDE, LATITUDE)) +
  geom_point(color = "brown") +
  facet_wrap(~FLOOR) +
  labs(title = "Building 0 by Floor")

WifiTrainingData %>% 
  filter(BUILDINGID == 1) %>% 
  group_by(FLOOR) %>% 
  ggplot(aes(LONGITUDE, LATITUDE)) +
  geom_point(color = "brown") +
  facet_wrap(~FLOOR) +
  labs(title = "Building 1 by Floor")

WifiTrainingData %>% 
  filter(BUILDINGID == 2) %>% 
  group_by(FLOOR) %>% 
  ggplot(aes(LONGITUDE, LATITUDE)) +
  geom_point(color = "brown") +
  facet_wrap(~FLOOR) +
  labs(title = "Building 2 by Floor")

## PhoneID Analysis
WifiTrainingData %>% 
  filter(BUILDINGID == 0, PHONEID == 13) %>%
  group_by(FLOOR) %>% 
  ggplot(aes(LONGITUDE, LATITUDE)) +
  geom_point(color = "brown") +
  facet_wrap(~FLOOR) +
  labs(title = "Building 0 PhoneID 13 by Floor")

WifiTrainingData %>% 
  filter(BUILDINGID == 0, PHONEID == 14) %>%
  group_by(FLOOR) %>% 
  ggplot(aes(LONGITUDE, LATITUDE)) +
  geom_point(color = "brown") +
  facet_wrap(~FLOOR) +
  labs(title = "Building 0 PhoneID 14 by Floor")

WifiTrainingData %>% 
  filter(BUILDINGID == 1, PHONEID == 6) %>%
  group_by(FLOOR) %>% 
  ggplot(aes(LONGITUDE, LATITUDE)) +
  geom_point(color = "brown") +
  facet_wrap(~FLOOR) +
  labs(title = "Building 1 PhoneID 6 by Floor")

WifiTrainingData %>% 
  filter(BUILDINGID == 1, PHONEID == 7) %>%
  group_by(FLOOR) %>% 
  ggplot(aes(LONGITUDE, LATITUDE)) +
  geom_point(color = "brown") +
  facet_wrap(~FLOOR) +
  labs(title = "Building 1 PhoneID 7 by Floor")

WifiTrainingData %>% 
  filter(BUILDINGID == 1, PHONEID == 8) %>%
  group_by(FLOOR) %>% 
  ggplot(aes(LONGITUDE, LATITUDE)) +
  geom_point(color = "brown") +
  facet_wrap(~FLOOR) +
  labs(title = "Building 1 PhoneID 8 by Floor")

WifiTrainingData %>% 
  filter(BUILDINGID == 1, PHONEID == 10) %>%
  group_by(FLOOR) %>% 
  ggplot(aes(LONGITUDE, LATITUDE)) +
  geom_point(color = "brown") +
  facet_wrap(~FLOOR) +
  labs(title = "Building 1 PhoneID 10 by Floor")

WifiTrainingData %>% 
  filter(BUILDINGID == 1, PHONEID == 13) %>%
  group_by(FLOOR) %>% 
  ggplot(aes(LONGITUDE, LATITUDE)) +
  geom_point(color = "brown") +
  facet_wrap(~FLOOR) +
  labs(title = "Building 1 PhoneID 13 by Floor")

WifiTrainingData %>% 
  filter(BUILDINGID == 1, PHONEID == 14) %>%
  group_by(FLOOR) %>% 
  ggplot(aes(LONGITUDE, LATITUDE)) +
  geom_point(color = "brown") +
  facet_wrap(~FLOOR) +
  labs(title = "Building 1 PhoneID 14 by Floor")

WifiTrainingData %>% 
  filter(BUILDINGID == 1, PHONEID == 17) %>%
  group_by(FLOOR) %>% 
  ggplot(aes(LONGITUDE, LATITUDE)) +
  geom_point(color = "brown") +
  facet_wrap(~FLOOR) +
  labs(title = "Building 1 PhoneID 17 by Floor")

WifiTrainingData %>% 
  filter(BUILDINGID == 1, PHONEID == 18) %>%
  group_by(FLOOR) %>% 
  ggplot(aes(LONGITUDE, LATITUDE)) +
  geom_point(color = "brown") +
  facet_wrap(~FLOOR) +
  labs(title = "Building 1 PhoneID 18 by Floor")

WifiTrainingData %>% 
  filter(BUILDINGID == 1, PHONEID == 22) %>%
  group_by(FLOOR) %>% 
  ggplot(aes(LONGITUDE, LATITUDE)) +
  geom_point(color = "brown") +
  facet_wrap(~FLOOR) +
  labs(title = "Building 1 PhoneID 22 by Floor")

WifiTrainingData %>% 
  filter(BUILDINGID == 1, PHONEID == 23) %>%
  group_by(FLOOR) %>% 
  ggplot(aes(LONGITUDE, LATITUDE)) +
  geom_point(color = "brown") +
  facet_wrap(~FLOOR) +
  labs(title = "Building 1 PhoneID 23 by Floor")

## -30 to 0 dBm Analysis
# Outliers on Train
WifiOutData <- WifiTrainingData
WifiOutData[WifiOutData == 100] <- -105
WAPSout <- apply(WifiOutData %>% select(starts_with("WAP")),1, max) > -30
sum(WAPSout)
WAPSout30 <- WifiTrainingData[WAPSout,] # 492 outliers

WAPSout30 %>% 
  group_by(BUILDINGID, FLOOR, USERID, PHONEID) %>% 
  count(USERID) # UserID 6

# User 6 Visualization
WifiTrainingData %>% 
  filter(USERID == 6) %>% 
  plot_ly(x = ~LONGITUDE, 
          y = ~LATITUDE, 
          z = ~as.factor(FLOOR), 
          color = ~as.factor(USERID)) %>%
  add_markers() %>%
  layout(title = "User 6 Behavior",
         scene = list(xaxis = list(title = "Longitude"),
                      yaxis = list(title = "Latitude"),
                      zaxis = list(title = "Floor")))

CleanWifiData <- WifiTrainingData[!WAPSout,]
CleanWifiData[CleanWifiData == 100] <- -105

# Save the clean data for modelization
saveRDS(CleanWifiData, file = "CleanWifiTrainData.rds")

# Outliers on Valid 
ValidOutData <- WifiValidationData
ValidOutData[ValidOutData == 100] <- -105
Validout <- apply(ValidOutData %>% select(starts_with("WAP")),1, max) > -30
sum(Validout) # there are no outliers
Validout30 <- WifiTrainingData[Validout,]
