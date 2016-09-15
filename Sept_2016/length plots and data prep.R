##Length data plot and data prep

library(dplyr)
library(readr)

#Data Summary Length Data

data <- read.csv("Sept_2016/Data/Montserrat_Species_Length_Composition_Data_July_2016_AS.csv")
names(data)
days <- length(unique(data$Date))
boats <- length(unique(data$Vessel.ID..Length))
area <- length(unique(data$Area.fished))
#unique(df[c("data$Date","data$Vessel.ID..Length")])
trips <- data.frame(data$Date,data$Vessel.ID..Length)
unique_trips <- unique(trips)
nrow(unique_trips)
nrow(trips)
gears<-unique(data$Gear.Type)
species_CN <- unique(lapply(data$COMMON_NAME, tolower))