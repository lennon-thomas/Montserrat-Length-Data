library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
catch<-read.csv("Sept_2016/data/MNI catch 94_15.csv")%>%
  mutate(trip_no=(paste(date,vesselid,sep="_")))
View(catch)
names(catch)
names(catch)
b<-unique(catch$trip_no)
catch$id9<-tolower(catch$id9)
catch$gearid<-tolower(catch$gearid)
## fix hoload/holoma species id problem
catch[catch=="HOLOMA"]<-"HOLOAD"
length(b)