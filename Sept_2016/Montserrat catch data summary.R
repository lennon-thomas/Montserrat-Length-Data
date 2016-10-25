rm(list=ls())
library(dplyr)
setwd("C:/Users/lthomas/GitHub/DPSA/Montserrat")
data<-read.csv("Montserrat all catch.csv")
data$land_wt_kg[1:10]
names(data)
unique(data$Year)
data$land_wt_kg<-as.numeric(as.character(data$land_wt_kg))
data$Year<-as.factor(data$Year)
data$common<-toupper(data$common)
data%>%
  group_by(data$Year)%>%
  summarise(catch=sum(land_wt_kg,na.rm=TRUE))%>%
  arrange(catch)
?arrange
totals<-tapply(data$land_wt_kg,data$Year,sum,rm.na=TRUE)
totals
data$yeartotal
species<-unique(data$common)
len_spec<-length(unique(data$common))
data$Year<-as.factor(data$Year)
# for (in in 1:len_spec){
   filter_catch<-data%>%
     filter(data$common=="BLUE TANG")%>%
     (filter_catch$Year=="1994")
filter_catch[4000:5000,] 
catch<-sum(filter_catch$land_wt_kg)
tapply(filter_catch$land_wt_kg,filter_catch$Year,sum)
totals[,"SNAPPER,MAHOGANY"]
data$common[2000:3000,"SNAPPER,MAHOGANY"]
species[300:400]
