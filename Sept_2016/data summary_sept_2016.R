library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
catch<-read.csv("data/MNI catch 94_15.csv")%>%
  mutate(trip_no=(paste(date,vesselid,sep="_")))%>%
  filter(spp_groupid=="BELO")




## simple data summaries
trip_level<-catch%>%
  group_by(year,trip_no)%>%
  summarise(cpue=sum(weight_kgs),
            t_catch=sum(weight_kgs))

  
totals<-trip_level%>%
  group_by(year)%>%
  summarise(avg_cpue=mean(cpue),
            tot_catch=sum(t_catch),
            total_trip=length(trip_no))
  
sp_id<-c("BELO","ACANCH","ACANCO","BALIVE","HOLOAD","LUTJMA","LUTJSY","LUTJVI","SERRGU")
totals2<-melt(totals,id=("year"))%>%
  filter(variable=="total_trip")
ggplot(data=totals2, aes(x=year,y=value,colour=variable))+
  geom_line()
         

