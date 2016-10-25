library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
sapply(list.files(pattern="[.]R$", path="Sept_2016/Functions", full.names=TRUE), source)


catch<-read.csv("Data/MNI catch 94_15.csv")%>%
  mutate(trip_no=(paste(date,vesselid,sep="_")))

catch$common<-tolower(catch$common)

table<-read.csv("Data/spcommon.csv")

catch<-merge(catch, table, by.x = "common", all.x = TRUE)

View(table)

names(catch)
sapply(list.files(pattern="[.]R$", path="Functions", full.names=TRUE), source)
#unique(catch$trip_no)
## fix hoload/holoma species id problem
catch[catch=="HOLOMA"]<-"HOLOAD"

# table<-read.csv("Sept_2016/Data/spcommon.csv")
# 
# catch<-merge(catch, table, by.x = "common", all.x = TRUE)
# View(catch2)
##########################################Annual############################################
#########################################################################################
sampling_days<-catch%>%
  group_by(year)%>%
  summarise(days=length(unique(date)))

avg_days_sampled=mean(sampling_days$days)

days<-ggplot(sampling_days,aes(x=year,y=days))+
  geom_line(lwd=1,col="blue")+
  theme_bw()+
  theme(legend.position="none",
        axis.title=element_text(),
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.4),
        axis.line.y = element_line(color="black", size = 0.4))+
  scale_x_continuous(limits=c(1993,2016),expand=c(0,0))+
  scale_y_continuous(limits=c(150,300),expand=c(0,0))+
  xlab("Year")+
  ylab("No. of Monitoring Days")
ggsave("plots/monitoring days.png",days, dpi=400)
?ggsave    
total_effort<-catch%>%
  group_by(year)%>%
  summarise(effort=length(unique(trip_no)))

total_catch<-catch%>%
  group_by(year)%>%
  summarise(catch=sum(weight_kgs))

annual_totals<-cbind(sampling_days,total_effort$effort,total_catch$catch)
colnames(annual_totals)<-c("Year","Sampling_Days","Effort","Catch")

annual_totals<-annual_totals%>%
  mutate('CPUE'=Catch/Effort)%>%
  mutate('Standardized_Catch'= Catch/Sampling_Days)#%>%

#select(Year,Catch,Standaridzed_Catch,Fishing Effort,CPUE,Sampling_Days)
all_annual<-annual_totals%>%
  select(Year,Catch,Sampling_Days,Standardized_Catch)%>%
  melt(id="Year") 

#levels(all_annual$variable)<-c("Monitoring effort (days)","Catch (kgs)","Standardized Catch")

all_days<-all_annual%>%
  filter(variable=="Sampling_Days")

m_plot<-ggplot(all_days,aes(x=Year,y=value,colour=variable))+
  geom_line(lwd=1.1,color="seagreen")+
  # geom_point()+
  #  facet_wrap(~variable,scale="free_y",ncol=1)+
  xlab("Year")+
  ylab("No. of Monitoring Days")+
  #scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=c("J","F","M","A","M","J",
  #                                                                "J","A","S","O","N","D"))+
  theme_bw()+
  theme(legend.position="none",
        axis.title=element_text(face="bold"),
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.4),
        axis.line.y = element_line(color="black", size = 0.4))

ggsave("plots/monitoring_days.png",m_plot,dpi=300)

all_catch<-all_annual%>%
  filter(variable=="Catch")

c_plot<-ggplot(all_catch,aes(x=Year,y=value,colour=variable))+
  geom_line(lwd=1.1)+
  # geom_point()+
  #  facet_wrap(~variable,scale="free_y",ncol=1)+
  xlab("Year")+
  ylab("Catch (kgs)")+
  
  scale_y_continuous(limits=c(0,50000),expand=c(0,0))+                                                                
  theme_bw()+
  theme(legend.position="none",
        axis.title=element_text(face="bold"),
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.4),
        axis.line.y = element_line(color="black", size = 0.4))

ggsave("plots/total_catch.png",c_plot,dpi=300)



