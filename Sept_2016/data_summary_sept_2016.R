library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
detach(package:plyr)
library(scales)
sapply(list.files(pattern="[.]R$", path="Sept_2016/Functions", full.names=TRUE), source)


catch<-read.csv("Data/MNI catch 94_15.csv")%>%
  mutate(trip_no=(paste(date,vesselid,sep="_")))

catch$common<-tolower(catch$common)

table<-read.csv("Data/spcommon.csv")

catch<-merge(catch, table, by.x = "common", all.x = TRUE)
write.csv(catch,"Data/MNI catch 94_15_fam.csv")
View(table)
catch$year
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
  summarise(days=length(unique(date)))%>%
  ungroup()

avg_days_sampled=mean(sampling_days$days)

days<-ggplot(sampling_days,aes(x=year,y=days))+
  geom_line(lwd=1,col="darkgreen")+
  theme_bw()+
  theme(legend.position="none",
        axis.title=element_text(face="bold",size=16),
        axis.text=element_text(size=14),
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.4),
        axis.line.y = element_line(color="black", size = 0.4))+
  scale_x_continuous(limits=c(1993,2016),expand=c(0,0))+
  scale_y_continuous(limits=c(0,300),expand=c(0,0))+
  xlab("Year")+
  ylab("No. of Catch Monitoring Days")
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
  select(Year,Catch,Sampling_Days,Standardized_Catch,CPUE)%>%
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
max(s_catch$value,na.rm=TRUE)
c_plot<-ggplot(all_catch,aes(x=Year,y=value,colour=variable))+
  geom_line(lwd=1.1)+
  # geom_point()+
  #  facet_wrap(~variable,scale="free_y",ncol=1)+
  xlab("Year")+
  ylab("Catch (kgs)")+
  
  scale_y_continuous(limits=c(0,50000),expand=c(0,0),labels=comma)+                                                                
  theme_bw()+
  theme(legend.position="none",
        axis.title=element_text(face="bold",size=16),
        axis.text=element_text(size=14),
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.4),
        axis.line.y = element_line(color="black", size = 0.4))

ggsave("plots/total_catch.png",c_plot,dpi=300)


s_catch<-all_annual%>%
  filter(variable=="Standardized_Catch")

s_plot<-ggplot(s_catch,aes(x=Year,y=value,colour=variable))+
  geom_line(lwd=1.1,color="blue")+
  # geom_point()+
  #  facet_wrap(~variable,scale="free_y",ncol=1)+
  xlab("Year")+
  ylab("Standardized Catch")+
  
  scale_y_continuous(limits=c(0,250),expand=c(0,0),labels=comma)+                                                                
  theme_bw()+
  theme(legend.position="none",
        axis.title=element_text(face="bold",size=16),
        axis.text=element_text(size=14),
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.4),
        axis.line.y = element_line(color="black", size = 0.4))

ggsave("plots/s_catch.png",c_plot,dpi=300)



e_plot<-ggplot(total_effort,aes(x=year,y=effort))+
  geom_line(lwd=1.1,color="navy")+
  # geom_point()+
  #  facet_wrap(~variable,scale="free_y",ncol=1)+
  xlab("Year")+
  ylab("Effort (fishing trips)")+
  scale_y_continuous(expand=c(0,0),labels=comma)+                                                                
  theme_bw()+
  theme(legend.position="none",
        axis.title=element_text(face="bold",size=16),
        axis.text=element_text(size=14),
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.4),
        axis.line.y = element_line(color="black", size = 0.4))
ggsave("plots/effort.png",e_plot,dpi=300)
min(total_effort$effort)

cpue<-all_annual%>%
  filter(variable=="CPUE")

s_cpue<-cbind(cpue,all_days)
colnames(s_cpue)<-c("Year","cpue","var1","Year","days","var2")
stand_cpue<-(s_cpue$var1/s_cpue$var2)*200
stand_cpue<-cbind(s_cpue$Year,stand_cpue)
colnames(stand_cpue)<-c("Year2","standarized cpue")
stand_cpue<-as.data.frame(stand_cpue)
keep<-c("value","standarized cpue")
cpue2<-cbind(cpue,stand_cpue)
 cpue2<-  cpue2%>%
  melt(id="Year")%>%
  filter(variable %in% keep)
cpue2$value<-as.numeric(cpue2$value)
CPUE_plot<-ggplot(cpue2,aes(x=Year,y=value,colour=variable))+
  geom_line(aes(linetype=variable),lwd=1.1)+
  #geom_line(aes(y=standarized cpue),lwd=1.1,color="orange",lty="dashed")+
  #geom_line(stand_cpue,aes(y=cpue),lwd=1.1,color="pink")+
  # geom_point()+
  #  facet_wrap(~variable,scale="free_y",ncol=1)+
  xlab("Year")+
  ylab("CPUE (kgs/trips)")+
  scale_y_continuous(limits=c(0,80),expand=c(0,0),labels=comma)+                                                                
  theme_bw()+
  theme(legend.position="right",
        legend.title=element_text(size=16),
        legend.text=element_text(size=14),
        axis.title=element_text(face="bold",size=16),
        axis.text=element_text(size=14),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.4),
        axis.line.y = element_line(color="black", size = 0.4))
CPUE_plot<- CPUE_plot+scale_color_manual(name="Index",labels=c("nominal","standardized"),values=c("darkblue","orange"))+
  scale_linetype_manual(values=c("solid","dashed"))+
  guides(linetype =FALSE) 
  
s_cpue<-cbind(cpue,all_days)
colnames(s_cpue)<-c("Year","cpue","var1","Year","days","var2")
stand_cpue<-(s_cpue$var1/s_cpue$var2)*200
stand_cpue<-cbind(s_cpue$Year,s_cpue$days,stand_cpue)
colnames(stand_cpue)<-c("Year","variable","cpue")
stand_cpue<-as.data.frame(stand_cpue)
CPUE_plot<-CPUE_plot+
  geom_line(data=stand_cpue,aes(y=cpue),lwd=1.1,color="orange",lty="dashed")+
  scale_colour_manual(values=c(value="darkblue",cpue="orange"))

ggsave("plots/cpue.png",CPUE_plot,dpi=300)


s_cpue_plot<-ggplot(stand_cpue,aes(x=Year,y=cpue))+
  geom_line(lwd=1.1,color="pink")+
  # geom_point()+
  #  facet_wrap(~variable,scale="free_y",ncol=1)+
  xlab("Year")+
  ylab("Standardized CPUE")+
  #scale_y_continuous(limits=c(0,0.5),expand=c(0,0),labels=comma)+                                                                
  theme_bw()+
  theme(legend.position="none",
        axis.title=element_text(face="bold",size=16),
        axis.text=element_text(size=14),
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.4),
        axis.line.y = element_line(color="black", size = 0.4))

