library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
catch<-read.csv("Sept_2016/data/MNI catch 94_15.csv")%>%
  mutate(trip_no=(paste(date,vesselid,sep="_")))%>%
catch$common<-tolower(catch$common)
str(catch)
names(catch)
sapply(list.files(pattern="[.]R$", path="Sept_2016/Functions", full.names=TRUE), source)
#unique(catch$trip_no)
## fix hoload/holoma species id problem
catch[catch=="HOLOMA"]<-"HOLOAD"



##########################################Annual############################################
#########################################################################################
sampling_days<-catch%>%
        group_by(year)%>%
        summarise(days=length(unique(date)))
        
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
  select(Year,Catch,Effort,CPUE,Sampling_Days)%>%
  melt(id="Year") 

levels(all_annual$variable)<-c("Catch (kgs)","Fishing Effort (trips)","CPUE (catch/effort)","Monitoring effort (days)")

pdf(file="Sept_2016/plots/annual_totals2.pdf")
a_plot<-ggplot(all_annual,aes(x=Year,y=value,colour=variable))+
  geom_line()+
  geom_point()+
  facet_wrap(~variable,scale="free_y",ncol=1)+
  xlab("Year")+
  ylab("Value")+
  #scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=c("J","F","M","A","M","J",
   #                                                                "J","A","S","O","N","D"))+
  theme_bw()+
  theme(legend.position="none",
        axis.title=element_text(face="bold"),
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.6),
        axis.line.y = element_line(color="black", size = 0.6))
print(a_plot)
dev.off()





catch<-ggplot(annual_totals,aes(x=Year,y=Catch))+
  geom_line(color="lightseagreen",lwd=1)+
  geom_point()+
  xlab("")+
  ylab("Catch (kgs)")+
  theme_bw() +
  theme(
    axis.title=element_text(face="bold"),
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.6),
        axis.line.y = element_line(color="black", size = 0.6),axis.text.x=element_blank())+
  coord_cartesian(ylim=c(0,50000),xlim=c(1994,2015))
 
# m_annual_totals<-annual_totals%>%
#   select(Year,Sampling_Days,Standardized_Catch)%>%
#   melt(id='Year')
#   
# ggplot(m_annual_totals)+
#   geom_line(aes(x=Year,y=value,linetype=variable),lwd=0.8)+
#   xlab("Year")+
#   ylab("")+
#   theme_bw() +
#   theme(legend.position="right",
#     axis.title=element_text(face="bold"),
#     plot.background = element_blank()
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#     ,panel.border = element_blank()) +
#   theme(axis.line.x = element_line(color="black", size = 0.6),
#         axis.line.y = element_line(color="black", size = 0.6))+
#   scale_linetype_manual(name="",values=c("dashed", "solid"),labels=c("No. of Sampling Days","Standardized Catch"))

  #scale_color_manual("",values=c('black','blue'),

effort<-ggplot(annual_totals,aes(x=Year,y=Effort))+
  geom_line(color="darkred",lwd=1,alpha=0.8)+
  geom_point()
  xlab("")+
  ylab("Effort (fishing trips)")+
  theme_bw() +
  theme(
    axis.title=element_text(face="bold"),
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.6),
        axis.line.y = element_line(color="black", size = 0.6), axis.text.x=element_blank())+
     coord_cartesian(ylim=c(0,1200),xlim=c(1994,2015))

cpue<-ggplot(annual_totals,aes(x=Year,y=CPUE))+
  geom_line(color="purple4",lwd=1,alpha=0.8)+
  geom_point()+
  xlab("Year")+
  ylab("CPUE (catch/effort)")+
  theme_bw() +
  theme(
    axis.title=element_text(face="bold"),
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.6),
        axis.line.y = element_line(color="black", size = 0.6))+
  coord_cartesian(ylim=c(0,70),xlim=c(1994,2015))

pdf(file="Sept_2016/plots/data_summary_plots.pdf")
multiplot(catch,effort,cpue, cols=1)
dev.off()

##########################################Seasonal############################################
##############################################################################################
catch$month<-as.factor(catch$month)
month_catch<-catch%>%
  group_by(month)%>%
  summarize('m_sum'=sum(weight_kgs))

monitoring<-catch%>%
  group_by(year,month)%>%
  summarize("tdays"=length(unique(date)))%>%
  ungroup()%>%
  group_by(month)%>%
  summarize("adays"=mean(tdays))
          
             

m_by_trip<-catch%>%
  group_by(year,month,trip_no)%>%
  summarize('trip_catch'=sum(weight_kgs))

month_effort<-m_by_trip%>%
  group_by(month)%>%
  summarize("m_effort"=length(trip_no),
             "m_cpue"=mean(trip_catch))
            
              #"m_sdev"=sd(trip_catch))

month<-cbind(month_effort,month_catch$m_sum,month_catch$m_days)%>%
  melt(id="month")

neworder <- c("month_catch$m_sum", "m_effort", "m_cpue","month_catch$m_days")
month2 <- arrange(transform(month,
                           variable=factor(variable,levels=neworder)),variable)
month2$month<-as.numeric(month2$month)
levels(month2$variable)<-c("Catch (kgs)","Effort (trips)","CPUE (catch/effort)","Monitoring effort (days)")

pdf(file="Sept_2016/plots/montly_plots.pdf")
m_plot<-ggplot(month2,aes(x=month,y=value,colour=variable))+
  geom_line()+
  geom_point()+
  facet_wrap(~variable,scale="free_y",ncol=1)+
  xlab("Month")+
  ylab("Value")+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=c("J","F","M","A","M","J",
                                                                "J","A","S","O","N","D"))+
  theme_bw()+
  theme(legend.position="none",
    axis.title=element_text(face="bold"),
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.6),
        axis.line.y = element_line(color="black", size = 0.6))

print(m_plot)
dev.off()

#############################################################################################
###byspecies#######################################################################

a_catch<-catch%>%
  group_by(year)%>%
  summarize("catch"=sum(weight_kgs))

s<-catch%>%
   group_by(year,common)%>% 
   summarize("sp_catch"=sum(weight_kgs))%>%
  arrange(desc(sp_catch))
  t<-sum(s$sp_catch)
 
  
View(s)
sp<-catch%>%
  group_by(year,common)%>%
  summarize("sp_catch"=sum(weight_kgs))%>%
  arrange(year,desc(sp_catch))%>%
  mutate("total"=0)
sp$total<-as.numeric(sp$total)

for (i in 1:nrow(sp)){
 y=sp$year[i]
 c<-a_catch[a_catch$year==y,]
sp$total[i]<-c$catch
}

common<-unique(sp$common)
write.csv(common,file="Sept_2016/Data/spcommon.csv")

sp2<-sp%>%
  mutate("perc"=sp_catch/total*100,
          "cumsum"=cumsum(perc))%>%
           mutate('species'=ifelse(cumsum>80,"other",as.character(common)))



sp3<-sp2%>%
  group_by(year,species)%>%
  summarize(catch=sum(sp_catch))%>%
  arrange(year,desc(catch))
View(sp3)
names(sp2)

group<-c("needlefish","squirrelfish","queen triggerfish","rock hind","parrotfish","cowfish","doctorfish","coney grouper","snapper","grunt","blue tang","rock beauty","ballyhoo","goatfish")



sp_plot<-ggplot(data = sp3,aes(x =year, y = catch, fill = species)) + 
  geom_bar(stat="identity")



sp_id<-c("BELO","ACANCH","ACANCO","BALIVE","HOLOAD","LUTJMA","LUTJSY","LUTJVI","SCARCH","SERRGU")
totals2<-melt(totals,id=("year"))%>%
  filter(variable=="total_trip")
ggplot(data=totals2, aes(x=year,y=value,colour=variable))+
  geom_line()
         
#2. Make figure showing break down of species and types of gear

#3 Make catch, effort and cpue plots for target species

# catch<-read.csv("Sept_2016/data/MNI catch 94_15.csv")%>%
#   mutate(trip_no=(paste(date,vesselid,sep="_")))
# 
# sp_id<-c("BELO","ACANCH","ACANCO","BALIVE","HOLOAD","LUTJMA","LUTJSY","LUTJVI","SCARVI",SERRGU")

target_catch<-catch%>%
  filter(id10%in%sp_id)%>%
  arrange(id10,year)%>%
  group_by(id10,year)%>%
  summarise(catch=sum(weight_kgs))%>%
  ungroup()%>%
  as.data.frame()

common<-c(
  ACANCH="Doctorfish",
  ACANCO="Blue Tang",
  BALIVE="Old wife",
  BELO="Gar",
  HOLOAD="Squirrelfish",
  LUTJMA="Mahogany snapper",
  LUTJSY="Lane snapper",
  LUTJVI="Silk snapper",
  SCARCH="Stoplight parrotfish",
  SERRGU="Red hind")

#NEED TO FIX HOLOAD!  
pdf("Sept_2016/plots/target_species_catch.pdf",width=11)
p<-ggplot(data=target_catch, aes(x=year,y=catch))+
 geom_line()+
  xlab("Year")+
  ylab("Catch (kgs)")+
  theme_bw() +
  theme(
    axis.title=element_text(face="bold"),
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.6),
        axis.line.y = element_line(color="black", size = 0.6))
p+facet_wrap(~id10,scale="free_y",labeller=labeller(id10=common))
dev.off() 

i=1
for (i in 1:length(sp_id)){
  c<-target_catch%>%
    filter(id10==sp_id[i])%>%
    write.csv(paste("Sept_2016/Data/",sp_id[i],"_catch.csv",sep=""))
}



##scratch this bc we need to estimate 0 catch effort too! strange that pots are showing up for belo 
#   trip_level<-catch%>%
#     filter(spp_groupid=='BELO')%>%
#     group_by(year,trip_no)%>%
#     select(gearid,spp_groupid,weight_kgs,trip_no)%>%
#     summarise(cpue=sum(weight_kgs),
#                trips=length(weight_kgs))%>%
#     ungroup()%>%
#     group_by(year)%>%
#       summarise(avg_cpue=mean(cpue),
#                 catch=sum(cpue),
#                 effort=sum(trips))
# 
  



# 
# p<-ggplot(data=totals, aes(x=year,y=value))+geom_line()
# p+facet_wrap(~variable,scales="free_y", ncol=1)
# 
# 
# trip_level$catch/trip_level$effort
# #5 Make gear and area breakdwon for target species
# #6 Make length plots 
#7 Length and catch based assessments


