library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
sapply(list.files(pattern="[.]R$", path="Sept_2016/Functions", full.names=TRUE), source)


catch<-read.csv("Sept_2016/data/MNI catch 94_15.csv")%>%
  mutate(trip_no=(paste(date,vesselid,sep="_")))
 
   catch$common<-tolower(catch$common)
   
   table<-read.csv("Sept_2016/Data/spcommon.csv")
   
   catch<-merge(catch, table, by.x = "common", all.x = TRUE)
   


names(catch)
sapply(list.files(pattern="[.]R$", path="Sept_2016/Functions", full.names=TRUE), source)
#unique(catch$trip_no)
## fix hoload/holoma species id problem
catch[catch=="HOLOMA"]<-"HOLOAD"

table<-read.csv("Sept_2016/Data/spcommon.csv")

catch<-merge(catch, table, by.x = "common", all.x = TRUE)
View(catch2)
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

f_catch<-catch%>%
   group_by(year,family)%>% 
   summarize("sp_catch"=sum(weight_kgs))%>%
  arrange(desc(sp_catch))
  t<-sum(s$sp_catch)
 

sp<-catch%>%
  group_by(year,family)%>%
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
           mutate('family'=ifelse(cumsum>80,"other",as.character(family)))

sp3<-sp2%>%
  group_by(year,family)%>%
  summarize("catch"=sum(sp_catch),
            "perc"=sum(perc))%>%
  arrange(year,(family))
            
View(sp3)


group<-c("needlefish","squirrelfish","queen triggerfish","rock hind","parrotfish","cowfish","doctorfish","coney grouper","snapper","grunt","blue tang","rock beauty","ballyhoo","goatfish")

rb=c("red","#9DFF00"  , "#FF7600" ,"#FFEB00" , "#27FF00", "#00FF4E" , "#00C4FF","#FF00EB", "#004EFF" ,"#2700FF" ,"#9D00FF",  "#FF0076","#00FFC4")

sp_plot<-ggplot(data = sp3,aes(x =year, y = perc, fill = family)) + 
  geom_bar(stat="identity")+
  ylab("Percent of Total Catch (%)")+
  xlab("Year")+
  theme_bw()+
  theme(
        axis.title=element_text(face="bold")
        ,plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()
        ,axis.line.x = element_line(color="black", size = 0.6)
        ,axis.line.y = element_line(color="black", size = 0.6))+
    scale_y_continuous(expand=c(0,0))+
    scale_x_continuous(expand=c(0,0))+
 scale_fill_discrete(name="Family")
#####################################################################################################################################            
catch$gearid<-tolower(catch$gearid)
gears<-unique(catch$gearid)

s<-c("spear fishing","sgun","fish gun","speargun","dive")
p<-c("pot","pots","fish urn","pot ")
bs<-c("beach seine","bsne","beach seine net","bs net","cnet","ballyhoo net","gillnet","gill net","gnet","b.s. net")
l<-c("line","hlin","reel","plin","hand","line/ocean","slin","hli2")
m<-c("line & pot", "pot/line","line/pot","line & spear")
o<-c("","cadr","25","trwl","other")
catch$gearid[catch$gearid %in% s]<-"spear"
catch$gearid[catch$gearid %in% p]<-"pot"
catch$gearid[catch$gearid %in% bs]<-"beach seine"
catch$gearid[catch$gearid %in% l]<-"line"
catch$gearid[catch$gearid %in% m]<-"multiple gears"
catch$gearid[catch$gearid %in% o]<-"unknown"


gear_s<-catch%>%
  group_by(year,gearid)%>%
  summarize("catch"=sum(weight_kgs),
            "effort"=length(unique(trip_no)))%>%
  arrange(year,desc(catch))%>%
  mutate("total"=0,"total2"=0)
gear_s$total<-as.numeric(gear_s$total)

for (i in 1:nrow(gear_s)){
  y=gear_s$year[i]
  c<-gear_s[gear_s$year==y,]
  gear_s$total[i]<-sum(c$catch)
  gear_s$total2[i]<-sum(c$effort)
}

g2<-gear_s%>%
  mutate("perc"=catch/total*100,
         "cumsum"=cumsum(perc),
         "perc2"=effort/total2*100)%>%
  arrange(year,gearid)
# 
# gear_sum<-catch%>%
#   group_by(year,gearid)%>%
#   summarize("catch"=sum(weight_kgs),
#   "effort"=length(unique(trip_no)))
  

gear_plot<-ggplot(data = g2,aes(x =year, y = perc, fill = gearid)) + 
  geom_bar(stat="identity")+
  ylab("Percent of Total Catch (%)")+
  xlab("Year")+
  theme_bw()+
  theme(
    axis.title=element_text(face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color="black", size = 0.6)
    ,axis.line.y = element_line(color="black", size = 0.6))+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  scale_fill_discrete(name="Fishing Method")

gear_plot2<-ggplot(data = g2,aes(x =year, y = perc2, fill = gearid)) + 
  geom_bar(stat="identity")+
  ylab("Percent of Total Effort (%)")+
  xlab("Year")+
  theme_bw()+
  theme(
    axis.title=element_text(face="bold")
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,axis.line.x = element_line(color="black", size = 0.6)
    ,axis.line.y = element_line(color="black", size = 0.6))+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))+
  scale_fill_discrete(name="Fishing Method")






c<-palette()
scale_fill_discrete()
test <- data.frame(
  test1 = sample(letters[1:2], 100, replace = TRUE),
  test2 = sample(letters[3:8], 100, replace = TRUE)
)

sp_plot<-sp_plot+scale_fill_brewer(name = 'test2', breaks = 1:6, labels = levels(test$test2), palette = 'Set3')


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


