library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
catch<-read.csv("Sept_2016/data/MNI catch 94_15.csv")%>%
  mutate(trip_no=(paste(date,vesselid,sep="_")))
View(catch)
names(catch)

## fix hoload/holoma species id problem
catch[catch=="HOLOMA"]<-"HOLOAD"


#1. Make 3 panelled graph showing total catch, cpue and effort
## simple data summaries maybe scratch cpue because doesn't include non zeros?
trip_level<-catch%>%
  group_by(year,trip_no)%>%
  summarise(cpue=sum(weight_kgs),
            t_catch=sum(weight_kgs))

  
totals<-trip_level%>%
  group_by(year)%>%
  summarise(avg_cpue=mean(cpue),
            tot_catch=sum(t_catch),
            total_trip=length(trip_no))
  


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
# totals<-melt(trip_level,id='year')
  



# 
# p<-ggplot(data=totals, aes(x=year,y=value))+geom_line()
# p+facet_wrap(~variable,scales="free_y", ncol=1)
# 
# 
# trip_level$catch/trip_level$effort
# #5 Make gear and area breakdwon for target species
# #6 Make length plots 
#7 Length and catch based assessments


