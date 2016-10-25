library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
library(scales)
catch<-read.csv("Data/MNI catch 94_15.csv")%>%
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
test<-catch%>%
  filter(year=="2015")
v<-unique(test$fishername)
names(test)
unique(catch$gearid)
pot<-c("pots","pot/line","line/pot","line & pot","pot ","fish urn")
line<-c("25","hlin","plin","line/ocean","hand","hli2","line & spear","reel","slin")
net<-c("beach seine","cnet","bs net", "beach seine net", "gill net", "gnet","gillnet","b.s. net",
       "ballyhoo net","trwl","bsne","cadr")
spear<-c("spear fishing", "sgun","fish gun","speargun","dive")

catch$gearid[catch$gearid %in% pot]<-"pot"
catch$gearid[catch$gearid %in% line]<-"line"
catch$gearid[catch$gearid %in% net]<-"net"
catch$gearid[catch$gearid %in% spear]<-"spear"

unique(catch$gearid)

write.csv(catch,"Data/MNI catch_cleangear.csv")
t<-sum(catch$weight_kgs,na.rm=TRUE)
dat<-catch%>%
  group_by(year,gearid)%>%
  summarise(c=sum(weight_kgs,na.rm=TRUE))%>%
  ungroup()%>%
  arrange(year,desc(gearid))

gear<-dat%>%
  group_by(gearid)%>%
  summarise(catch=sum(c,na.rm=TRUE),
            perc=catch/t*100)
g<-c("pot","net","line","spear")
dat$gearid<-factor(dat$gearid,as.character(g))
dat<-dat%>%
  arrange(year,gearid)
View(dat)

gear_plot<-ggplot(dat,aes(x=year,y=c,fill=gearid))+
  geom_bar(stat="identity")+
  theme_bw()+
  xlab("Year")+
  ylab("Catch (kgs)")+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  theme(axis.title=element_text(face="bold",size=14),
        axis.text=element_text(size=12),
        legend.title=element_text(face="bold",size=14),
        legend.text=element_text(size=12))+
  scale_fill_discrete(name="Fishing Gear",breaks=c("pot","net","line","spear"),
                      labels=c("Pot","Net","Line","Spear"))
ggsave("plots/gears.png",gear_plot,dpi=400)


catch<-read.csv("Data/MNI catch 94_15_fam.csv")
fam<-catch%>%
  group_by(year,Family)%>%
  summarise(catch=sum(weight_kgs,na.rm=TRUE))%>%
  ungroup()%>%
  arrange(year,Family)

names(catch)
catch$Family<-as.character(catch$Family)
f<-c("Belonidae","Serranidae","Acanthuridae","Lutjanidae","Balistidae","Carangidae")
catch$Family[!(catch$Family %in% f)]<-"Other"
unique(fam$Family)
#p<-bind(f,"Other")
f<-as.character(c(f,"Other"))
fam$Family<-as.character(fam$Family)
fam$Family<-factor(fam$Family,as.character(f))

fam<-fam%>%
  arrange(year,Family)

fam


fam_plot<-ggplot(fam,aes(x=year,y=catch,fill=Family))+
  geom_bar(stat="identity")+
  theme_bw()+
  xlab("Year")+
  ylab("Catch (kgs)")+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(limits=c(0,50000),expand=c(0,0))+
  theme(axis.title=element_text(face="bold",size=14),
        axis.text=element_text(size=12),
        legend.title=element_text(face="bold",size=14),
        legend.text=element_text(size=12))+
  scale_fill_brewer(palette="Set3")
 # scale_fill_discrete(name="Fishing Gear",breaks=c("pot","net","line","spear"),
                      labels=c("Pot","Net","Line","Spear"))
ggsave("plots/family.png",fam_plot,dpi=400)




target<-read.csv("Sept_2016/Data/target_catch.csv")
 

t<-c("SERRGU","BALIVE","ACANCO","ACANCH","LUTJVI")
target$id10<-factor(target$id10,as.character(t))

target<-target%>%
  arrange((id10))



target_plot<-ggplot(target,aes(x=year,y=catch,fill=id10))+
 geom_bar(stat="identity")+
  #geom_line(lwd=1.2)+
  theme_bw()+
  xlab("Year")+
  ylab("Catch (kg)")+
  scale_x_continuous(expand=c(0,0))+
 scale_y_continuous(limits=c(0,15000),expand=c(0,0),labels=comma)+
  theme(axis.title=element_text(face="bold",size=14),
        axis.text=element_text(size=12),
        legend.title=element_text(face="bold",size=14),
        legend.text=element_text(size=12))+
  #scale_fill_brewer(palette="Set3")
 scale_fill_discrete(name="Species",breaks=c("SERRGU","BALIVE","ACANCO","ACANCH","LUTJVI"),
labels=c("Red hind","Queen triggerfish","Blue tang","Doctorfish","Silk snapper"))
ggsave("Sept_2016/plots/target.png",target_plot,dpi=400)


unique(data$gearid)


net_trips<-read.csv("Sept_2016/Data/MNI catch_cleangear.csv")%>%
  mutate(trip_no=(paste(date,vesselid,sep="_")))%>%
  group_by(year,gearid)%>%
  filter(gearid=="net")%>%
  summarise(trips=length(unique(trip_no)))




bel_catch<-read.csv("Sept_2016/Data/MNI catch_cleangear.csv")%>%
  mutate(trip_no=(paste(date,vesselid,sep="_")))%>%
  group_by(year,id10)%>%
  summarise(catch=sum(weight_kgs,na.rm=FALSE))%>%
  filter(id10=="BELO")

bel_catch<-cbind(bel_catch,net_trips$trips)
bel_catch<-bel_catch%>%
  mutate("cpue"=catch/net_trips$trips)
colnames(bel_catch)<-c("Year","Species","catch","trips","cpue")


bel_plot<-ggplot(bel_catch,aes(x=Year,y=cpue))+
  geom_line()+
  geom_line(aes(y=catch))+
  # geom_point()+
  #  facet_wrap(~variable,scale="free_y",ncol=1)+
  xlab("Year")+
  ylab("CPUE (catch/trips)")+
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
        axis.line.y = element_line(color="black", size = 0.4))+
  scale_y_continuous(limits=c(0,120),expand=c(0,0),labels=comma)

ggsave("Sept_2016/plots/Bel_cpue.png",bel_plot)















d
tot<-catch%>%
  group_by(year)%>%
  summarise(total=sum(weight_kgs,na.rm=TRUE))
sum(catch$weight_kgs,na.rm=TRUE)
tb<-catch%>%
  filter(year=="2015")%>%
  summarize(total=sum(weight_kgs),na.rm=TRUE)
catch<-read.csv("Data/MNI catch 94_15_fam.csv")
fams<-catch%>%
  group_by(Family,)%>%
  summarise(catch=sum(weight_kgs,na.rm=TRUE),
            perc=catch/612795.4*100)
%
  ungroup()%>%
  arrange(desc(perc))
write.csv(fams,"Data/family_totals.csv")

sum(fams$perc[1:6])

catch$id10<-tolower(catch$id10)
sp<-catch%>%
  group_by(id10)%>%
  summarise(catch=sum(weight_kgs,na.rm=TRUE),
            perc2=catch/612795.4*100)%>%
  ungroup()%>%
  arrange(desc(catch))
sum(sp$perc2)
write.csv(sp,"Data/sp_totals.csv")

names(catch)
unique(catch$common)
unique(catch$id10)
sp_name<-unique(catch$scientific)
sp_test<-catch[,c(1,28)]
sp_test<-unique(sp_test)
View(sp_test)
write.csv(sp_test,"Data/speciesname.csv")
catch[catch$id10=="holoma",]
catch[catch$id10=="holoma",28]
  sum(catch$weight_kgs,na.rm=TRUE)
names(catch)
detach(package:plyr)
land<-catch%>%
  group_by(year,name)%>%
  summarise(to=sum(weight_kgs,na.rm=TRUE))
View(land)
