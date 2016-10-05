library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)
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


names(catch)
unique(catch$Family)
fam<-catch%>%
  group_by(year,Family)%>%
  summarise(catch=sum(weight_kgs,na.rm=TRUE))%>%
  ungroup()%>%
  arrange(year,Family)


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


dev.off()
tot<-catch%>%
  group_by(year)%>%
  summarise(total=sum(weight_kgs,na.rm=TRUE))


fams<-fam%>%
  group_by(Family)%>%
  summarise(catch=sum(catch,na.rm=TRUE),
            perc=catch/t*100)%>%
  arrange(desc(perc))
sum(fams$perc[1:6])

catch$id10<-tolower(catch$id10)
sp<-catch%>%
  group_by(id10)%>%
  summarise(catch=sum(weight_kgs,na.rm=TRUE))%>%
  ungroup()%>%
  arrange(desc(catch))

