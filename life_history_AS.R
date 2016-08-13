library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

lifehistory<-read.csv("MNI_LH_FINAL_9.24.csv")
lifehistory[is.na(lifehistory)] <- 0
View(lifehistory)

lifehistory$Value[lifehistory$Species=="ACANCO"&lifehistory$Parameter=="Linf"]

lifehistory<-na.omit(lifehistory)

lh2<- lifehistory[,1:3] %>%
      spread(Parameter, Value)
  
  
  #dcast(lifehistory[1:3],Species~Parameter,value.var="Value",fun.aggreate=NULL)

allspecies<-unique(lifehistory$Species)

Lm <- data.frame(Species = allspecies, 
                 Lm = rep(0, length(allspecies)))

pdf('TESTlength.pdf')
i=1
for (i in 1:length(allspecies))
{
  temp<-lifehistory %>%
    filter(lifehistory$Species==allspecies[i]) 
             
  age<- c(1:temp$Value[temp$Parameter=='tmax'])

  
             length<-temp$Value[temp$Parameter=='Linf']*(1-exp(-temp$Value[temp$Parameter=='K']*(age-temp$Value[temp$Parameter=='t0'])))
  plotdf<-data.frame(age,length)  
  print(ggplot(plotdf,aes(x=age,y=length))+
    geom_line())

  Lm[i,2] <-  tail(length, n=1)
  print(i)
  
}

dev.off() # turns off pdf call
# 
# x<-(1:tmax[])
# 
# 
# acanch<-lh2$Linf[1]*(1-exp(-lh2$K[1]*(x[]-lh2$t0[1])))
# plot(acanch~x)
# 
# 
# ?dcast
# names(lifehistory)
# View(lh2)
# lvalues<-lifehistory$X=="Linf"
# head(lh2)
# View(lh2)
# library(dplyr)
# library(plyr)
# library(tidyr)
# library(reshape2)
# library(reshape)
# allspecies<-unique(lifehistory$Species)
# a=1
# 
# 
# for (a in 1:length(allspecies))
#  {
#   lifehistory %>%
#     filter(Species==allspecies[a]) %>%
#              ggplot(aes(x=Species, y=length)) +
#              geom_bar(width=0.5)
#  }
#      dev.off()
#      

# Test LHI 
#
lh2$Lm= Lm$Lm 

LHI <- lh2 %>%
  mutate(mk = M/K , LmLoo = Lm/Linf, Mtm = M * tmax) 



