library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

lifehistory <- read.csv("MNI_LH_FINAL_aug_2016.csv")
lifehistory[is.na(lifehistory)] <- 0
View(lifehistory)

lifehistory$Value[lifehistory$Species=="ACANCO"&lifehistory$Parameter=="Linf"]

lifehistory<-na.omit(lifehistory)

lh2<- lifehistory[,1:3] %>%
      spread(Parameter, Value)
  

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


# Test LHI 
#

LHI <- lh2 %>%
  mutate(mk = M/K , 
         LmLinf = m50/Linf, 
         LmLinf.est = 3/((M/K)+3),
         Mtmax = M*tmax) 



