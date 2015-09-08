library(ggplot2)
library(tidyr)
library(plyr)
library(dplyr)
setwd("H:/Documents/Work for waitt/WI 2015/June Sampling/Montserrat/length based analysis")
lifehistory<-read.csv("Montserrat Parameters for Analysis 7.17.csv")
lifehistory[is.na(lifehistory)] <- 0
#View(lifehistory)

#lifehistory$Value[lifehistory$Species=="ACANCO"&lifehistory$Variable=="Linf"]

#lifehistory<-na.omit(lifehistory)
#lh2<-dcast(lifehistory,Species~X,value.var="Value",fun.aggreate=NULL)

allspecies<-unique(lifehistory$Species)

pdf('length at age.pdf')##creates pdf
i=1
## loop to plot vb curve and length/age at maturity
for (i in 1:length(allspecies))
{
  temp<-lifehistory %>%
    filter(lifehistory$Species==allspecies[i]) 
             
  age<- c(1:temp$Value[temp$Variable=='tmax'])

  
  length<-temp$Value[temp$Variable=='Linf']*(1-exp(-temp$Value[temp$Variable=='K']*(age-temp$Value[temp$Variable=='t0'])))
  mat_length<-temp$Value[temp$Variable=='Linf']*(1-exp(-temp$Value[temp$Variable=='K']*(temp$Value[temp$Variable=='tmat']-temp$Value[temp$Variable=='t0'])))
  plotdf<-data.frame(age,length)  
  length_age<-ggplot(plotdf,aes(x=age,y=length))+
    geom_point(shape=1,size=3)+geom_line()

  
length_age<-length_age+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
       panel.background = element_blank(), axis.line = element_line(colour = "black"),  
  plot.title = element_text(face = "bold.italic",size=22))+
    ylab("Length (cm)")+
    xlab("Age (years)")+
    ggtitle(temp$Common[1])
  length_age<-length_age+geom_point(aes(x=temp$Value[temp$Variable=='tmat'],y=mat_length),shape=4,color="red",size=5)
  print(length_age)
}

dev.off() # turns off pdf call






