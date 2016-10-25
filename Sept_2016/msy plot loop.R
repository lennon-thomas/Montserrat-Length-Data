



rm(list=ls())

setwd("C:/Users/lthomas/GitHub/DPSA/Montserrat/Catch figures")
Directory<-setwd("C:/Users/lthomas/GitHub/DPSA/Montserrat/Catch figures")


Sp<-c("A. chirurgus","A. coeruleus", "B. vetula", "E. guttatus","H. adscensionis", "L. mahogoni")
Common<-c("doctorfish", "blue tang","queen triggerfish", "red hind", "squirrelfish", "mahogany snapper")
results<-read.csv("MSY results.csv")

length(Year)
i=6
for(i in 1:6) {
  Species<-Sp[i]
  Font <- 'Helvetica'
  FontColor <- 'Black'
  PlotFontSize <- 11
  Files <- list.files(Directory)
  
  if ( any(grepl(paste(Common[i],'_CatchData',sep=""),Files) ))
  {
    Catchdata <- read.csv(paste(Common[i],'_CatchData.csv',sep=""), stringsAsFactors = F)
  }
 Catchdata<-as.data.frame(Catchdata) 
 Catchdata[Catcdata==0]<-NA
names(Catchdata)
Year<-Catchdata$Year
Weight<-Catchdata$Catch


msy<-rep(results$MSY[i],length(Year))
lmsy<-rep(results$lower[i],length(Year))
umsy<-  rep(results$upper[i],length(Year))
cmax<-max(Weight)+100
#Catch$Year<-as.factor(Catch$Year)
pdf(file=paste(Species, "_Catchplot.pdf",sep=""),fonts=Font,width=6,height=5)
figure<-gap.plot(Year,Weight,gap=0,type="o",xlab="Year",ylab="Total Recorded Catch (kgs)",
     main=Species,cex.lab=0.95,cex.axis=0.8,pch=19,cex=0.3,cex.main=1.2,font.lab=2,font.main=3,bty="n",las=1,family=Font, xlim=c(1993,2015), ylim=c(0,cmax),xaxs="i",
     yaxs="i")
box(which="plot",lty="solid",bty="l")
figure<-figure+lines(Year,msy,type="l",col="red",lwd=0.8)

figure<-figure+lines(Year,umsy,type="l",col="red",lty=2,lwd=0.8)

figure<-figure+lines(Year,lmsy,type="l",col="red",lty=2,lwd=0.8)
print(i)
dev.off()
graphics.off()
}

