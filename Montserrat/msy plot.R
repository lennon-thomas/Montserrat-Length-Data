



rm(list=ls())

setwd("C:/Users/lthomas/GitHub/DPSA/Montserrat/Catch figures")
Directory<-setwd("C:/Users/lthomas/GitHub/DPSA/Montserrat/Catch figures")


Sp<-c("A. chirurgus","A. coeruleus", "B. vetula", "E. guttatus","H. adscensionis", "L. mahogoni")
Common<-c("doctorfish", "blue tang","queen triggerfish", "red hind", "squirrelfish", "mahogany snapper")
results<-read.csv("MSY results.csv")

length(Year)
i=2
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
names(Catchdata)
x<-Catchdata$Year
y<-Catchdata$Catch
plot(x,y)
test<-plot(Catchdata$Catch,Catchdata$Year,xlim=c(1994,2014))
plot(test)
msy<-rep(results$MSY[i],length(Catch$Year))
lmsy<-rep(results$lower[i],length(Catch$Year))
umsy<-  rep(results$upper[i],length(Catch$Year))
cmax<-max(Catch$Catch)+100
Catch$Year<-as.factor(Catch$Year)
pdf(file=paste(Species, "_Catchplot.pdf",sep=""),fonts=Font,width=5,height=5)
figure<-plot(Year,Catch$Catch),type="l"),xlab="Year",ylab="Total Recorded Catch (kg)",
     main=Species,cex.lab=0.95,cex.axis=0.8,cex.main=1.2,font.lab=2,font.main=2,bty="n",las=1,family=Font, xlim=c(1994,2014))
plot(figure)
figure<-figure+lines(data$Year,msy,type="l",col="red")

figure<-figure+lines(data$Year,umsy,type="l",col="red",lty=2)

figure<-figure+lines(data$Year,lmsy,type="l",col="red",lty=2)
print(i)
dev.off()
graphics.off()
}