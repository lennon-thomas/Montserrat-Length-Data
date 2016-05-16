rm(list=ls())


library(ggplot2)
library(grid)
library(gridExtra)

setwd("C:/Users/lthomas/GitHub/DPSA/Montserrat/Length figures")
sapply(list.files(pattern="[.]R$", path="Functions", full.names=TRUE), source)
source("SubFunctions.R") #Pull in helper functions for assessment modules

Sp<-c("A. chirurgus","A. coeruleus", "B. vetula", "H. adscensionis", "L. mahogoni","E. guttatus")
Common<-c("doctorfish", "blue tang","queen triggerfish", "squirrelfish", "mahogany snapper","red hind")
i=6
for(i in 1:6){
Species<-Sp[i]
Assessment <- Common[i]
Site<-("Montserrat")
Directory<-setwd("C:/Users/lthomas/GitHub/DPSA/Montserrat/Length figures")

Font <- 'Helvetica'
FontColor <- 'Black'
PlotFontSize <- 11
MPAColor<-"red"
FishedColor <- "lightseagreen"

Files <- list.files(Directory)

if ( any(grepl(paste(Common[i],'_LengthData',sep=""),Files) ))
{
  LengthData <- read.csv(paste(Common[i],'_LengthData.csv',sep=""), stringsAsFactors = F)
}



source(paste(Common[i],"_ControlFile.R",sep=""))
Mat50<-Fish$Mat50
Linf<-Fish$Linf

FigureFolder<- paste(Directory,'Figures/',sep='')

if (file.exists(FigureFolder)==F)
{
  dir.create(FigureFolder,recursive=T)
  
}

Theme<- theme(legend.position='none',plot.background=element_rect(color="white"),
              rect=element_rect(fill='transparent',color=NA),
              text=element_text(size=11,family=Font,color=FontColor),plot.title = element_text( face="italic"),
              axis.text.x=element_text(color=FontColor), axis.text.y=element_text(color="black"),panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),strip.background = element_rect(colour="white", fill="white"),axis.line=element_line(color="black"))

if (exists('LengthData')) {PlotLengthData(LengthData,FigureFolder,Fish,Species,Site,Theme)}
#graphics.off()
print(i)
i<-i+1
}
