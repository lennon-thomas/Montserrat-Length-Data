rm(list=ls())
setwd("C:/Users/lthomas/GitHub/DPSA/Montserrat")


library(ggplot2)


sapply(list.files(pattern="[.]R$", path="Functions", full.names=TRUE), source)
source("SubFunctions.R") #Pull in helper functions for assessment modules

Species<-'Queen triggerfish'
Assessment <- 'Queen triggerfish'
dir.create(Assessment)
Sites<-c("Montserrat")
AssessmentName <- paste(Assessment,sep='_')
Directory<- paste(Assessment, "/", sep='')

Font <- 'Helvetica'

FontColor <- 'Black'

PlotFontSize <- 11
MPAColor<-"red"
FishedColor <- "lightseagreen"
Files <- list.files(Assessment)

if ( any(grepl('_LengthData',Files)) )
{
  LengthData <- read.csv(paste(Assessment,'/',Files[grepl('_LengthData',Files)],sep=''), stringsAsFactors = F)
}



source(paste("Queen triggerfish/Montserrat2-LittleBay-Queen triggerfish_ControlFile.R"))
Mat50<-Fish$Mat50
Linf<-Fish$Linf

FigureFolder<- paste(Directory,'Figures/',sep='')

if (file.exists(FigureFolder)==F)
{
  dir.create(FigureFolder,recursive=T)
  
}
Linf<-Fish$Linf
Theme<- theme(legend.position='right',plot.background=element_rect(color="white"),
              rect=element_rect(fill='transparent',color=NA),
              text=element_text(size=11,family=Font,color=FontColor),
              axis.text.x=element_text(color=FontColor), axis.text.y=element_text(color="black"),panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),strip.background = element_rect(colour="white", fill="white"),axis.line=element_line(color="black"))

if (exists('LengthData')) {PlotLengthData(LengthData,FigureFolder,Fish,Species,Sites,Theme)}
graphics.off()