PlotLengthData<- function(LengthDat,FigureFolder,Fish,Species,Theme)
{
   LengthDat<- LengthData
  
  #LengthDat<- subset(LengthDat,is.na(Length)==F & Length>0)
  
  #LengthDat$SiteType[LengthDat$MPA==0]<- 'Fished'

  #engthDat$SiteType[LengthDat$MPA==1]<- 'MPA'
  minimum<-min(LengthDat$Length)-5
  maximum<-Fish$Linf+10
 pdf(file=paste(FigureFolder,Species[i],'_Length Hist.pdf',sep=''),width=7,height=5)
  LengthDist<-ggplot(data=LengthDat,aes(Length,fill=(Data.Type)))+geom_density(alpha=0.6,aes(y=..count..))+
  geom_vline(xintercept=Fish$Mat50,linetype='longdash')+
    geom_vline(xintercept=Fish$Linf,linetype='longdash',color='red2')+
    #facet_wrap(paste("n =",length(LengthData$Length),sep=""),as.table=F)+
    #facet_wrap(~Year,as.table=F)+
   ggtitle(paste(Species,sep='\nn = ',length(LengthDat$Length)))+Theme+xlab("Length (cm)")+ylab("Frequency")+
    scale_x_continuous(limits=c(minimum,maximum),expand = c(0, 0))+ 
    scale_y_continuous(expand = c(0, 0))
    LengthDist<-LengthDist+labs(fill="Data Type")
    print(LengthDist)
    dev.off()
 
}

