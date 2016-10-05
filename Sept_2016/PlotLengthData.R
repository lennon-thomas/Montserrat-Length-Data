PlotLengthData<- function(LengthDat,FigureFolder,Fish,Species,Theme)
{
   LengthDat<- LengthData
  
  #LengthDat<- subset(LengthDat,is.na(Length)==F & Length>0)
  
  #LengthDat$SiteType[LengthDat$MPA==0]<- 'Fished'

  #engthDat$SiteType[LengthDat$MPA==1]<- 'MPA'
  minimum<-min(LengthDat$Length)-5
  maximum<-Fish$Linf+1
  pdf(file=paste(FigureFolder,Fish_'Length Hist.pdf',sep=''),width=5,height=5)
  
  LengthDist<- ggplot(data=LengthDat,aes(Length,fill=(Data.Type)))+geom_histogram(alpha=0.8,aes(y=..count..),color=
                                                                                    "black")+
  scale_fill_manual(name='',values=c(Fisherycolor,Surveycolor))+
  geom_vline(xintercept=Fish$Mat50,linetype='longdash')+
    geom_vline(xintercept=Fish$Linf,linetype='longdash',color='red2')+
   # facet_wrap(paste("n =",length(LengthData$Length),sep=""),as.table=F)+
    #facet_wrap(~Year,as.table=F)+
   ggtitle(paste(Species,sep='\nn = ',length(LengthDat$Length)))+Theme+xlab("Length (cm)")+ylab("Frequency")+
    scale_x_continuous(limits=c(minimum,maximum),expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
    print(LengthDist)

# 
# 
# LengthBox<- (ggplot(data=LengthDat,aes(factor(Year),Length,fill=SiteType))
# +geom_boxplot(varwidth=F,alpha=1)+scale_fill_manual(name='',values=c(FishedColor,MPAColor))+Theme+xlab('Year')+
#   ylab('Length (cm)'))
  
#print(LengthBox)
#plot(LengthBox)
  dev.off()

save(LengthDist,file=paste(FigureFolder,Species,'-',Site,'Length Data.Rdata',sep=''))
    
}
