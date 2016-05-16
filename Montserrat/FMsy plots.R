library(ggplot2)
library(RColorBrewer)
  
setwd("C:/Users/lthomas/GitHub/DPSA/Montserrat")

data<-read.csv("results.csv")

blues <- brewer.pal(3, "Blues")


fplot<-ggplot(data=data,aes(x=Species, y=fish,fill=Method))+
              geom_bar(stat="identity",position=position_dodge(),color="black")+
  geom_hline(yintercept=mean(1),linetype="dashed",size=0.6)+
labs(x="",y=expression(F/F[MSY]))+
  geom_errorbar(aes(ymin=data$lower, ymax=data$upper), width=.2,position=position_dodge(.9),colour="black")+
  scale_y_continuous(limits=c(0,20), expand = c(0, 0))
                                                                                  
fplot<-fplot+theme(
  axis.text = element_text(size = 14),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "white"),
  axis.line=element_line(colour="Black"),axis.text.x = element_text(angle = 45,size=14,hjust=1),axis.title.y=element_text(size=16,vjust=1.5,face="italic"),
  axis.title.x=element_text(size=16,vjust=0.1),legend.position=c(0.865,0.92),legend.title=element_text(size=14),legend.text=element_text(size=12))

fplot<-fplot + 
  
  scale_fill_manual(values=c("#666666", "#CCCCCC", "white"),                    #"#0066CC", "#FF9999", "#00CC66""
                       name="Assessment Method",
                       breaks=c("Catch MSY", "Lbar", "lbspr"),
                       labels=c("Catch MSY", "LBAR", "LB-SPR"))


plot(fplot)
tiff(file="FMSY Results2.tiff",res=400,width=6.5,height=6,units="in")
fplot
dev.off()
# pdf(file="Fmsy_blue_error.pdf")
# fplot
# dev.off()
