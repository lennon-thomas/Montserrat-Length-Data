library(ggplot2)
library(RColorBrewer)
  
#setwd("C:/Users/lthomas/GitHub/DPSA/Montserrat")

data<-read.csv("length_results.csv")
data<-data[1:5,]
blues <- brewer.pal(3, "Blues")
names(data)
data$Species
fplot<-ggplot(data=data,aes(x=Species, y=f))+
              geom_bar(stat="identity",position=position_dodge(),fill="blue",alpha=0.6)+
  geom_hline(yintercept=mean(1),linetype="dashed",size=1.2)+
labs(x="",y=expression(F/F[MSY]))+
  geom_errorbar(aes(ymin=data$flower, ymax=data$fupper), width=.2,position=position_dodge(.9),colour="black")+
  scale_y_continuous(limits=c(0,2.5), expand = c(0, 0))

fplot<-fplot+theme_bw()
                                                                                  
fplot<-fplot+theme(
  axis.text = element_text(size = 14),
  #panel.grid.major = element_blank(),
  #panel.grid.minor = element_blank(),
  #panel.background = element_rect(fill = "white"),
  axis.line=element_line(colour="Black"),axis.text.x = element_text(angle = 45,size=14,hjust=1),axis.title.y=element_text(size=16,vjust=1.5,face="italic"),
  axis.title.x=element_text(size=16,vjust=0.1),legend.position="right",legend.title=element_text(size=14),legend.text=element_text(size=12))
ggsave("plots/fplot_length.png",fplot,dpi=400)

names(catch)

# fplot<-fplot + 
#   
#   scale_fill_manual(values=c("#666666", "#CCCCCC", "white"),                    #"#0066CC", "#FF9999", "#00CC66""
#                        name="Assessment Method",
#                        breaks=c("Catch MSY", "Lbar", "lbspr"),
#                        labels=c("Catch MSY", "LBAR", "LB-SPR"))


plot(fplot)
tiff(file="FMSY Results.tiff",res=600,width=6.5,height=6,units="in")
fplot
dev.off()
# pdf(file="Fmsy_blue_error.pdf")
# fplot
# dev.off()
