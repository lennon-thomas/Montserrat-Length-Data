

gar<-read.csv("gar/Montserrat-LittleBay-gar_CatchData.csv")
nrow(gar)
msy<-rep(8110.58,22)
lower<-rep(4298.21,22)
upper<-rep(10581.13,22)
gar<-cbind(gar,msy,upper,lower)

names(gar)
gar_plot<-ggplot(gar,aes(x=Year,y=Catch))+
  geom_line(lwd=1.1, color="blue")+
  geom_line(aes(y=lower),color="red",lty="dashed",lwd=0.9)+
  geom_line(aes(y=upper),color="red",lty="dashed",lwd=0.9)+
  geom_line(aes(y=msy),color="red",lty="solid",lwd=0.9)+
  theme_bw()+
  theme(legend.position="none",
        axis.title=element_text(face="bold",size=16),
        axis.text=element_text(size=14),
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.border = element_blank()) +
  theme(axis.line.x = element_line(color="black", size = 0.4),
        axis.line.y = element_line(color="black", size = 0.4))+
 # scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(limits=c(0,17000),expand=c(0,0),labels=comma)+
  ylab("Catch (kgs)")
ggsave("plots/gar_plot.png",gar_plot,dpi=400)
