## plots for Martin Quinn Scores

## https://mqscores.lsa.umich.edu/measures.php - website for the data

## load packages and data
library(tidyverse)
library(geomtextpath)

mqscore <- read.csv("justices.csv")


## more data for the viz

mqscore.names=data.frame(
  name=c("John Roberts","Clarence Thomas","Ruth Bader Ginsburg","Stephen Breyer",
         "Samuel Alito","Sonia Sotomayor","Elena Kagan",
         "Neil Gorsuch","Brett Kavanaugh","Amy Coney Barrett","Anthony Kennedy","Antonin Scalia"),
  justiceName=c("JGRoberts","CThomas","RBGinsburg","SGBreyer","SAAlito","SSotomayor",
                "EKagan","NMGorsuch","BMKavanaugh","ACBarrett","AMKennedy","AScalia"),
  party=c("red","red","blue","blue","red","blue","blue","red","red","red","red","red"),
  vjust=c(-1,-1,-1,1.5,-1,-1,-1,-1,-1,-1,1.5,1.5),
  hjust=c(.35,0.35,.9,0.35,0.35,0.35,0.35,0.35,0.5,.5,0,0)
)

kennedy <- mqscore %>% filter(term >= 2012, justiceName=="AMKennedy") %>% 
  rbind(mqscore %>% filter(term > 2012, term < 2017, justiceName=="AMKennedy")) %>%  
  arrange(term) %>% 
  mutate(group=rep(1:5,each=2),colorc=rep(c("red","blue","blue","blue","red"),each=2))

## viz code

mqscore %>% filter(term >= 2012,justiceName != "AMKennedy") %>%
  select(justiceName,post_mn,term) %>%
  left_join(mqscore.names) %>%
  mutate(justiceName=fct_reorder(factor(justiceName),post_mn)) %>%
  ggplot(aes(term,post_mn,group=justiceName))+
  geom_line(data=
              data.frame(x=c(rep(2011.5,3),rep(2021.25,3)),
                         y=rep(c(-4,-2,2),2),
                         justiceName=rep(letters[1:3],2)),
            aes(x=x,y=y,group=justiceName),col="gray90",alpha=.1)+
  geom_segment(x=2011.5,y=0,xend=2021.5,yend=0,col="gray50")+
  geom_point(aes(col=party),size=3)+
  geom_textline(aes(label=name,linecolor=party,vjust=vjust),size=4.5,lty=2,
                linewidth=.75,col="gray90")+
  geom_line(data=kennedy,
            aes(x=term,y=post_mn,col=colorc,group=group),lty=2,size=.75)+
  geom_point(data=mqscore %>% filter(term >= 2012, justiceName=="AMKennedy") %>% mutate(colorc=ifelse(post_mn>0,"red","blue")),
             aes(x=term,y=post_mn,col=colorc),size=3)+
  geom_textline(data=kennedy %>% select(-group,-colorc),
                aes(x=term,y=post_mn,label="Anthony Kennedy"),linecolor=NA,size=4.5,vjust=1.5,col="gray90")+
  geom_segment(aes(x=2021.9,y=2.65,xend=2021.9,yend=3.5),col="red",
               arrow=arrow(length=unit(.5,"cm"),type="closed"))+
  geom_segment(aes(x=2021.9,y=-2.3,xend=2021.9,yend=-3.5),col="blue",
               arrow=arrow(length=unit(.5,"cm"),type="closed"))+
  geom_textline(data=data.frame(x=c(2021.9,2021.9),y=c(.5,2.5),justiceName=NA),
                aes(x,y,label="More Conservative"),size=6,linecolor=NA,color="gray90")+
  geom_textline(data=data.frame(x=c(2021.9,2021.9),y=c(-.5,-2.5),justiceName=NA),
                aes(x,y,label="More Liberal"),size=6,angle=180,linecolor=NA,color="gray90")+
  geom_text(data=mqscore %>% filter(term>=2012) %>% 
              group_by(justiceName) %>% slice_min(order_by=term),
            aes(x=term-.3,y=post_mn,label=post_mn),col="gray90",size=4)+
  geom_text(data=mqscore %>% filter(term>=2012) %>%
              group_by(justiceName) %>% slice_max(order_by=term),
            aes(x=term+.3,y=post_mn,label=post_mn),col="gray90",size=4)+
  geom_text(aes(x=2011.5,y=0,label=0),hjust=2,size=4,col="gray70")+
  geom_text(data=data.frame(x=rep(2011.5,3),y=c(-4,-2,2),label=c(-4,-2,2),justiceName=NA),
            aes(x=x,y=y,label=label),hjust=2,col="gray70",size=3)+
  ylim(-4.2,3.5)+
  scale_color_manual(values=c("blue","red"))+
  scale_x_continuous(breaks=2012:2021,labels=2012:2021,limits=c(2011.5,2022))+
  labs(
    title="SCOTUS Ideology Scores 2012-2021",
    subtitle="Martin-Quinn Scores\n(https://mqscores.lsa.umich.edu/measures.php)",
    caption='\nSource:  Andrew D. Martin and Kevin M. Quinn. 2002.  "Dynamic Ideal Point Estimation via Markov Chain Monte Carlo for the U.S. Supreme Court, 1953-1999."  Political Analysis. 10:134-153\n'
  )+
  theme_bw()+
  theme(legend.position = "none",
        plot.title=element_text(hjust=.5,size=24,color="gray90"),
        plot.subtitle=element_text(hjust=.5,size=16,color="gray90"),
        plot.caption=element_text(hjust=.5,color="gray90"),
        axis.title=element_blank(),
        axis.text.x=element_text(size=12,color="gray90"),
        axis.text.y=element_blank(),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),panel.grid.major.x=element_line(color="gray90",linewidth=.1),
        panel.grid.major.y = element_blank(),
        panel.border=element_blank(),
        panel.background = element_rect(fill="gray10"),
        plot.background = element_rect(fill="gray10")
        )

ggsave("MQscores_2012_2021.png",dpi=320,height=10,width=16)


