## plot for Martin Quinn Scores

## https://mqscores.lsa.umich.edu/measures.php - website for the data

## load packages and data
library(tidyverse)
library(geomtextpath)

mqscore <- read.csv("justices.csv")

## more data for the viz

mqscore.names=data.frame(
  name=c("John Roberts","Clarence Thomas","Ruth Bader Ginsburg","Stephen Breyer",
         "Samuel Alito","Sonia Sotomayor","Elena Kagan",
         "Neil Gorsuch","Brett Kavanaugh","Amy Coney Barrett"),
  justiceName=c("JGRoberts","CThomas","RBGinsburg","SGBreyer","SAAlito","SSotomayor",
                "EKagan","NMGorsuch","BMKavanaugh","ACBarrett")
) %>% left_join(
  mqscore %>% filter(term>=2017,justiceName != "AMKennedy") %>%
    group_by(justiceName) %>%
    summarize(post_mn_sd=sd(post_mn),post_mn=mean(post_mn))
)

## viz code

mqscore %>% filter(term >= 2017, justiceName != "AMKennedy") %>%
  select(justiceName,post_mn,term) %>%
  left_join(mqscore.names) %>%
  mutate(justiceName=fct_reorder(factor(justiceName),post_mn)) %>%
  ggplot(aes(post_mn,justiceName))+
  geom_vline(xintercept=0,col="gray60",lty=2)+
  geom_segment(data=data.frame(x=c(-5:-1,1:5),y=rep(0,10),xend=c(-5:-1,1:5),yend=rep(10.5,10)),
               aes(x=x,y=y,xend=xend,yend=yend),col="gray90")+
  geom_point(aes(alpha=term),size=4)+
  geom_text(data=mqscore.names,aes(post_mn,justiceName,label=name),size=6,vjust=1.75)+
  scale_x_continuous(breaks=-5:5,labels=-5:5,limits=c(-5,5))+
  scale_y_discrete(expand=c(.1,.5))+
  labs(
    title="SCOTUS:  Justice Ideology 2017-2021",
    subtitle="Ideology measured using Martin-Quinn Scores*",
    caption="*https://mqscores.lsa.umich.edu/"
  )+
  geom_segment(aes(x=2.5,xend=4.5,y=11,yend=11),arrow=arrow(length=unit(.5,"cm"),type="closed"))+
  geom_segment(aes(x=-2.5,xend=-4.5,y=11,yend=11),arrow=arrow(length=unit(.5,"cm"),type="closed"))+
  geomtextpath::geom_textline(data=data.frame(x=c(.5,2.5),y=c(11,11),justiceName=NA),
                              aes(x,y,label="More Conservative"),size=6,linecolor=NA)+
  geomtextpath::geom_textline(data=data.frame(x=c(-.5,-2.5),y=c(11,11),justiceName=NA),
                              aes(x,y,label="More Liberal"),size=6,linecolor=NA)+
  annotate("text",x=3,y=2,label="The most conserative score ever recorded is 4.51 (Rehnquist, 1979)\n\nJustice Thomas has the second most conservative average score (3.47)")+
  annotate("text",x=-3,y=8,label="The most liberal score ever recorded is -7.93 (Douglas, 1975)\n\nJustice Sotomayor has the second most liberal average score (-3.05)")+
  theme_minimal()+
  theme(
    legend.position = "top",
    legend.title=element_blank(),
    legend.text=element_text(size=12),
    plot.title=element_text(size=24,hjust=.5),
    plot.subtitle=element_text(size=18,hjust=.5),
    plot.caption=element_text(size=12),
    axis.title=element_blank(),
    axis.text.y=element_blank(),
    axis.text.x=element_text(size=12),
    panel.grid=element_blank()
  )

ggsave("MQscores_2017_2021_alt.png",dpi=320,height=8,width=12,bg="white")
