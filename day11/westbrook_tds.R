library(tidyverse)
library(rvest)

### Create Data

westbrook <- data.frame()
for(i in 2009:2021) {
  url <- paste0("https://www.basketball-reference.com/players/w/westbru01/gamelog/",i)
  xxx <- read_html(url) %>%
    html_nodes("table") %>%
    html_table(fill=T)
  xxx <- xxx[[8]]
  xxx$Season=paste0(i-1,"-",i)
  xxx$Season2=100*(i-2008)
  westbrook <- rbind(westbrook,xxx)
}
wb.names <- names(westbrook)
wb.names[6]="Home_Away"
wb.names[8]="Win_Loss"
wb.names[30] = "plus_minus"
names(westbrook) <- wb.names
westbrook <- westbrook[-which(westbrook[,1]=="Rk"),]
westbrook <- westbrook[-which(westbrook$GS=="Inactive"),]
westbrook <- westbrook[-which(westbrook$GS=="Did Not Play"),]
westbrook <- westbrook[-which(westbrook$GS=="Not With Team"),]
westbrook <- westbrook[-which(westbrook$GS=="Did Not Dress"),]
westbrook$Date <- as.Date(westbrook$Date,"%Y-%m-%d")
westbrook <- westbrook %>% 
  mutate_at(vars(PTS,TRB,AST),as.numeric) %>% 
  mutate(TDB=ifelse((PTS>=10 & AST>=10 & TRB >= 10),1,0))

### Create Viz

westbrook %>%
  select(G,Rk,PTS,TRB,AST) %>%
  mutate_if(is.character,as.numeric) %>%
  mutate(Season = westbrook$Season,TDB=westbrook$TDB) %>%
  mutate(Season=ifelse(Season=="2019-2020","2019-2020*",Season)) %>%
  mutate(Season=ifelse(Season=="2020-2021","2020-2021*",Season),
         Season=paste0(Season,"\n",westbrook$Tm)) %>%
  gather("stats","value",-Season,-G,-TDB,-Rk) %>%
  mutate(stats=factor(stats,levels=c("AST","TRB","PTS"))) %>%
  filter(TDB==1) %>% 
  ggplot(aes(Rk,value,fill=stats)) +
  geom_col() +
  xlim(-3,85) +
  #geom_text(aes(x=1,y=90,label="Game 1"),hjust=-.15,size=3)+
  geom_segment(data=data.frame(x1=c(5,26,45,64),x2=c(18,37,57,77),
                               y1=c(90,90,90,90),stats=NA),
               aes(x=x1,xend=x2,y=y1,yend=y1),
               arrow=arrow(length=unit(0.2,"cm")))+
  geom_text(data=data.frame(x=c(1,22,41,60,82),
                            y=90,stats=NA,
                            label=c("Game 1","Game 21","Game 41","Game 61","Game 82"),
                            face=c("bold","plain","plain","plain","plain")),
            aes(x=x,y=y,label=label,fontface=face),size=3.5)+
  geom_hline(yintercept=seq(10,70,10),col="black",alpha=0.15)+
  geom_vline(xintercept=c(1,21,41,61,82),col="black",lty=2,alpha=.4)+
  scale_fill_manual(values=c("#0068FF","#00E7FF","#1800FF"),
                    limits=c("AST","TRB","PTS"),
                    labels=c("Assists","Rebounds","Points"))+
  theme_bw()+
  labs(title="Russell Westbrook - Triple Doubles\n2008-2021",
       subtitle="(2011-2012 not shown - 0 triple doubles that season)",
       caption="*2019-2020 and 2020-2021 were COVID shortened seasons\n
       data:  basketball.reference.com",
       fill="")+
  coord_polar()+
  facet_wrap(.~Season)+
  
  theme(legend.position = "top",
        legend.background=element_rect(fill="cornsilk2"),
        plot.title=element_text(hjust=.5,size=24),
        plot.subtitle=element_text(hjust=.5,size=14),
        plot.caption=element_text(size=12),
        axis.title=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        strip.text.x=element_text(size=13,face="bold"),
        strip.background=element_rect(linetype="solid",fill="cornsilk2"),
        panel.background=element_rect(fill="cornsilk"),
        plot.background=element_rect(fill="#FF9700",color="#FF9700"))

ggsave("rwtd.png",dpi=320,height=14,width=14)
