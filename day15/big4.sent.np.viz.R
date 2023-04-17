library(tidyverse)
library(patchwork)

big4.mean.np <- big4.all.sent %>% group_by(artist) %>%
  summarize(pos=mean(positive,na.rm=T),neg=mean(negative,na.rm=T)) %>% 
  ungroup() %>%
  drop_na() %>% 
  pivot_longer(cols=-artist,names_to="cat",values_to = "score") %>% 
  mutate(x=c(rep(-2.25,8)),y=c(rep(c(1995,2003),4)))

slayer.pic <- magick::image_read("slayer.jpg")
anthrax.pic<-magick::image_read("anthrax.jpg")
mega.pic <- magick::image_read("megadeth.jpg")
metallica.pic <- magick::image_read("metallica.jpg")


#####################

### slayer viz

#####################

slayer.np <- big4.all.sent %>%
  mutate(Date2=ifelse(album.date=="Fistful Of Metal NA",1984,Date2),
         album.date=ifelse(album.date=="Fistful Of Metal NA","Fistful of Metal 1984",album.date)
  ) %>%
  mutate(album.y=ifelse(album.y=="Killing Is My Business... And Business Is Good!",
                        "Killing is My Business...",album.y)) %>%
  mutate(album.y=ifelse(album.y=="Peace Sells... But Who's Buying?", "Peace Sells...",album.y)) %>%

  mutate(album.y=fct_reorder(album.y,Date2)) %>%
  group_by(album.y,artist) %>%
  summarize(mpos=mean(positive,na.rm=T),mneg=mean(negative,na.rm=T),tot=n(),year=mean(Date2,na.rm=T)) %>% 
  drop_na() %>%
  ungroup() %>%
  mutate(max.neg=max(mneg),min.neg=min(mneg),max.pos=max(mpos),min.pos=min(mpos)) %>%
  filter(artist=="Slayer") %>%
  
  ggplot()+
  #geom_point(aes(x=.75,y=year,size=mpos),col="gold")+
  #geom_point(aes(x=-.75,y=year,size=mneg),col="darkred")+
  geom_text(aes(x=0,y=year+.25,label=album.y),size=3.5,col="white")+
  geom_text(aes(x=0,y=year-.25,label=year),size=2.5,col="white")+
  geom_text(aes(x=1,y=year,size=mpos,label=round(mpos,1)),col="gold")+
  geom_text(aes(x=-1,y=year,size=mneg,label=round(mneg,1)),col="gray70")+
  geom_text(aes(x=-1,y=2018,label="Negative"),col="gray70",size=5)+
  geom_text(aes(x=1,y=2018,label="Positive"),col="gold",size=5)+
  geom_text(aes(x=0,y=2018,label="Album"),col="white",size=5)+
  
  scale_size(range=c(1,5))+
  xlim(-3,1.5)+
  ylim(1982.5,2019)+

  geom_text(data=big4.mean.np %>% filter(artist=="Slayer"),
            aes(x=x,y=y,label=round(score,1),col=cat),size=4)+
  geom_text(data=data.frame(x=c(-2.25,-2.25),y=c(2004,1996),
                            label=c("Overall Average Negative","Overall Average Positive"),
                            cat=c("neg","pos")),
            aes(x=x,y=y,label=label,col=cat),size=4)+scale_color_manual(values=c("gray70","gold"))+
  
  annotation_custom(rasterGrob(slayer.pic),
                    xmin=-3,xmax=-1.5,ymin=2008,ymax=2016)+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        strip.text=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background = element_rect(fill="black"),
        plot.background=element_rect(fill="red",color="black"),
        axis.ticks=element_blank()
        )

#####################
 
### megadeth viz

#####################

megadeth.np <- big4.all.sent %>%
  mutate(Date2=ifelse(album.date=="Fistful Of Metal NA",1984,Date2),
         album.date=ifelse(album.date=="Fistful Of Metal NA","Fistful of Metal 1984",album.date)
  ) %>%
  mutate(album.y=ifelse(album.y=="Killing Is My Business... And Business Is Good!",
                        "Killing is My Business...",album.y)) %>%
  mutate(album.y=ifelse(album.y=="Peace Sells... But Who's Buying?", "Peace Sells...",album.y)) %>%
  
  mutate(album.y=fct_reorder(album.y,Date2)) %>%
  group_by(album.y,artist) %>%
  summarize(mpos=mean(positive,na.rm=T),mneg=mean(negative,na.rm=T),tot=n(),year=mean(Date2,na.rm=T)) %>% 
  drop_na() %>%
  ungroup() %>%
  mutate(max.neg=max(mneg),min.neg=min(mneg),max.pos=max(mpos),min.pos=min(mpos)) %>%
  filter(artist=="Megadeth") %>%
  
  ggplot()+
  #geom_point(aes(x=.75,y=year,size=mpos),col="gold")+
  #geom_point(aes(x=-.75,y=year,size=mneg),col="darkred")+
  geom_text(aes(x=0,y=year+.25,label=album.y),size=3.5,col="white")+
  geom_text(aes(x=0,y=year-.25,label=year),size=2.5,col="white")+
  geom_text(aes(x=1,y=year,size=mpos,label=round(mpos,1)),col="gold")+
  geom_text(aes(x=-1,y=year,size=mneg,label=round(mneg,1)),col="gray70")+
  geom_text(aes(x=-1,y=2018,label="Negative"),col="gray70",size=5)+
  geom_text(aes(x=1,y=2018,label="Positive"),col="gold",size=5)+
  geom_text(aes(x=0,y=2018,label="Album"),col="white",size=5)+
  
  scale_size(range=c(1,5))+
  xlim(-3,1.5)+
  ylim(1983,2019)+

  geom_text(data=big4.mean.np %>% filter(artist=="Megadeth"),
            aes(x=x,y=y,label=round(score,1),col=cat),size=4)+
  geom_text(data=data.frame(x=c(-2.25,-2.25),y=c(2004,1996),
                            label=c("Overall Average Negative","Overall Average Positive"),
                            cat=c("neg","pos")),
            aes(x=x,y=y,label=label,col=cat),size=4)+scale_color_manual(values=c("gray70","gold"))+
  
  annotation_custom(rasterGrob(mega.pic),
                    xmin=-3,xmax=-1.5,ymin=2008,ymax=2016)+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        strip.text=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background = element_rect(fill="black"),
        plot.background=element_rect(fill="blue",color="black"),
        axis.ticks=element_blank()
        )

#####################

### metallica viz

#####################

metallica.np <- big4.all.sent %>%
  mutate(Date2=ifelse(album.date=="Fistful Of Metal NA",1984,Date2),
         album.date=ifelse(album.date=="Fistful Of Metal NA","Fistful of Metal 1984",album.date)
  ) %>%
  mutate(album.y=ifelse(album.y=="Killing Is My Business... And Business Is Good!",
                        "Killing is My Business...",album.y)) %>%
  mutate(album.y=ifelse(album.y=="Peace Sells... But Who's Buying?", "Peace Sells...",album.y)) %>%
  
  mutate(album.y=fct_reorder(album.y,Date2)) %>%
  group_by(album.y,artist) %>%
  summarize(mpos=mean(positive,na.rm=T),mneg=mean(negative,na.rm=T),tot=n(),year=mean(Date2,na.rm=T)) %>% 
  drop_na() %>%
  ungroup() %>%
  mutate(max.neg=max(mneg),min.neg=min(mneg),max.pos=max(mpos),min.pos=min(mpos)) %>%
  filter(artist=="Metallica") %>%
  
  ggplot()+
  #geom_point(aes(x=.75,y=year,size=mpos),col="gold")+
  #geom_point(aes(x=-.75,y=year,size=mneg),col="darkred")+
  geom_text(aes(x=0,y=year+.25,label=album.y),size=3.5,col="white")+
  geom_text(aes(x=0,y=year-.25,label=year),size=2.5,col="white")+
  geom_text(aes(x=1,y=year,size=mpos,label=round(mpos,1)),col="gold")+
  geom_text(aes(x=-1,y=year,size=mneg,label=round(mneg,1)),col="gray70")+
  geom_text(aes(x=-1,y=2018,label="Negative"),col="gray70",size=5)+
  geom_text(aes(x=1,y=2018,label="Positive"),col="gold",size=5)+
  geom_text(aes(x=0,y=2018,label="Album"),col="white",size=5)+
  
  scale_size(range=c(1,5))+
  xlim(-3,1.5)+
  ylim(1983,2019)+
  
  geom_text(data=big4.mean.np %>% filter(artist=="Metallica"),
            aes(x=x,y=y,label=round(score,1),col=cat),size=4)+
  geom_text(data=data.frame(x=c(-2.25,-2.25),y=c(2004,1996),
                            label=c("Overall Average Negative","Overall Average Positive"),
                            cat=c("neg","pos")),
            aes(x=x,y=y,label=label,col=cat),size=4)+scale_color_manual(values=c("gray70","gold"))+
  
  annotation_custom(rasterGrob(metallica.pic),
                    xmin=-3,xmax=-1.5,ymin=2008,ymax=2016)+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        strip.text=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background = element_rect(fill="black"),
        plot.background=element_rect(fill="gray20",color="black"),
        axis.ticks=element_blank()
        )

#####################

### anthrax viz

#####################

anthrax.np <- big4.all.sent %>%
  mutate(Date2=ifelse(album.date=="Fistful Of Metal NA",1984,Date2),
         album.date=ifelse(album.date=="Fistful Of Metal NA","Fistful of Metal 1984",album.date)
  ) %>%
  mutate(album.y=ifelse(album.y=="Killing Is My Business... And Business Is Good!",
                        "Killing is My Business...",album.y)) %>%
  mutate(album.y=ifelse(album.y=="Peace Sells... But Who's Buying?", "Peace Sells...",album.y)) %>%
  
  mutate(album.y=fct_reorder(album.y,Date2)) %>%
  group_by(album.y,artist) %>%
  summarize(mpos=mean(positive,na.rm=T),mneg=mean(negative,na.rm=T),tot=n(),year=mean(Date2,na.rm=T)) %>% 
  drop_na() %>%
  ungroup() %>%
  mutate(max.neg=max(mneg),min.neg=min(mneg),max.pos=max(mpos),min.pos=min(mpos)) %>%
  filter(artist=="Anthrax") %>%
  
  ggplot()+
  #geom_point(aes(x=.75,y=year,size=mpos),col="gold")+
  #geom_point(aes(x=-.75,y=year,size=mneg),col="darkred")+
  geom_text(aes(x=0,y=year+.25,label=album.y),size=3.5,col="white")+
  geom_text(aes(x=0,y=year-.25,label=year),size=2.5,col="white")+
  geom_text(aes(x=1,y=year,size=mpos,label=round(mpos,1)),col="gold")+
  geom_text(aes(x=-1,y=year,size=mneg,label=round(mneg,1)),col="gray70")+
  geom_text(aes(x=-1,y=2018,label="Negative"),col="gray70",size=5)+
  geom_text(aes(x=1,y=2018,label="Positive"),col="gold",size=5)+
  geom_text(aes(x=0,y=2018,label="Album"),col="white",size=5)+
  
  scale_size(range=c(1,5))+
  xlim(-3,1.5)+
  ylim(1983,2019)+
  
  geom_text(data=big4.mean.np %>% filter(artist=="Anthrax"),
            aes(x=x,y=y,label=round(score,1),col=cat),size=4)+
  geom_text(data=data.frame(x=c(-2.25,-2.25),y=c(2004,1996),
                            label=c("Overall Average Negative","Overall Average Positive"),
                            cat=c("neg","pos")),
            aes(x=x,y=y,label=label,col=cat),size=4)+scale_color_manual(values=c("gray70","gold"))+
  
  annotation_custom(rasterGrob(anthrax.pic),
                    xmin=-3,xmax=-1.5,ymin=2008,ymax=2016)+
  theme_bw()+
  theme(legend.position = "none",
        axis.title=element_blank(),
        axis.text=element_blank(),
        strip.text=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background = element_rect(fill="black"),
        plot.background=element_rect(fill="gold",color="black"),
        axis.ticks=element_blank()
        )

#####################

### patchwork viz

#####################

(anthrax.np+metallica.np)/(megadeth.np+slayer.np)+
  plot_annotation(title="The Big Four of Thrash",
                  subtitle="Average Positive/Negative Sentiment by Album 1983-2018",
                  caption="Data from Wikipedia, Spotify & azlyrics.com",
  theme=theme(plot.title=element_text(size=24,hjust=.5,face="bold"),
              plot.subtitle=element_text(size=16,hjust=.5),
        plot.background = element_rect(fill="gray50")))




ggsave("big4.np.png",dpi=320,width=12.5,height=17)









