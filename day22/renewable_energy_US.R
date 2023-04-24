# source: https://ourworldindata.org/renewable-energy
library(tidyverse)
library(showtext)

##### Load Data

modern_renewable_prod <- read_csv("modern-renewable-prod.csv")
renewable_share_energy <- read_csv("renewable-share-energy.csv")

##### Data Prep

mrp <- cbind(
  modern_renewable_prod %>% 
    filter(Entity=="United States") %>%
    `colnames<-`(c("Entity","Code","Year","Wind","Hydro","Solar","Other")),
  renewable_share_energy %>% 
    filter(Entity=="United States") %>%
    select(`Renewables (% equivalent primary energy)`) %>%
    `colnames<-`(c("prop_energy"))
) %>%
  pivot_longer(
    cols=c("Wind","Hydro","Solar","Other"),
    names_to="type",
    values_to="total_twh"
  )

rse <- renewable_share_energy %>%
    filter(Year %in% c(1970,1980,1990,2000,2010,2020),Entity=="United States") %>%
    rename(percent=`Renewables (% equivalent primary energy)`) %>%
    mutate(x=1050,y=1:6)

mrp.hydro <- mrp %>% filter(Year %in% c(1970,1980,1990,2000,2010,2020),type=="Hydro") %>%
  mutate(y=(Year-1970)/10+1)
mrp.wind <- mrp %>% filter(Year %in% c(1970,1980,1990,2000,2010,2020),type=="Wind") %>%
  mutate(y=(Year-1970)/10+1)
mrp.solar <- mrp %>% filter(Year %in% c(1970,1980,1990,2000,2010,2020),type=="Solar") %>%
  mutate(y=(Year-1970)/10+1)
mrp.other <- mrp %>% filter(Year %in% c(1970,1980,1990,2000,2010,2020),type=="Other") %>%
  mutate(y=(Year-1970)/10+1)


## hydro data

xxx.lims <- ceiling(
  cumsum(
    c(10,max(mrp.hydro$total_twh)+20,max(mrp.wind$total_twh)+10,
              max(mrp.solar$total_twh)+10,max(mrp.other$total_twh)+10)))
xxx.lims <- c(mean(xxx.lims[1:2]),mean(xxx.lims[2:3]),mean(xxx.lims[3:4]),mean(xxx.lims[4:5]))
xxx.dat <- data.frame(
  x.yr=round(mrp.hydro$total_twh),
  y.yr=1:6,
  Year=mrp.hydro$Year
)
xxx.hydro <- NULL
for(i in 1:6) {
  x.1 <- data.frame(

    x1=c(rep(mean(xxx.lims[1])-xxx.dat[i,1]/2,2),
         rep(mean(xxx.lims[1])+xxx.dat[i,1]/2,2)),
    y1=rep(c(xxx.dat[i,2]-.25,xxx.dat[i,2]+.25),2),
    ord=c(1,2,4,3),
    Year=xxx.dat$Year[i],
    grp=i
  )
  xxx.hydro <- rbind(xxx.hydro,x.1)
}
xxx.hydro <- xxx.hydro %>% arrange(Year,ord)
xxx.hydro2 <- data.frame(
  x1=c(seq(40,293,1),
       seq(35.5,307.5,1),
       seq(20.5,312.5,1),
       seq(31.5,301.5,1),
       seq(39,294,1),
       seq(26.5,306.5,1)),
  y1=c(runif(254,.75,1.25),
       runif(273,1.75,2.25),
       runif(293,2.74,3.25),
       runif(271,3.75,4.24),
       runif(256,4.75,5.25),
       runif(281,5.75,6.25)),
  grp=c(rep(1,254),
        rep(2,273),
        rep(3,293),
        rep(4,271),
        rep(5,256),
        rep(6,281))
)

## wind data
xxx.dat <- data.frame(
  x.yr=round(mrp.wind$total_twh),
  y.yr=1:6,
  Year=mrp.wind$Year
)
xxx.wind <- NULL
for(i in 1:6) {
  x.1 <- data.frame(
    #x1=c(rep(525-xxx.dat[i,1]/2,2),rep(525+xxx.dat[i,1]/2,2)),
    x1=c(rep(mean(xxx.lims[2])-xxx.dat[i,1]/2,2),
         rep(mean(xxx.lims[2])+xxx.dat[i,1]/2,2)),
    
    y1=rep(c(xxx.dat[i,2]-.25,xxx.dat[i,2]+.25),2),
    ord=c(1,2,4,3),
    Year=xxx.dat$Year[i],
    grp=i
  )
  xxx.wind <- rbind(xxx.wind,x.1)
}
xxx.wind <- xxx.wind %>% arrange(Year,ord)
xxx.wind2 <- data.frame(
  x1=c(seq(495.5,498.5,.5),
       seq(494,500,.5),
       seq(449.5,544.5,1),
       seq(328,666,1)),
  y1=c(runif(7,2.75,3.25),
       runif(13,3.75,4.25),
       runif(96,4.75,5.25),
       runif(339,5.75,6.25)),
  grp=c(rep(3,7),
        rep(4,13),
        rep(5,96),
        rep(6,339))
)

## solar data
xxx.dat <- data.frame(
  x.yr=round(mrp.solar$total_twh),
  y.yr=1:6,
  Year=mrp.solar$Year
)
xxx.solar <- NULL
for(i in 1:6) {
  x.1 <- data.frame(
    #x1=c(rep(875-xxx.dat[i,1]/2,2),rep(875+xxx.dat[i,1]/2,2)),
    x1=c(rep(mean(xxx.lims[3])-xxx.dat[i,1]/2,2),
         rep(mean(xxx.lims[3])+xxx.dat[i,1]/2,2)),
    
    y1=rep(c(xxx.dat[i,2]-.25,xxx.dat[i,2]+.25),2),
    ord=c(1,2,4,3),
    Year=xxx.dat$Year[i],
    grp=i
  )
  xxx.solar <- rbind(xxx.solar,x.1)
}
xxx.solar <- xxx.solar %>% arrange(Year,ord)
xxx.solar2 <- data.frame(
  x1=c(seq(740.5,741.5,.5),
       seq(675.5,806.5,1)),
  y1=c(runif(3,4.75,5.25),
       runif(132,5.75,6.25)),
  grp=c(rep(5,3),
        rep(6,132))
)

## other data
xxx.dat <- data.frame(
  x.yr=round(mrp.other$total_twh),
  y.yr=1:6,
  Year=mrp.other$Year
)
xxx.other <- NULL
for(i in 1:6) {
  x.1 <- data.frame(
    #x1=c(rep(1225-xxx.dat[i,1]/2,2),rep(1225+xxx.dat[i,1]/2,2)),
    x1=c(rep(mean(xxx.lims[4])-xxx.dat[i,1]/2,2),
         rep(mean(xxx.lims[4])+xxx.dat[i,1]/2,2)),
    
    y1=rep(c(xxx.dat[i,2]-.25,xxx.dat[i,2]+.25),2),
    ord=c(1,2,4,3),
    Year=xxx.dat$Year[i],
    grp=i
  )
  xxx.other <- rbind(xxx.other,x.1)
}
xxx.other <- xxx.other %>% arrange(Year,ord)
xxx.other2 <- data.frame(
  x1=c(seq(845.5,861.5,1),
       seq(840,867,1),
       seq(825,882,1),
       seq(816,891,1),
       seq(816.5,890.5,1),
       seq(817,890,1)),
  y1=c(runif(17,.75,1.25),
       runif(28,1.75,2.25),
       runif(58,2.75,3.25),
       runif(76,3.75,4.25),
       runif(75,4.75,5.25),
       runif(74,5.75,6.25)),
  grp=c(rep(1,17),
        rep(2,28),
        rep(3,58),
        rep(4,76),
        rep(5,75),
        rep(6,74))
)

## GET FONT
font_add_google("Shadows Into Light","sil")
showtext_auto()


## Viz 1
ggplot()+
  geom_polygon(data=xxx.hydro,aes(x=x1,y=y1,group=grp),fill="blue")+
  geom_polygon(data=xxx.wind,aes(x=x1,y=y1,group=grp),fill="gray50")+
  geom_polygon(data=xxx.solar,aes(x=x1,y=y1,group=grp),fill="gold")+
  geom_polygon(data=xxx.other,aes(x=x1,y=y1,group=grp),fill="darkgreen")+
  geom_text(data=data.frame(
    x=c(xxx.lims,1050),
    y=rep(6.75,5),
    label=c("Hydro","Wind","Solar","Other","% of Energy")),
    aes(x=x,y=y,label=label),size=9,fontface="italic"
  )+
  geom_text(data=data.frame(
    x=rep(-30,6),
    y=1:6,
    label=seq(1970,2020,10)
  ),aes(x=x,y=y,label=label),size=8)+
  geom_text(data=data.frame(
    x=rep(xxx.lims[1],6),
    y=seq(1.4,6.4,1),
    label=paste(round(mrp.hydro$total_twh),"TWh")
  ),aes(x=x,y=y,label=label),size=5)+
  geom_text(data=data.frame(
    x=rep(xxx.lims[2],6),
    y=seq(1.4,6.4,1),
    label=paste(round(mrp.wind$total_twh),"TWh")
  ),aes(x=x,y=y,label=label),size=5)+
  geom_text(data=data.frame(
    x=rep(xxx.lims[3],6),
    y=seq(1.4,6.4,1),
    label=paste(round(mrp.solar$total_twh),"TWh")
  ),aes(x=x,y=y,label=label),size=5)+
  geom_text(data=data.frame(
    x=rep(xxx.lims[4],6),
    y=seq(1.4,6.4,1),
    label=paste(round(mrp.other$total_twh),"TWh")
  ),aes(x=x,y=y,label=label),size=5)+
  geom_text(data=rse,aes(x=x,y=y,label=paste0(round(percent,1),"%")),
            size=8)+
  labs(
    title="U.S. Renewable Energy - Percent of Total Energy 1970-2020",
    subtitle="(Terawatt hours (TWh) provided for renewable energy sources)",
    caption="Source: OWID - Renewable Energy"
  )+
  xlim(-50,1150)+
  ylim(.75,6.75)+
  theme_void()+
  theme(
    plot.title=element_text(size=32,hjust=.5),
    plot.subtitle=element_text(size=18,hjust=.5),
    plot.caption=element_text(size=16,hjust=.5),
    plot.background = element_rect(fill="#E6FFE6"),
    panel.background = element_rect(fill="#E6FFE6",color="#E6FFE6")
  )
ggsave("renewable_energy_US_v1.png",dpi=320,height=4,width=6)
    
## Viz 2

ggplot()+
  geom_line(data=xxx.hydro2,aes(x=x1,y=y1,group=grp),col="blue")+
  geom_line(data=xxx.wind2,aes(x=x1,y=y1,group=grp),col="gray50")+
  geom_line(data=xxx.solar2,aes(x=x1,y=y1,group=grp),col="gold")+
  geom_line(data=xxx.other2,aes(x=x1,y=y1,group=grp),col="darkgreen")+
  geom_text(data=data.frame(
    x=c(xxx.lims,1000),
    y=rep(6.75,5),
    label=c("Hydro","Wind","Solar","Other","% of Energy")),
    aes(x=x,y=y,label=label),size=7,fontface="italic",family="sil"
  )+
  geom_text(data=data.frame(
    x=rep(-30,6),
    y=1:6,
    label=seq(1970,2020,10)
  ),aes(x=x,y=y,label=label),size=6,family="sil")+
  geom_text(data=data.frame(
    x=rep(xxx.lims[1],6),
    y=seq(1.4,6.4,1),
    label=paste(round(mrp.hydro$total_twh),"TWh")
  ),aes(x=x,y=y,label=label),family="sil")+
  geom_text(data=data.frame(
    x=rep(xxx.lims[2],6),
    y=seq(1.4,6.4,1),
    label=paste(round(mrp.wind$total_twh),"TWh")
  ),aes(x=x,y=y,label=label),family="sil")+
  geom_text(data=data.frame(
    x=rep(xxx.lims[3],6),
    y=seq(1.4,6.4,1),
    label=paste(round(mrp.solar$total_twh),"TWh")
  ),aes(x=x,y=y,label=label),family="sil")+
  geom_text(data=data.frame(
    x=rep(xxx.lims[4],6),
    y=seq(1.4,6.4,1),
    label=paste(round(mrp.other$total_twh),"TWh")
  ),aes(x=x,y=y,label=label),family="sil")+
  geom_text(data=rse,aes(x=x,y=y,label=paste0(round(percent,1),"%")),
            size=6,family="sil")+
  labs(
    title="U.S. Renewable Energy - Percent of Total Energy 1970-2020",
    subtitle="(Terawatt hours (TWh) provided for renewable energy sources)",
    caption="Source: OWID - Renewable Energy"
  )+
  xlim(-50,1100)+
  ylim(.75,6.75)+
  theme_void()+
  theme(
    plot.title=element_text(size=24,hjust=.5),
    plot.subtitle=element_text(size=14,hjust=.5),
    plot.caption=element_text(size=10,hjust=.5),
    plot.background = element_rect(fill="#E6FFE6"),
    panel.background = element_rect(fill="#E6FFE6",color="#E6FFE6"),
    text=element_text(family="sil")
  )
ggsave("renewable_energy_US.png",dpi=320,height=7,width=10)
