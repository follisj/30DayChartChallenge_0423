## data from rottentomatoes.com
library(httr)
library(rvest)
library(tidyverse)
library(patchwork)

tom.names=data.frame(Actor1=c("tom_cruise","tom_hanks","tom_hardy","tom_sizemore","tommy_lee_jones","tom_berenger","tom_hiddleston"),
                     Actor2=c("Tom Cruise","Tom Hanks","Tom Hardy","Tom Sizemore","Tommy Lee Jones","Tom Berenger","Tom Hiddleston"))

tom.data=data.frame()

for (i in 1:nrow(tom.names)) {
  address=paste0("https://www.rottentomatoes.com/celebrity/",tom.names[i,1],"/")
  zero <- httr::GET(address)
  tables <- rvest::html_table(content(zero))
  tom.xxx <- tables[[1]]
  names(tom.xxx) <- c("Tomatometer","Audience_Score","Title","Credit","Box_Office","Year")
  tom.xxx$Tomatometer=gsub("\\%","",tom.xxx$Tomatometer)
  tom.xxx$Tomatometer=gsub("No Score Yet",NA,tom.xxx$Tomatometer)
  tom.xxx$Tomatometer=as.numeric(tom.xxx$Tomatometer)
  tom.xxx$Audience_Score=gsub("\\%","",tom.xxx$Audience_Score)
  tom.xxx$Audience_Score=gsub("No Score Yet",NA,tom.xxx$Audience_Score)
  tom.xxx$Audience_Score=as.numeric(tom.xxx$Audience_Score)
  tom.xxx$Box_Office=gsub("M","",tom.xxx$Box_Office)
  tom.xxx$Box_Office=gsub("[[:punct:]]","",tom.xxx$Box_Office)
  tom.xxx$Box_Office=as.numeric(tom.xxx$Box_Office)/10
  #tom.xxx$Credit<-gsub("\\n+\\s*"," ",tom.xxx$Credit)
  tom.xxx$actor <- tom.names[i,2]
  tom.data=rbind(tom.data,tom.xxx)
}
tom.data <- tom.data %>% drop_na()
tom.data <- tom.data %>%
  filter(grepl("Character",Credit))
tom.data$critic_fresh <- ifelse(tom.data$Tomatometer>=60,"Fresh","Rotten")
tom.data$audience_fresh <- ifelse(tom.data$Audience_Score>=60,"Fresh","Rotten")
tom.data$Title <- gsub("Mission: Impossible","MI",tom.data$Title)
tom.data$Title <- gsub("A Beautiful Day in the Neighborhood","A Beautiful Day...",
                       tom.data$Title)

tom.plots <- function(data,tom) {
  data %>% filter(actor==tom) %>%
    ggplot()+
    geom_segment(aes(x=0,xend=110,y=60,yend=60),col="grey30",lty=2,alpha=.5,linewidth=.25)+
    geom_segment(aes(x=60,xend=60,y=0,yend=100),col="grey30",lty=2,alpha=.5,linewidth=.25)+
    geom_point(aes(Tomatometer,Audience_Score),col="#000075")+
    ggrepel::geom_text_repel(aes(Tomatometer,Audience_Score,label=Title),size=4,col="#000075")+
    geom_text(aes(x=112,y=60,label="60"))+
    geom_text(aes(x=60,y=102,label="60"))+
    geom_text(data=data.frame(x=c(30,90),y=c(108,108),label=c("Rotten","Fresh"),hue=c("a","b")),
              aes(x=x,y=y,label=label,col=hue),size=5.5)+
    geom_text(aes(x=60,y=108,label="Tomatometer"),size=6.5)+
    geom_text(data=data.frame(x=c(118,118),y=c(30,90),label=c("Rotten","Fresh"),hue=c("a","b")),
              aes(x=x,y=y,label=label,col=hue),angle=270,size=5.5)+
    geom_text(aes(x=118,y=60,label="Audience Score"),angle=270,size=6.5)+
    scale_color_manual(values=c("#00A300","#FF0000"))+
    xlim(-1,125)+
    ylim(0,110)+
    #labs(subtitle=tom)+
    theme_void()+
    annotate("text",x=10,y=90,label=tom.names$Actor2[i],size=8,angle=45)+
    theme(#plot.subtitle=element_text(size=20,hjust=.5,face="italic"),
          panel.background = element_rect(fill="#E3ECF2",color="#E3ECF2"),
          plot.background = element_rect(fill="#E3ECF2"),
          legend.position="none")
}

for(i in 1:nrow(tom.names)) {
  xxx <- tom.plots(tom.data,tom.names$Actor2[i])
  assign(tom.names$Actor1[i],xxx)
}

plot_gray <- ggplot()+
  theme_void()+
  theme(plot.background = element_rect(fill="gray80"),
        panel.background = element_rect(fill="gray80",color="gray80"))

((tom_cruise+plot_gray+tom_hanks+
    plot_layout(widths=c(1,.025,1)))/
  plot_gray/
  (tom_berenger+plot_gray+tom_sizemore+plot_layout(widths=c(1,.025,1)))/
  (plot_gray)/
  (tom_hardy+plot_gray+tom_hiddleston+plot_layout(widths=c(1,.025,1))))+
  plot_layout(heights=c(1,.025,1,.05,1))+
  plot_annotation(title="The Tom,...,Tom Club",
                  subtitle="The Good (Fresh) and Bad (Rotten) Movies from Rotten Tomatoes' Ratings",
  theme=theme(plot.title=element_text(size=32,hjust=.5),
        plot.subtitle=element_text(size=16,hjust=.5),
        plot.margin=margin(15,25,25,25),
        plot.background = element_rect(fill="gray80")
        ))

ggsave("tom_tom_club.png",dpi=320,height=22,width=24)
