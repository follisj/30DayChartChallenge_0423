library(tidyverse)
library(rvest)

#Scraping data from the nces.ed.gov website
#https://nces.ed.gov/programs/digest/d19/tables_3.asp#Ch3Sub25
#####################################################
#set up variables need to scrape the different tables
data.col.names <- c("Year","B.no","B.males","B.females","B.pct.female",
                    "M.tot","M.males","M.females",
                    "D.tot","D.males","D.females","field")

fields1 <- c("325.10","325.15","325.20","325.25",
             "325.30","325.35","325.40","325.45",
             "325.50","325.55","325.60",
             "325.65","325.70","325.80","325.85",
             "325.90","325.95")

fields2 <- c("Agriculture","Architecture","Biological/Biomedical","Business",
             "Communication","Computer Science","Education","Engineering",
             "English","Foreign Language","Health Professions",
             "Math/Stat","Physical Science","Psych","PubAdmin",
             "SocSci/Hist","Vis/Perf Arts")

#create a data frame for the scraped data
all.fields <- data.frame()
#scrape the data
for(i in 1:length(fields1)) {
  address <- paste0("https://nces.ed.gov/programs/digest/d19/tables/dt19_",fields1[i],".asp")
  
  h <- read_html(address)
  h2 <- h %>% html_nodes("table") %>% html_table(fill=T)
  h3 <- h2[[5]][-(1:3),-3]
  h3$names <- fields2[i]
  names(h3) <- data.col.names
  h3$Year <- ifelse(h3$Year=="",NA,h3$Year)
  h3$B.no <- ifelse(h3$B.no=="",NA,h3$B.no)
  h3 <- h3 %>% drop_na(Year) %>% drop_na(B.no)
  h3 <- h3[-grep("to",h3$Year),]
  
  all.fields <- rbind(all.fields,h3)
}

#clean up the data
all.fields[,2:11] <- data.frame(lapply(all.fields,function(x) {gsub(",","",x)})) %>% #remove commas from the numbers
  mutate_if(is.factor,as.character) %>% #change factor variables to character variables
  select(2:11) %>% #drop the first column
  mutate_if(is.character,as.numeric) #change character variables to numeric
all.fields$Year <- gsub("1999-2000","1999-00",all.fields$Year) #change 1999-00 to 1999-2000

year.all <- data.frame(table(all.fields$Year)) %>% filter(Freq > 16) #create a data set that has the years of data needed
names(year.all) <- c("Year","Freq") #rename the columns
all.fields <- left_join(year.all,all.fields) #add year to data set

#Create a data set with fields ordered by frequency

all.fields2 <- all.fields %>% filter(Year=="2017-18") %>% 
  mutate(field2=fct_reorder(field,B.pct.female),
         diff=B.pct.female-all.fields %>% filter(Year=="1970-71") %>% 
           select(B.pct.female)) %>%
  select(field,field2,diff) %>%
  right_join(all.fields,by="field")


#### create viz

all.fields2_r <- all.fields2 %>%
  filter(Year=="1970-71" | Year=="2017-18") %>%
  mutate(Year2=ifelse(Year=="1970-71",1,4))

ggplot(all.fields2_r,aes(x=Year2,y=B.pct.female,group=field2))+
  
  geom_point(data=all.fields2_r %>% filter(field2 %in% STEM.field),
             aes(x=Year2,y=B.pct.female,group=field2,col=field2),size=3)+
  geom_point(data=all.fields2_r %>% filter(!field2 %in% STEM.field),
             aes(x=Year2,y=B.pct.female,group=field2), col='gray',size=1)+
  
  geom_line(data=all.fields2_r %>% filter(field2 %in% STEM.field),
            aes(x=Year2,y=B.pct.female,group=field2,col=field2),size=1)+
  geom_line(data=all.fields2_r %>% filter(!field2 %in% STEM.field),
            aes(x=Year2,y=B.pct.female,group=field2), col='gray',lty=2)+
  geom_vline(xintercept=c(1,4),linewidth=.2,alpha=.5,lty=2)+
  expand_limits(x=c(0,5))+
  labs(title="Percentage of Bachelors Degrees Awarded to Females in STEM\n1971-2018")+
  
  ## adding labels
  ### Left Side
  geom_text(data=all.fields2_r %>% 
              filter(field2 %in% STEM.field) %>% 
              filter(field2 != "Physical Science"),
            aes(x=0.2,y=ifelse(Year=="1970-71",B.pct.female,NA),
                label=field2,color=field2),size=5)+ ## STEM labels (w/o Physical Science)
  geom_text(data=all.fields2_r %>% filter(field2 == "Physical Science"),
            aes(x=0.2,y=ifelse(Year=="1970-71",B.pct.female+1.5,NA),
                label=field2,color=field2),size=5)+ ## Adding Physical Science (remove overlap with Computer Science)
  geom_text(data=all.fields2_r %>% filter(!field2 %in% STEM.field),
            aes(x=0.2,y=ifelse(Year=="1970-71",B.pct.female,NA),
                label=field2),col="gray")+  ## Other Disciplines
  
  geom_text(data=all.fields2_r %>% filter(field2 %in% STEM.field) %>% 
              filter(field2 != "Physical Science"),
            aes(x=0.8,y=ifelse(Year=="1970-71",B.pct.female,NA),
                label=paste0(B.pct.female,"%"),color=field2),size=5)+  ## STEM % (w/o Physical Science)
  geom_text(data=all.fields2_r %>% filter(field2 == "Physical Science"),
            aes(x=0.8,y=ifelse(Year=="1970-71",B.pct.female+1.5,NA),
                label="13.8%",color=field2),size=5)+ ## Adding Physical Science (remove overlap with Computer Science)
  geom_text(data=all.fields2_r %>% filter(!field2 %in% STEM.field),
            aes(x=0.8,y=ifelse(Year=="1970-71",B.pct.female,NA),
                label=paste0(B.pct.female,"%")),col="gray")+  ## Other Disciplines
  
  ### Right Side
  geom_text(data=all.fields2_r %>% filter(field2 %in% STEM.field) %>%
              filter(field2 != "Engineering"),
            aes(x=4.75,y=ifelse(Year=="2017-18",B.pct.female,NA),
                label=field2,color=field2),size=5)+  ## STEM labels
  geom_text(data=all.fields2_r %>% filter(field2 == "Engineering"),
            aes(x=4.75,y=ifelse(Year=="2017-18",B.pct.female+1,NA),
                label=field2,color=field2),size=5)+ ## Adding Engineering (remove overlap with Computer Science)
  
  geom_text(data=all.fields2_r %>% filter(!field2 %in% STEM.field) %>% 
              filter(!field2 %in% c("Business","Architecture","Vis/Perf Arts")),
            aes(x=4.7,y=ifelse(Year=="2017-18",B.pct.female,NA),
                label=field2),color="gray")+  ## Other Disciples (w/o Business & Architecture,VPA)
  geom_text(data=all.fields2_r %>% filter(field2=="Business"),
            aes(x=4.7,y=ifelse(Year=="2017-18",B.pct.female,NA),
                label="Business/Architecture"),color="gray")+ ## Adding Business & Architecture labels (remove overlap)
  geom_text(data=all.fields2_r %>% filter(field2=="Vis/Perf Arts"),
            aes(x=4.7,y=ifelse(Year=="2017-18",B.pct.female-1,NA),
                label=field2),color="gray")+ ## Adding Vis/Perf Arts labels (remove overlap)
  
  geom_text(data=all.fields2_r %>% filter(field2 %in% STEM.field) %>%
              filter(field2 != "Engineering"),
            aes(x=4.2,y=ifelse(Year=="2017-18",B.pct.female,NA),label=paste0(B.pct.female,"%"),
                color=field2),size=5)+ ## STEM %
  geom_text(data=all.fields2_r %>% filter(field2 == "Engineering"),
            aes(x=4.2,y=ifelse(Year=="2017-18",B.pct.female+1,NA),
                label=paste0(B.pct.female,"%"),color=field2),size=5)+ ## Adding Engineering (remove overlap with Computer Science)
  
  geom_text(data=all.fields2_r %>% filter(!field2 %in% STEM.field) %>% 
              filter(!field2 %in% c("Business","Architecture","Vis/Perf Arts")),
            aes(x=4.2,y=ifelse(Year=="2017-18",B.pct.female,NA),label=paste0(B.pct.female,"%")),
            col="gray")+ ## Other Disciples (w/o Business & Architecture,Vis/Perf Arts)
  geom_text(data=all.fields2_r %>% filter(field2=="Business"),
            aes(x=4.2,y=ifelse(Year=="2017-18",B.pct.female,NA),label=paste0(B.pct.female,"%")),
            col="gray")+ ## Adding Business & Architecture %
  geom_text(data=all.fields2_r %>% filter(field2=="Vis/Perf Arts"),
            aes(x=4.2,y=ifelse(Year=="2017-18",B.pct.female-1,NA),label=paste0(B.pct.female,"%")),
            col="gray")+ 
  
  scale_x_continuous(breaks=c(-1,0,1,2,3,4),labels=c("","","1971","","","2018"))+
  scale_color_manual(values=c("#177e89","#084c61","#db3a34","#fdc500","#323031"))+
  theme_minimal()+
  theme(legend.position="none",
        axis.text.x=element_text(size=14),
        panel.border=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #panel.grid.minor.x=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.title=element_text(hjust=.5,size=24),
        plot.subtitle=element_text(hjust=.5,size=12),
        axis.text.y=element_blank()
  )
ggsave("pct_female_deg_71_18.png",dpi=320,height=12,width=14,bg="white")









