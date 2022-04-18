library(worldfootballR)  #for scraping
library(tidyverse)       #for ggplot, dplyr and several other stuff
library(forcats)         #for sorting within ggplot
library(glue)            #easier than paste()

# you can choose your own player through the url below
df <- fb_player_scouting_report("https://fbref.com/en/players/70d74ece/Karim-Benzema")
head(df)

df_selected <- df[c(1,2,3,11,13,73,28,29,47,117,125,110,48,87,106,147),]



df_selected <- df_selected %>% 
  mutate(stat=case_when(Statistic == "Non-Penalty Goals"|
                          Statistic == "Goals"|
                          Statistic == "Shots Total"|
                          Statistic == "Assists"|
                          Statistic == "xA"|
                          Statistic == "npxG+xA"|
                          Statistic == "Shot-Creating Actions" ~ "Attacking",
                        Statistic == "Passes Attempted"|
                          Statistic == "Pass Completion %"|
                          Statistic == "Progressive Passes"|
                          Statistic == "Progressive Carries"|
                          Statistic == "Dribbles Completed"|
                          Statistic == "Touches"~ "Possession",
                        TRUE ~ "Defending"))


# impage parameters

temp <- (360/(nrow(df_selected))/2)                             #find the difference in angle between to labels and divide by two.
myAng <- seq(-temp, -360+temp, length.out = nrow(df_selected))  #get the angle for every label
ang<-ifelse(myAng < -90, myAng+180, myAng)                                    #rotate label by 180 in some places for readability
ang<-ifelse(ang < -90, ang+180, ang)     



df_selected$Statistic <- gsub(" ","\n",df_selected$Statistic)





# Start ploting


ggplot(df_selected,aes(fct_reorder(Statistic,stat),Percentile)) +                       #select the columns to plot and sort it so the types of metric are grouped
  geom_bar(aes(y=100,fill=stat),stat="identity",width=1,colour="white",                 #make the whole pizza first
           alpha=0.5) +                                                                          #change alphe to make it more or less visible
  geom_bar(stat="identity",width=1,aes(fill=stat),colour="white") +                     #insert the values 
  coord_polar() +                                                                       #make it round
  geom_label(aes(label=Per90,fill=stat),size=2,color="white",show.legend = FALSE)+      #add a label for the value. Change 'label=Per.90' to 'label=Percentile' to show the percentiles
  scale_fill_manual(values=c("Possession" = "#D70232",                                   #choose colors to fill the pizza parts
                             "Attacking" = "#1A78CF",
                             "Defending" = "#FF9300")) +                                                              
  scale_y_continuous(limits = c(-10,100))+                                              #create the white part in the middle.   
  labs(fill="",   
       caption = "Data from StatsBomb via FBref",     
       #remove legend title
       title=glue("{df_selected$Player[1]} | Real Madrid"),
       subtitle = "2021-2022 | stats per 90 minutes")+ #let the title be te name of the player                                                
  
  theme_minimal() +                                                                     #from here it's only themeing. 
  theme(plot.background = element_rect(fill = "#F2F4F5",color = "#F2F4F5"),
        panel.background = element_rect(fill = "#F2F4F5",color = "#F2F4F5"),
        legend.position = "top",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 6, angle = ang),
        text = element_text(family="Spartan-Light"),                                    #I downloaded this font from Google Fonts. You can use your own font of course
        plot.title = element_markdown(hjust=0.5,family="Spartan-Medium"),
        plot.subtitle = element_text(hjust=0.5,size=8),
        plot.caption = element_text(hjust=0.5,size=6),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.margin = margin(5,2,2,2)) 

# Export the plot

ggsave("image.png",bg="#F2F4F5")