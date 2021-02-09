library(readxl)
library(lubridate)
library(dplyr)
library(ggplot2)

plotTheme <- theme(text = element_text( family = "Avenir", color = "black"),
                   plot.title =element_text(size=13, face = "bold"),
                   plot.subtitle = element_text(size=8),
                   plot.caption = element_text(size = 6),
                   axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
                   axis.title.x = element_text(size = 10),
                   axis.text.y = element_text(size = 8),
                   axis.title.y = element_text(size = 10),
                   # Set the entire chart region to blank
                   panel.background=element_blank(),
                   plot.background=element_blank(),
                   #panel.border=element_rect(colour="#F0F0F0"),
                   # Format the grid
                   panel.grid.major=element_line(colour="#565050",size=.2),
                   axis.ticks=element_blank())

palette4 <- c("#3a7d7c","#ffa137","#ff4400","#065125")
palette5 <- c("#03254c","#1167b1","#64b5f6","#2196f3","#bbdefb")
palette7 <- c('#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69')

#Read in data from CSV that AG put on box
AGdata <- read.csv("210207_client_dat_race_by_program_type_eda_ag.csv") 



#X axis is program type, and the fill and count is race
ggplot(data = AGdata %>%
         select(ProjectType, Race)%>%
         filter(Race == "White"|Race ==  "Black or African American"|Race ==  "American Indian or Alaska Native" |Race ==  "Two or More Races") %>%
         group_by(ProjectType, Race) %>%
         count(Race) %>%
         rename (`Number of Clients`= n))+
  geom_bar(aes(x=ProjectType, y=`Number of Clients`, fill=Race),
           position="dodge", stat="identity", width=0.5) +
  scale_fill_manual(values = palette4) + 
  labs(title= "Racial Makeup of Clients in Each Project Type") +
  plotTheme

#X axis is race, and the fill is project type - what is distribution of racial group across project types 
ggplot(data = AGdata %>%
         select (ProjectType, Race)%>%
         filter(Race == "White"|Race ==  "Black or African American"|Race ==  "American Indian or Alaska Native" |Race ==  "Two or More Races") %>%
         group_by(Race, ProjectType) %>%
         summarize(`Number of Clients`= n())) +
  geom_bar(aes(x=Race, y=`Number of Clients`, fill=ProjectType),
           position="dodge", stat="identity", width=0.5) +
  scale_fill_manual(values = palette7) + 
  labs(title= "Distribution of Project Type by Race") +
  plotTheme
