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

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

ES <- read_excel("UPenn.xlsx", sheet = "ES")
SH <- read_excel("UPenn.xlsx", sheet = "SH")
SO <- read_excel("UPenn.xlsx", sheet = "SO")
TH <- read_excel("UPenn.xlsx", sheet = "TH")
PSH <- read_excel("UPenn.xlsx", sheet = "PSH")
RRH <- read_excel("UPenn.xlsx", sheet = "RRH")
PH <- read_excel("UPenn.xlsx", sheet = "PH")

dat <- rbind(ES,SH,SO,TH,PSH,RRH,PH)

#write.csv(dat, file = "client_dat.csv")

serv_ES <- read_excel("UPenn Services 100114 - 123120.xlsx", sheet = "ES") 

ES_count <- serv_ES %>% 
  count(`Client ID`)
  
serv_ES_recent <- serv_ES %>%
  group_by(`Client ID`) %>%
  slice(which.max(`Service Date`)) %>%
  rename (recent_ES_service_date = `Service Date`)

ES_clients <- merge(serv_ES_recent, ES_count, by = "Client ID", all.x = TRUE) %>%
  mutate (ES = 1) %>%
  rename(n_ES = n)


serv_SH <- read_excel("UPenn Services 100114 - 123120.xlsx", sheet = "SH") 

SH_count <- serv_SH %>% 
  count(`Client ID`)

serv_SH_recent <- serv_SH %>%
  group_by(`Client ID`) %>%
  slice(which.max(`Service Date`)) %>%
  rename (recent_SH_service_date = `Service Date`)

SH_clients <- merge(serv_SH_recent, SH_count, by = "Client ID", all.x = TRUE) %>%
  mutate(SH = 1) %>%
  rename(n_SH = n)


serv_SO <- read_excel("UPenn Services 100114 - 123120.xlsx", sheet = "SO")

SO_count <- serv_SO %>% 
  count(`Client ID`)

serv_SO_recent <- serv_SO %>%
  group_by(`Client ID`) %>%
  slice(which.max(`Service Date`)) %>%
  rename (recent_SO_service_date = `Service Date`)

SO_clients <- merge(serv_SO_recent, SO_count, by = "Client ID", all.x = TRUE) %>%
  mutate(SO = 1) %>%
  rename(n_SO = n)


serv_TH <- read_excel("UPenn Services 100114 - 123120.xlsx", sheet = "TH")

TH_count <- serv_TH %>% 
  count(`Client ID`)

serv_TH_recent <- serv_TH %>%
  group_by(`Client ID`) %>%
  slice(which.max(`Service Date`)) %>%
  rename (recent_TH_service_date = `Service Date`)

TH_clients <- merge(serv_TH_recent, TH_count, by = "Client ID", all.x = TRUE) %>%
  mutate(TH = 1) %>%
  rename(n_TH = n)

serv_PSH <- read_excel("UPenn Services 100114 - 123120.xlsx", sheet = "PSH")

PSH_count <- serv_PSH %>% 
  count(`Client ID`)

serv_PSH_recent <- serv_PSH %>%
  group_by(`Client ID`) %>%
  slice(which.max(`Service Date`)) %>%
  rename (recent_PSH_service = `Service Date`)

PSH_clients <- merge(serv_PSH_recent, PSH_count, by = "Client ID", all.x = TRUE) %>%
  mutate(PSH = 1) %>%
  rename(n_PSH = n)

serv_RRH <- read_excel("UPenn Services 100114 - 123120.xlsx", sheet = "RRH")

RRH_count <- serv_RRH %>% 
  count(`Client ID`)

serv_RRH_recent <- serv_RRH %>%
  group_by(`Client ID`) %>%
  slice(which.max(`Service Date`)) %>%
  rename (recent_RRH_service = `Service Date`)

RRH_clients <- merge(serv_RRH_recent, RRH_count, by = "Client ID", all.x = TRUE) %>%
  mutate(RRH = 1) %>%
  rename(n_RRH = n)

serv_OPH <- read_excel("UPenn Services 100114 - 123120.xlsx", sheet = "OPH")

OPH_count <- serv_OPH %>% 
  count(`Client ID`)

serv_OPH_recent <- serv_OPH %>%
  group_by(`Client ID`) %>%
  slice(which.max(`Service Date`)) %>%
  rename (recent_OPH_service = `Service Date`)

OPH_clients <- merge(serv_OPH_recent, OPH_count, by = "Client ID", all.x = TRUE) %>%
  mutate(OPH = 1) %>%
  rename(n_OPH = n)

serv_dat <- rbind(serv_ES,serv_SH,serv_SO,serv_TH,serv_PSH,serv_RRH,serv_OPH)
#write.csv(serv_dat, file = "services_dat.csv")

dat <- merge (dat, RRH_clients, by.x = "clientid", by.y = "Client ID", all.x = TRUE) %>%
  dplyr::select (-`Associated Program`, -`Service`, -`Enroll ID`, -`Family ID`)

dat <- merge (dat, OPH_clients, by.x = "clientid", by.y = "Client ID", all.x = TRUE) %>%
  dplyr::select (-`Associated Program`, -`Service`, -`Enroll ID`, -`Family ID`)

dat <- merge (dat, PSH_clients, by.x = "clientid", by.y = "Client ID", all.x = TRUE) %>%
  dplyr::select (-`Associated Program`, -`Service`, -`Enroll ID`, -`Family ID`)

dat <- merge (dat, TH_clients, by.x = "clientid", by.y = "Client ID", all.x = TRUE) %>%
  dplyr::select (-`Associated Program`, -`Service`, -`Enroll ID`, -`Family ID`)

dat <- merge (dat, SO_clients, by.x = "clientid", by.y = "Client ID", all.x = TRUE) %>%
  dplyr::select (-`Associated Program`, -`Service`, -`Enroll ID`, -`Family ID`)

dat <- merge (dat, SH_clients, by.x = "clientid", by.y = "Client ID", all.x = TRUE) %>%
  dplyr::select (-`Associated Program`, -`Service`, -`Enroll ID`, -`Family ID`)

dat <- merge (dat, ES_clients, by.x = "clientid", by.y = "Client ID", all.x = TRUE) %>%
  dplyr::select (-`Associated Program`, -`Service`, -`Enroll ID`, -`Family ID`)

dat$Race <- NA
dat$Race[which (dat$Races == "White")] <- "White" 
dat$Race[which (dat$Races == "Black or African American")] <- "Black or African American" 
dat$Race[which (dat$Races == "American Indian or Alaska Native")] <- "American Indian or Alaska Native" 
dat$Race[which (grepl(",", dat$Races, fixed=TRUE))] <- "Two or More Races"

#write.csv(dat, file = "210207_client_dat_race_by_program_type_eda_ag.csv")

#revise the following with 6 levels over the x axis, adjacent bar chart https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
#or make a plot for each race

#data2vis
#stacked bar chart, 

#average  number of services per month

mean

ggplot(data = dat %>%
         filter(Race == "White"|Race ==  "Black or African American"|Race ==  "American Indian or Alaska Native" |Race ==  "Two or More Races") %>%
         filter(ES == 1) %>%
         group_by(Race) %>%
         count(Race) %>%
         rename (`Number of Clients`= n))+
  geom_bar(aes(x=Race, y=`Number of Clients`, fill=Race), stat="identity", position=position_dodge(), show.legend = FALSE, width=0.5) +
  scale_fill_manual(values = palette4) + 
  labs(title= "Racial Makup of Clients Receiving Emergency Shelter Services") +
  plotTheme

ggplot(data = dat %>%
         filter(Race == "White"|Race ==  "Black or African American"|Race ==  "American Indian or Alaska Native" |Race ==  "Two or More Races") %>%
         filter(SO == 1) %>%
         group_by(Race) %>%
         count(Race) %>%
         rename (`Number of Clients`= n))+
  geom_bar(aes(x=Race, y=`Number of Clients`, fill=Race), stat="identity", position=position_dodge(), show.legend = FALSE, width=0.5) +
  scale_fill_manual(values = palette4) + 
  labs(title= "Racial Makup of Clients Receiving Street Outreach Services") +
  plotTheme

ggplot(data = dat %>%
         filter(Race == "White"|Race ==  "Black or African American"|Race ==  "American Indian or Alaska Native" |Race ==  "Two or More Races") %>%
         filter(SH == 1) %>%
         group_by(Race) %>%
         count(Race) %>%
         rename (`Number of Clients`= n))+
  geom_bar(aes(x=Race, y=`Number of Clients`, fill=Race), stat="identity", position=position_dodge(), show.legend = FALSE, width=0.5) +
  scale_fill_manual(values = palette4) + 
  labs(title= "Racial Makup of Clients Receiving Safe Haven Services") +
  plotTheme

ggplot(data = dat %>%
         filter(Race == "White"|Race ==  "Black or African American"|Race ==  "American Indian or Alaska Native" |Race ==  "Two or More Races") %>%
         filter(RRH == 1) %>%
         group_by(Race) %>%
         count(Race) %>%
         rename (`Number of Clients`= n))+
  geom_bar(aes(x=Race, y=`Number of Clients`, fill=Race), stat="identity", position=position_dodge(), show.legend = FALSE, width=0.5) +
  scale_fill_manual(values = palette4) + 
  labs(title= "Racial Makup of Clients Receiving Rapid Re-Housing Services") +
  plotTheme

ggplot(data = dat %>%
         filter(Race == "White"|Race ==  "Black or African American"|Race ==  "American Indian or Alaska Native" |Race ==  "Two or More Races") %>%
         filter(PSH == 1) %>%
         group_by(Race) %>%
         count(Race) %>%
         rename (`Number of Clients`= n))+
  geom_bar(aes(x=Race, y=`Number of Clients`, fill=Race), stat="identity", position=position_dodge(), show.legend = FALSE, width=0.5) +
  scale_fill_manual(values = palette4) + 
  labs(title= "Racial Makup of Clients Receiving Permanent Supportive Housing Services") +
  plotTheme

ggplot(data = dat %>%
         filter(Race == "White"|Race ==  "Black or African American"|Race ==  "American Indian or Alaska Native" |Race ==  "Two or More Races") %>%
         filter(TH == 1) %>%
         group_by(Race) %>%
         count(Race) %>%
         rename (`Number of Clients`= n))+
  geom_bar(aes(x=Race, y=`Number of Clients`, fill=Race), stat="identity", position=position_dodge(), show.legend = FALSE, width=0.5) +
  scale_fill_manual(values = palette4) + 
  labs(title= "Racial Makup of Clients Receiving Transitional Housing Services") +
  plotTheme
