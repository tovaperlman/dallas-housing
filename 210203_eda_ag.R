library(readxl)
library(lubridate)
library(dplyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

ES <- read_excel("UPenn.xlsx", sheet = "ES")
SH <- read_excel("UPenn.xlsx", sheet = "SH")
SO <- read_excel("UPenn.xlsx", sheet = "SO")
TH <- read_excel("UPenn.xlsx", sheet = "TH")
PSH <- read_excel("UPenn.xlsx", sheet = "PSH")
RRH <- read_excel("UPenn.xlsx", sheet = "RRH")
PH <- read_excel("UPenn.xlsx", sheet = "PH")

dat <- rbind(ES,SH,SO,TH,PSH,RRH,PH)

write.csv(dat, file = "client_dat.csv")

serv_ES <- read_excel("UPenn Services 100114 - 123120.xlsx", sheet = "ES")
serv_SH <- read_excel("UPenn Services 100114 - 123120.xlsx", sheet = "SH")
serv_SO <- read_excel("UPenn Services 100114 - 123120.xlsx", sheet = "SO")
serv_TH <- read_excel("UPenn Services 100114 - 123120.xlsx", sheet = "TH")
serv_PSH <- read_excel("UPenn Services 100114 - 123120.xlsx", sheet = "PSH")
serv_RRH <- read_excel("UPenn Services 100114 - 123120.xlsx", sheet = "RRH")
serv_OPH <- read_excel("UPenn Services 100114 - 123120.xlsx", sheet = "OPH")

serv_dat <- rbind(serv_ES,serv_SH,serv_SO,serv_TH,serv_PSH,serv_RRH,serv_OPH)

write.csv(serv_dat, file = "services_dat.csv")

study <- subset(TH_count, n > 1 ) 
#none

#check if the Srevice Date Matches the Enroll Date

case_serv_TH <- serv_TH %>%
  dplyr::filter (Service == "Case Management")

ES_count <- serv_ES %>% 
  count(`Client ID`)

SH_count <- serv_SH %>% 
  count(`Client ID`)

SO_count <- serv_SO %>% 
  count(`Client ID`)

TH_count <- serv_TH %>% 
  count(`Client ID`)

PSH_count <- serv_PSH %>% 
  count(`Client ID`)

RRH_count <- serv_RRH %>% 
  count(`Client ID`)

OPH_count <- serv_OPH %>% 
  count(`Client ID`)

dem_count <- dat %>% 
  count(`clientid`)

serv_count <- serv_dat %>% 
  count(`Client ID`)


