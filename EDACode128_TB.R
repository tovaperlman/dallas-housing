library(tidyverse)
library(sf)
library(readxl)
library(openxlsx)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(kableExtra)
library(ggalluvial)
library(stringr)
library(ggfittext)

setwd("C:/Users/ctb80/Box/Practicum_Dallas")

# Read demographic data
composite_file <- loadWorkbook('UPenn.xlsx')
fileNames <- sheets(composite_file)
for(i in 1:length(fileNames))
{
  assign(fileNames[i],readWorkbook(composite_file,sheet = i, detectDates = T))
}
# Rename demographic data files
TH_Dem <- TH%>% select(-ClientID)
SO_Dem <- SO
SH_Dem <- SH 
PSH_Dem <- PSH
RRH_Dem <- RRH
PH_Dem <- PH
ES_Dem <- ES

# Read Services Data
composite_file <- loadWorkbook('UPenn Services 100114 - 123120.xlsx')
fileNames <- sheets(composite_file)
for(i in 1:length(fileNames))
{
  assign(fileNames[i],readWorkbook(composite_file,sheet = i, detectDates = T))
}

TH$prog_type = 'TH'
PSH$prog_type = 'PSH'
OPH$prog_type = 'OPH'
RRH$prog_type = 'RRH'
SO$prog_type = 'SO'
ES$prog_type = 'ES'
SH$prog_type = 'SH'



# Alluvial plot # 1
# Just start with Transitional Housing into OPH, PH, RRH
# Join 3 tables to TH, group by TH, housed type, count unique ppl


TH$prog_type = 'TH'
PSH$prog_type = 'PSH'
OPH$prog_type = 'OPH'
RRH$prog_type = 'RRH'

aluv_test <- TH %>% select(Client.ID, prog_type) %>%
  rename(start=prog_type)

aluv_test <- left_join(aluv_test,
                       rbind(RRH %>% select(Client.ID, prog_type),
                             PSH %>% select(Client.ID, prog_type),
                             OPH %>% select(Client.ID, prog_type)),
                       by='Client.ID')

aluv_comb <- 
  unique(aluv_test) %>%
  count(start, prog_type)

# The most basic alluvial. Just shows that, from TH, ~80% don't get housing services, ~5% go to PSH and ~15% got to RRH
# Above percentages are eyeball estimates

ggplot(as.data.frame(aluv_comb),
       aes(y = n, axis1 = start, axis2 = prog_type)) +
  geom_alluvium(aes(fill=prog_type), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("start", "prog_type"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") #+


## Alluvial plot # 2
## This plot tracks unique individuals by homelessness service, race, and housing service

race_dem <- rbind(TH_Dem, SO_Dem, SH_Dem, PSH_Dem, RRH_Dem, PH_Dem, ES_Dem) %>%
  select(clientid, Races) %>%
  unique()

homeless <- rbind(SO, ES, TH, SH) %>%
  select(Client.ID, prog_type) %>%
  unique() %>%
  rename(homeless_serv=prog_type) %>%
  left_join(., race_dem, by=c("Client.ID" = "clientid"))

housed <- rbind(PSH, OPH, RRH) %>%
  select(Client.ID, prog_type) %>%
  unique() %>%
  rename(housed_serv=prog_type)

alluv <- left_join(homeless, housed) %>%
  count(homeless_serv, Races, housed_serv) %>%
  filter(n>60) %>% # Threhshold to help visualize
  filter(Races!='NA') # Remove NA to help visualize what we do know


# Unique individuals who have ever received any homelessness service and where they eventually ended up. 
# NB: If they received multiple services (homeless or housed), they are counted multiple times
cpalette <- c('#1b9e77', '#d95f02', '#7570b3', '#e7298a') # Not used here, but gives you more control over colors used

# This tracks all individuals who receive homelessness services, if/where they end up in housing services
# The bands separate out races
ggplot(as.data.frame(alluv),
       aes(y = n, axis1 = homeless_serv,  axis2 = housed_serv)) +
  geom_alluvium(aes(fill=Races), width = 1/12) +
  geom_stratum(width = 1/12, fill = "light grey", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("homeless_serv", "housed_serv"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set3", direction = 1)


## Alluvial Plot # 3
## This is the same as above, but adds in ethnicity ##
## This plot tracks unique individuals by homelessness service, race, and housing service

race_dem <- rbind(TH_Dem, SO_Dem, SH_Dem, PSH_Dem, RRH_Dem, PH_Dem, ES_Dem) %>%
  select(clientid, Races, HUDEthnicity) %>%
  unique()

homeless <- rbind(SO, ES, TH, SH) %>%
  select(Client.ID, prog_type) %>%
  unique() %>%
  rename(homeless_serv=prog_type) %>%
  left_join(., race_dem, by=c("Client.ID" = "clientid"))

housed <- rbind(PSH, OPH, RRH) %>%
  select(Client.ID, prog_type) %>%
  unique() %>%
  rename(housed_serv=prog_type)

alluv <- left_join(homeless, housed) %>%
  count(homeless_serv, Races, HUDEthnicity, housed_serv) %>%
  filter(n>10) %>%
  filter(Races!='NA') %>%
  filter(housed_serv != 'NA') # Remove those who didn't end up in housing services, so we can really see RRH v. PH v. OPH


# Unique individuals who have ever received any homelessness service and where they eventually ended up. 
# If they received multiple services (homeless or housed), they are counted multiple times
# There are very few Hispanic which is very surprising
# White/Black breakdown seems to be roughly consistent across different services

cpalette <- c('#1b9e77', '#d95f02', '#7570b3', '#e7298a')

ggplot(as.data.frame(alluv),
       aes(y = n, axis1 = homeless_serv, axis2 = HUDEthnicity,  axis3 = housed_serv)) +
  geom_alluvium(aes(fill=Races), width = 1/12) +
  geom_stratum(width = 1/12, fill = "light grey", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("homeless_serv", "housed_serv"), expand = c(.05, .05)) +
  #scale_fill_manual(values=c(cpalette))
  scale_fill_brewer(type = "qual", palette = "Set3", direction = -1)


## Alluvial plot # 4
## Track transitions from prior residence to exit destination
# This information is recorded in the demographics tables
# It is not yet clear how people are assigned to a demo table -- perhaps most recent type of service received?
# If above hypothesis is true, this plot would show the most recent prior --> exit that an individual went through

all_dem <- rbind(TH_Dem, SO_Dem, SH_Dem, PSH_Dem, RRH_Dem, PH_Dem, ES_Dem) %>%
  unique() # not necessary

priorToExit <- all_dem %>% 
       select(PriorResidence, Races, ExitDestination) %>%
       count(PriorResidence, Races, ExitDestination)

# Largest groups coming from Emergency shelter, place not meant for habitation
# Largest destination is no exit interview complete, followed by emergency shelter

ggplot(as.data.frame(priorToExit %>% filter(n>150)),
       aes(y = n, axis1 = PriorResidence, axis2 = ExitDestination, label = PriorResidence)) +
  geom_alluvium(aes(fill=Races), width = 1/4) +
  geom_stratum(width = 1/4, fill = "light grey", color = "grey") +
  #geom_label(stat = "stratum", aes(label = after_stat(stratum)), size=2) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size=2.75)+
  #scale_y_continuous(limits = c(0, 2.25)) +
  #ggfittext::geom_fit_text(stat='stratum', width = 1/4, min.size=3) +
  scale_x_discrete(limits = c("priorToExit", "ExitDestination"), expand = c(.05, .05)) +
  #scale_fill_manual(values=c(cpalette))
  scale_fill_brewer(type = "qual", palette = "Set3", direction = -1)


# TB TO DO: Data wrangling and aggregate statistics about recidivism



### ARCHIVE CODE ###

# 
# # The enrollment date
# 
# PH$EnrollMonth <- month(PH$EnrollDate)
# ES$EnrollMonth <- month(ES$EnrollDate)
# PSH$EnrollMonth <- month(PSH$EnrollDate)
# RRH$EnrollMonth <- month(RRH$EnrollDate)
# SH$EnrollMonth <- month(SH$EnrollDate)
# SO$EnrollMonth <- month(SO$EnrollDate)
# TH$EnrollMonth <- month(TH$EnrollDate)
# 
# h1 <- ggplot(PH, aes(EnrollMonth, )) + geom_histogram() + ggtitle('Permanent Housing')
# h2 <- ggplot(ES, aes(EnrollMonth)) + geom_histogram()+ ggtitle('Emergency Shelter')
# h3 <- ggplot(PSH, aes(EnrollMonth)) + geom_histogram()+ ggtitle('Permanent Supportive Housing')
# h4 <- ggplot(RRH, aes(EnrollMonth)) + geom_histogram()+ ggtitle('Rapid Re-Housing')
# h5 <- ggplot(SH, aes(EnrollMonth)) + geom_histogram()+ ggtitle('Safe Haven')
# h6 <- ggplot(SO, aes(EnrollMonth)) + geom_histogram()+ ggtitle('Street Outreach')
# h7 <- ggplot(TH, aes(EnrollMonth)) + geom_histogram()+ ggtitle('Transitional Housing')
# 
# 
# plotsList <- list(h1, h2, h3, h4,h5,h6,h7)
# 
# grid.arrange(grobs=plotsList, ncol=2)
# 
# # The exit date
# 
# PH$ExitMonth <- month(PH$ExitDate)
# ES$ExitMonth <- month(ES$ExitDate)
# PSH$ExitMonth <- month(PSH$ExitDate)
# RRH$ExitMonth <- month(RRH$ExitDate)
# SH$ExitMonth <- month(SH$ExitDate)
# SO$ExitMonth <- month(SO$ExitDate)
# TH$ExitMonth <- month(TH$ExitDate)
# 
# h1 <- ggplot(PH, aes(ExitMonth, )) + geom_histogram(bins=12) + ggtitle('Permanent Housing')
# h2 <- ggplot(ES, aes(ExitMonth)) + geom_histogram(bins=12)+ ggtitle('Emergency Shelter')
# h3 <- ggplot(PSH, aes(ExitMonth)) + geom_histogram(bins=12)+ ggtitle('Permanent Supportive Housing')
# h4 <- ggplot(RRH, aes(ExitMonth)) + geom_histogram(bins=12)+ ggtitle('Rapid Re-Housing')
# h5 <- ggplot(SH, aes(ExitMonth)) + geom_histogram(bins=12)+ ggtitle('Safe Haven')
# h6 <- ggplot(SO, aes(ExitMonth)) + geom_histogram(bins=12)+ ggtitle('Street Outreach')
# h7 <- ggplot(TH, aes(ExitMonth)) + geom_histogram(bins=12)+ ggtitle('Transitional Housing')
# 
# 
# plotsList <- list(h1, h2, h3, h4,h5,h6,h7)
# 
# grid.arrange(grobs=plotsList, ncol=2)
# 
# 
# #indivudals v families
# 
# # 15411 individuals
# count(unique(services %>% select(FamilyID, ClientID)))
# 
# #14744 Families
# count(unique(services %>% select(FamilyID)))
# 
# indWFamily <- 
#   services %>%
#   select(FamilyID, ClientID)%>%
#   unique() %>%
#   count(FamilyID, sort=T) %>%
#   filter(n>=2)
# 
# 
# # number of services consumed
# servpPerson <-
# services %>%
#   count(ClientID)
# 
# ggplot(servpPerson, aes(n)) +
#   geom_histogram(bins=100)
# 
# services%>%
#   count(Service, sort=T)%>%
#   kable() %>%
#   kable_styling()


# Date range per individual
# Count of continuous stays per individual 

