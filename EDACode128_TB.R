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
library(janitor)

setwd("C:/Users/ctb80/Box/Practicum_Dallas")

# Read demographic data
# Stick with this for now but eventually migrate it to the lapply method used below
composite_file <- loadWorkbook('UPenn.xlsx')
fileNames <- sheets(composite_file)
for(i in 1:length(fileNames))
{
  assign(fileNames[i],readWorkbook(composite_file,sheet = i, detectDates = T))
}

# Rename demographic data files
TH_Demo <- TH %>% select(-ClientID)
SO_Demo <- SO
SH_Demo <- SH 
PSH_Demo <- PSH
RRH_Demo <- RRH
PH_Demo <- PH
ES_Demo <- ES

# Read Services Data
sheets <- excel_sheets('UPenn Services 100114 - 123120.xlsx')
lst <- lapply(sheets, function(i)read_excel('UPenn Services 100114 - 123120.xlsx', sheet = i))
names(lst) <- sheets ##Add names
list2env(lst ,.GlobalEnv)

TH_Serv <- TH
SO_Serv <- SO
SH_Serv <- SH 
PSH_Serv <- PSH
RRH_Serv <- RRH
OPH_Serv <- OPH
ES_Serv <- ES

# TO DO: Turn these into apply functions
PSH_Serv$`Service Date`<- excel_numeric_to_date(PSH_Serv$`Service Date`)
SO_Serv$`Service Date`<- excel_numeric_to_date(SO_Serv$`Service Date`)
ES_Serv$`Service Date`<- excel_numeric_to_date(ES_Serv$`Service Date`)
TH_Serv$`Service Date`<- excel_numeric_to_date(TH_Serv$`Service Date`)
SH_Serv$`Service Date`<- excel_numeric_to_date(SH_Serv$`Service Date`)
OPH_Serv$`Service Date`<- excel_numeric_to_date(OPH_Serv$`Service Date`)
RRH_Serv$`Service Date`<- excel_numeric_to_date(RRH_Serv$`Service Date`)

TH_Serv$ProgramType = 'TH'
PSH_Serv$ProgramType = 'PSH'
OPH_Serv$ProgramType = 'OPH'
RRH_Serv$ProgramType = 'RRH'
SO_Serv$ProgramType = 'SO'
ES_Serv$ProgramType = 'ES'
SH_Serv$ProgramType = 'SH'


# Alluvial plot # 1
# Just start with Transitional Housing into OPH, PH, RRH
# Join 3 tables to TH, group by TH, housed type, count unique ppl


aluv_test <- TH_Serv %>% select(`Client ID`, ProgramType) %>%
  rename(start=ProgramType)

aluv_test <- left_join(aluv_test,
                       rbind(RRH_Serv %>% select(`Client ID`, ProgramType),
                             PSH_Serv %>% select(`Client ID`, ProgramType),
                             OPH_Serv %>% select(`Client ID`, ProgramType)),
                       by='Client ID')

aluv_comb <- 
  unique(aluv_test) %>%
  count(start, ProgramType)

# The most basic alluvial. Just shows that, from TH, ~80% don't get housing services, ~5% go to PSH and ~15% got to RRH
# Above percentages are eyeball estimates

ggplot(as.data.frame(aluv_comb),
       aes(y = n, axis1 = start, axis2 = ProgramType)) +
  geom_alluvium(aes(fill=ProgramType), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("start", "prog_type"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") #+


## Alluvial plot # 2
## This plot tracks unique individuals by homelessness service, race, and housing service

race_dem <- rbind(TH_Demo, SO_Demo, SH_Demo, PSH_Demo, RRH_Demo, PH_Demo, ES_Demo) %>%
  select(clientid, Races) %>%
  unique()

homeless <- rbind(SO_Serv, ES_Serv, TH_Serv, SH_Serv) %>%
  select(`Client ID`, ProgramType) %>%
  unique() %>%
  rename(homeless_serv=ProgramType) %>%
  left_join(., race_dem, by=c("Client ID" = "clientid"))

housed <- rbind(PSH_Serv, OPH_Serv, RRH_Serv) %>%
  select(`Client ID`, ProgramType) %>%
  unique() %>%
  rename(housed_serv=ProgramType)

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

race_dem <- rbind(TH_Demo, SO_Demo, SH_Demo, PSH_Demo, RRH_Demo, PH_Demo, ES_Demo) %>%
  select(clientid, Races, HUDEthnicity) %>%
  unique()

homeless <- rbind(SO_Serv, ES_Serv, TH_Serv, SH_Serv) %>%
  select(`Client ID`, ProgramType) %>%
  unique() %>%
  rename(homeless_serv=ProgramType) %>%
  left_join(., race_dem, by=c("Client ID" = "clientid"))

housed <- rbind(PSH_Serv, OPH_Serv, RRH_Serv) %>%
  select(`Client ID`, ProgramType) %>%
  unique() %>%
  rename(housed_serv=ProgramType)

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

all_dem <- rbind(TH_Demo, SO_Demo, SH_Demo, PSH_Demo, RRH_Demo, PH_Demo, ES_Demo) %>%
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

# find all people that were housed
# join the earliest homelessness date that happened after


# Ordered list of all housing services dates client in chronological order
housed <- rbind(PSH_Serv, RRH_Serv, OPH_Serv) %>% select('Client ID', 'Service Date', Service, ProgramType)
housed$`Service Date` <- lubridate::ymd(housed$`Service Date`)
housed <- housed[order(housed$`Client ID`, housed$'Service Date'),] # Order by client ID, then service date
housed <- housed %>% 
  rename(ServiceDate = 'Service Date', ClientID = 'Client ID') %>%
  unique() # Unique client, date service received
housed$ServiceType <- 'Housed' #Housing service

# Ordered list of all homelessness services dates client in chronological order
homeless <- rbind(SO_Serv, ES_Serv, TH_Serv, SH_Serv) %>% 
  select('Client ID', 'Service Date', Service, ProgramType)
homeless$`Service Date` <- lubridate::ymd(homeless$`Service Date`)
homeless <- homeless[order(homeless$`Client ID`, homeless$'Service Date'),]
homeless <- homeless %>% 
  rename(ServiceDate = 'Service Date', ClientID = 'Client ID') %>%
  unique()
homeless$ServiceType <- 'Homelessness'

# Create a filtered list of events when clients have received a homelessness service after a housing service
h_combined <- rbind(housed, homeless)
h_combined <- h_combined[order(h_combined$ClientID, h_combined$ServiceDate),]
# Create a set of lead fields that indicate information about the next service received
h_combined$NextClientID <- lead(h_combined$ClientID)
h_combined$NextServiceDate <- lead(h_combined$ServiceDate)
h_combined$NextService <- lead(h_combined$Service)
h_combined$NextProgramType <- lead(h_combined$ProgramType)
h_combined$NextServiceType <- lead(h_combined$ServiceType)

HousedToHoused <- h_combined %>%
  filter(ClientID == NextClientID) %>%
  filter(ServiceType == 'Housed') %>%
  filter(NextServiceType == 'Housed')
length(unique(HousedToHoused$ClientID)) # 2,985 unique ppl housed then not homeless again
length(unique(HousedToHoused$ClientID)) / length(unique(housed$ClientID)) # 85% don't become homeless again

HousedToHomeless <- h_combined %>%
  filter(ClientID == NextClientID) %>%
  filter(ServiceType == 'Housed') %>%
  filter(NextServiceType == 'Homelessness')
length(unique(HousedToHomeless$ClientID))
length(unique(HousedToHomeless$ClientID)) / length(unique(housed$ClientID))

# Add in demographic features
HousedToHomeless <- left_join(HousedToHomeless, 
                        all_dem %>% select(clientid, Races, VeteranStatus, DisablingCondition), 
                        by = c('ClientID' = 'clientid'))

# Evaluate time from housed to homeless
# Large majority of people receiving homelessness services very soon after housing services
HousedToHomeless$ServiceDateDiff <- difftime(HousedToHomeless$NextServiceDate, HousedToHomeless$ServiceDate, units='days')
ggplot(HousedToHomeless %>% filter(ServiceDateDiff>0), aes(x=ServiceDateDiff)) +
  geom_histogram(fill='light blue')
  
# Make alluvial plot data sets from those aggregates
recidivism_alluvial <- HousedToHomeless %>% filter(ServiceDateDiff>0) %>%
  count(ProgramType, NextProgramType, Races, DisablingCondition) %>%
  filter(n>4)

recidivism_alluvial_2 <- HousedToHomeless %>%
  count(Service, DisablingCondition, NextService) %>%
  filter(n>10) 

# The types of programs people flow into when they recidivate
# Demonstration of faceting by disabling condition
# These represent transitions in time, not distinct individuals
p <- ggplot(as.data.frame(recidivism_alluvial),
       aes(y = n, axis1 = ProgramType, axis2 = NextProgramType)) +
  geom_alluvium(aes(fill=Races), width = 1/12) +
  geom_stratum(width = 1/12, fill = "light grey", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("ProgramType", "NextProgramType"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set3", direction = -1, na.value = '#f7b75e') +
  ggtitle('Recidivism from Housed to Homeless by Program Type, Faceted by Disability')
p +  facet_grid(cols = vars(DisablingCondition))

ggplot(as.data.frame(recidivism_alluvial_2),
       aes(y = n, axis1 = Service, axis2 = NextService)) +
  geom_alluvium(aes(fill=DisablingCondition), width = 1/12) +
  geom_stratum(width = 1/12, fill = "light grey", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Service", "NextService"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set3", direction = -1, na.value = '#f7b75e')

# Next, look for people with "housed" exit destinations and look for them in homeless services after their exit
ServicesPostExit <-left_join(
  all_dem %>% select(clientid, ExitDestination, ExitDate),
  h_combined, 
  by = c('clientid' = 'ClientID')) %>%
  filter(ServiceDate > ExitDate)


ggplot(as.data.frame(ServicesPostExit %>% count(ExitDestination, ProgramType, ServiceType) %>% 
                       filter(n>100) %>% filter(ExitDestination!='No exit interview completed (30)')),
       aes(y = n, axis1 = ExitDestination, axis2 = ProgramType)) +
  geom_alluvium(aes(fill=ServiceType), width = 1/12) +
  geom_stratum(width = 1/12, fill = "light grey", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("ExitDestination", "ProgramType"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set3", direction = -1)


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

