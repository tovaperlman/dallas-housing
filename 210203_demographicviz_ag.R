options(scipen=10000000)

<<<<<<< Updated upstream
#install.packages('tidyverse')
library(tidyverse)
#install.packages("readxl")
#install.packages("openxlsx")
library(readxl)
library(openxlsx)
#install.packages("networkD3")
library(networkD3)

setwd("C:/Users/gault/OneDrive/UPenn/Fourth Semester/MUSA Practicum/data/original")


demographicsexcel <- "Upenn.xlsx"
demographics <- read.csv("C:/Users/gault/OneDrive/UPenn/Fourth Semester/MUSA Practicum/data/client_dat.csv")
=======
install.packages('tidyverse')
library(tidyverse)
install.packages("readxl")
install.packages("openxlsx")
library(readxl)
library(openxlsx)
install.packages(networkD3)
library(networkD3)

setwd('C:/Users/tovap/Documents/Spring 2020 classes/Practicum')


demographicsexcel <- "C:/Users/tovap/Documents/Spring 2020 classes/Practicum/UPenn.xlsx"

>>>>>>> Stashed changes

#Trying to get all the sheets in one
composite_file <-  loadWorkbook('UPenn Services 100114 - 123120.xlsx')
fileNames <- sheets(composite_file)
for(i in 1:length(fileNames))
{
  assign(fileNames[i],readWorkbook(composite_file,sheet = i, detectDates = T))
}

#Trying to get all the sheets in one
<<<<<<< Updated upstream
composite_file <-  loadWorkbook("UPenn.xlsx")
=======
composite_file <-  loadWorkbook("C:/Users/tovap/Documents/Spring 2020 classes/Practicum/UPenn.xlsx")
>>>>>>> Stashed changes
fileNames <- sheets(composite_file)
for(i in 1:length(fileNames))
{
  assign(fileNames[i],readWorkbook(composite_file,sheet = i, detectDates = T))
}

#taking out duplicate column
THedit <-  TH %>% 
  select(-ClientID)

#Then I want to do a facet wrap of services by demographics groups
Bigdata <- rbind(ES, SH, SO, THedit, PH, PSH, RRH) #could count twice - #deduplicate #only showing up once


Racialgroups <- table(demographics$Races)
racepercentage <- prop.table(Racialgroups)

#Frequency of Race
racepercentagedf <- as.data.frame(racepercentage)
  
ggplot(racepercentagedf, aes(x=Var1, y=Freq))+
  geom_bar(stat='identity' )+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

table(demographics$Gender)
table(demographics$DisablingCondition)

ggplot(Bigdata, aes(x = `Races`)) +
  geom_bar(color="blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Facet Wrap of Race

# re-order levels
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}

<<<<<<< Updated upstream


=======
>>>>>>> Stashed changes
#Race and Gender by proportion
Bigdata%>%
  select(Races, Gender, DisablingCondition) %>%
  filter(Races == "White"|Races ==  "Black or African American"|Races ==  "American Indian or Alaska Native") %>%
  ggplot(aes(x = reorder_size(`Races`))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Races") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  facet_grid(~ Gender) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Race and Gender by count
Bigdata%>%
  select(Races, Gender, DisablingCondition) %>%
  filter(Races == "White"|Races ==  "Black or African American"|Races ==  "American Indian or Alaska Native") %>%
  ggplot(aes(x = reorder_size(`Races`))) +
  geom_bar(stat='count')+
  xlab("Races") +
  #scale_y_continuous(labels = scales::percent, name = "Proportion") +
  facet_grid(~Gender) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Facet wrap of races in each housing program type by proportion
Bigdata%>%
  select(Races, Gender, DisablingCondition, ProjectType) %>%
  filter(Races == "White"|Races ==  "Black or African American"|Races ==  "American Indian or Alaska Native") %>%
  ggplot(aes(x = reorder_size(`Races`))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Races") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  facet_grid(~ ProjectType) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Facet wrap of races in each housing program type by count
Bigdata%>%
  select(Races, Gender, DisablingCondition, ProjectType) %>%
  filter(Races == "White"|Races ==  "Black or African American"|Races ==  "American Indian or Alaska Native") %>%
  ggplot(aes(x = reorder_size(`Races`))) +
  geom_bar(stat='count')+
  xlab("Races") +
  facet_wrap(~ ProjectType) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Facet wrap of Gender in each housing program type by proportion
Bigdata%>%
  select(Races, Gender, DisablingCondition, ProjectType) %>%
  filter(Gender == "Male (1)"|Gender ==  "Female (0)") %>%
  ggplot(aes(x = reorder_size(`Gender`))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Gender") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  facet_grid(~ ProjectType) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Facet wrap of Gender in each housing program type by count
Bigdata%>%
  select(Races, Gender, DisablingCondition, ProjectType) %>%
  filter(Gender == "Male (1)"|Gender ==  "Female (0)") %>%
  ggplot(aes(x = reorder_size(`Gender`))) +
  geom_bar(stat='count')+
  xlab("Gender") +
  facet_grid(~ ProjectType) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Facet wrap of Disabling Condition in each housing program type by proportion
Bigdata%>%
  select(Races, Gender, DisablingCondition, ProjectType) %>%
  filter(Gender == "Male (1)"|Gender ==  "Female (0)") %>%
  ggplot(aes(x = reorder_size(`DisablingCondition`))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("DisablingCondition") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  facet_grid(~ ProjectType) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Facet wrap of Disabling Condition in each housing program type by count
#Why is there no for PSH when a disability is required
Bigdata%>%
  select(Races, Gender, DisablingCondition, ProjectType) %>%
  filter(Gender == "Male (1)"|Gender ==  "Female (0)") %>%
  ggplot(aes(x = reorder_size(`DisablingCondition`))) +
  geom_bar(stat='count')+
  xlab("DisablingCondition") +
  facet_grid(~ ProjectType) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Facet wrap of Prior Residence for gender by proportion
Bigdata%>%
  select(Races, Gender, DisablingCondition, ProjectType, PriorResidence) %>%
  filter(Gender == "Male (1)"|Gender ==  "Female (0)") %>%
  ggplot(aes(x = reorder_size(`PriorResidence`))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("PriorResidence") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  facet_grid(~ Gender) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Facet wrap of Prior Residence for gender by count
Bigdata%>%
  select(Races, Gender, DisablingCondition, ProjectType, PriorResidence) %>%
  filter(Gender == "Male (1)"|Gender ==  "Female (0)") %>%
  ggplot(aes(x = reorder_size(`PriorResidence`))) +
  geom_bar(stat='count')+
  xlab("PriorResidence") +
  facet_grid(~ Gender) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#trimming to top ten prior residences by frequency
Bigdata%>%
  select(ProjectType, PhysicalDisability, DevelopmentalDisability, ChronicHealthCondition, PriorResidence) %>%
  group_by(PriorResidence)%>%
  summarise(Freq=n()) %>%
  #arrange(desc(freq))%>%
  slice(0:10)%>%
  #filter(Gender == "Male (1)"|Gender ==  "Female (0)") %>%
  ggplot(aes(x = reorder_size(`PriorResidence`), y=Freq)) +
  geom_bar(stat='identity')+
  xlab("PriorResidence") +
  #facet_grid(~ ProjectType) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#trimming to top five prior residences for each project type - not working yet
Bigdata%>%
  select(ProjectType,  PriorResidence) %>%
  #group_by(PriorResidence)%>%
  summarise(Freq=n()) %>%
  #arrange(desc(freq))%>%
  slice(0:10)%>%
  #filter(Gender == "Male (1)"|Gender ==  "Female (0)") %>%
  ggplot(aes(., x = reorder_size(`PriorResidence`), y=Freq)) +
  geom_bar(stat='identity')+
  xlab("PriorResidence") +
  facet_grid(~ ProjectType) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(demographics, aes(x = `Races`)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("State or Province") +
  scale_y_continuous(labels = scales::percent, name = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#income by gender - need to work on this
Bigdata%>%
  select(Gender, EntryEarnedAmount, EntrySSIAmount ) %>%
  filter(Gender == "Male (1)"|Gender ==  "Female (0)") %>%
  group_by(Gender) %>%
  mutate(TotalAmount = sum((EntryEarnedAmount + EntrySSIAmount))) %>%
  ggplot(aes(x = reorder_size(`TotalAmount`))) +
  geom_bar(color="blue") +
  facet_grid(~ Gender) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Most Popular Prior Residence - Next step is filtering to top 5- can I do this with numbers or do I need to write out the whole entry?
ggplot(Bigdata, aes(x = reorder_size(`PriorResidence`))) +
  geom_bar(color="blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Most Popular Exit Destination
ggplot(Bigdata, aes(x = reorder_size(`ExitDestination`))) +
  geom_bar(color="blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#gender
ggplot(Bigdata, aes(x = `Gender`)) +
  geom_bar(fill='blue') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#disablingcondition
ggplot(Bigdata, aes(x = `DisablingCondition`)) +
  geom_bar(fill="blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Veteran Status
ggplot(Bigdata, aes(x = `VeteranStatus`)) +
  geom_bar(color="blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Domestic violence victim
ggplot(Bigdata, aes(x = `Gender`)) +
  geom_bar(color="blue") +
  facet_grid(~DomesticViolenceVictim)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(demographics, aes(x = `ExitDestination`)) +
  geom_bar(color="blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(demographics, aes(x = `PriorResidence`)) +
  geom_bar(color="blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(Bigdata, aes(x = `DomesticViolenceVictim`)) +
  geom_bar(color="blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

demographics %>%
  dplyr::select(Races)%>%
  gather(Variable, value, -Races)
  ggplot(aes(demographics, value, fill=Races))+ 
    geom_bar(position = "dodge", stat = "summary", fun = "mean") + 
    facet_wrap(~Variable, scales = "free") +
    scale_fill_manual(values = palette2) +
    labs(x="Races", y="count", 
         title = "Feature associations with the likelihood of churn",
         subtitle = "(Continous outcomes)") +
    plotTheme() + theme(legend.position = "none")


ggplot(demographics, aes(x=as.factor(Races) )) +
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) )

ggplot(demographics, aes(x=as.factor(Gender) )) +
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) )

ggplot(demographics, aes(x=as.factor(Races), fill=as.factor(Races) )) + 
  geom_bar( ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none")

ggplot(demographics, aes(x=as.factor(Races) )) + 
geom_bar(stat = "identity") +
  coord_flip()

ggplot(demographics, aes(x=Races, y=Value )) + 
  geom_bar(stat = "identity") +
  coord_flip()

link
#Alluvial plot

# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  PriorResidence=c("group_A","group_A", "group_B", "group_C", "group_C", "group_E"), 
  ExitDestination=c("group_C","group_D", "group_E", "group_F", "group_G", "group_H"), 
  value=c(2,3, 2, 3, 1, 3)
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   