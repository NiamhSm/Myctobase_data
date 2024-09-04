#creation date: 30/08/2024
#last modification: 30/08/2024
#author: Niamh Smith
#
# content: analyse all southern ocean data in myctobase to get
# depth range of myctophid species for day and night


# packages ----------------------------------------------------------------
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(suncalc)
library(lubridate)

# import data -------------------------------------------------------------
event <- read_csv("D:/lanternfish/5_Data_analysis/2_Myctobase/event.csv")
groupOccurrence <- read_csv("D:/lanternfish/5_Data_analysis/2_Myctobase/groupOccurrence.csv")
individualOccurrence <- read_csv("D:/lanternfish/5_Data_analysis/2_Myctobase/individualOccurrence.csv")


# cleaning data -----------------------------------------------------------
myctobase_depth <- full_join(event, groupOccurrence) %>% select(eventID,start_eventTime, diel, 
                   minimumDepthInMeters, maximumDepthInMeters, medianDepthInMeters,
                   family, scientificName, individualCount, zone) %>% 
                   filter(family == 'Myctophidae') %>% 
                #   mutate_at(vars(maximumDepthInMeters, minimumDepthInMeters), ~replace_na(., 0)) %>% 
                   mutate(avgDepth = (maximumDepthInMeters+minimumDepthInMeters)/2, 
                          date_time = dmy_hm(start_eventTime), date = as.Date(date_time))

#verification
setdiff(event$eventID, groupOccurrence$eventID)
setdiff(groupOccurrence$eventID, event$eventID)
setdiff(groupOccurrence$eventID, individualOccurrence$eventID)
setdiff(individualOccurrence$eventID, groupOccurrence$eventID) #no dif means 
#all individuals recorded in groups

#add the lunar phase
# Calculate lunar phase for each date
get_lunar_phase <- function(date) {
  phases <- suncalc::getMoonIllumination(date)
  return(phases$phase)
}

myctobase_depth <- myctobase_depth %>%
  mutate(
    lunar_phase = sapply(date, get_lunar_phase)
  )


# plots -------------------------------------------------------------------
expanded_myct_depth <- myctobase_depth %>%
  uncount(weight = individualCount) %>% filter(diel == 'day' | diel == 'night')

ggplot(expanded_myct_depth)+
  facet_wrap(~scientificName)+
  geom_violin(aes(x = diel, y = -minimumDepthInMeters))+
  theme_bw()

ggplot(expanded_myct_depth)+
  facet_wrap(~scientificName)+
  geom_violin(aes(x = diel, y = -avgDepth))+
  theme_bw()

ggplot(expanded_myct_depth)+
  facet_wrap(~scientificName)+
  geom_boxplot(aes(x = diel, y = -avgDepth))+
  theme_bw()

# Loop through each zone and create a separate plot
# Get unique zones
zones <- unique(expanded_myct_depth$zone)
for (zone in zones) {
  # Filter data for the current zone
  subset_data <- filter(expanded_myct_depth, zone == !!zone)
  
  # Open a new graphics window
  windows()
  
  # Create and save the plot
  print (
    ggplot(subset_data)+
    facet_wrap(~scientificName)+
    geom_violin(aes(x = diel, y = -avgDepth))+
    labs(title = zone)+
    theme_bw()
  )
  

}

#plot of a migrating species according to moonphase
e_antarctica_night <- myctobase_depth %>%
  uncount(weight = individualCount) %>% 
  filter(diel == 'night', scientificName == "Electrona antarctica")

ggplot(e_antarctica_night)+
  geom_point(aes(x = lunar_phase, y = -avgDepth))+
  theme_bw()
#hard to get a pattern apart from the day/night one

myct_dapth_dn <- filter(myctobase_depth,diel == 'night' | diel == 'day')
ggplot(myct_dapth_dn, 
       aes(x = month(myct_dapth_dn$date, label = TRUE, abbr = TRUE), 
           y = -avgDepth))+
  facet_wrap(~scientificName, ncol = 5)+
  geom_boxplot(aes(fill = diel))+
  theme_bw()
