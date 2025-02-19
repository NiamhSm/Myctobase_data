#creation date: 11/02/2025
#last modification: 18/02/2025
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
library(broom)
library(mgcv)

# import data -------------------------------------------------------------
# Set locale
Sys.setlocale("LC_TIME", "English")
event <- read_csv("E:/lanternfish/5_Data_analysis/2_Myctobase/event.csv")
groupOccurrence <- read_csv("E:/lanternfish/5_Data_analysis/2_Myctobase/groupOccurrence.csv")
individualOccurrence <- read_csv("E:/lanternfish/5_Data_analysis/2_Myctobase/individualOccurrence.csv")


# cleaning data -----------------------------------------------------------
# selecting myctophid data
myctobase_depth <- full_join(event, groupOccurrence) %>% select(eventID,start_eventTime, diel, 
                                                                minimumDepthInMeters, maximumDepthInMeters, medianDepthInMeters,
                                                                family, scientificName, individualCount, zone, netSystem) %>% 
  filter(family == 'Myctophidae') %>% 
  mutate(minimumDepthInMeters = replace_na(minimumDepthInMeters, 0),      #replace all na values in min depth by 0
         avgDepth = (maximumDepthInMeters+minimumDepthInMeters)/2,        #calculate avg depth
         date_time = dmy_hm(start_eventTime), date = as.Date(date_time))  #setting date format


# comparing haul depths during day and night to analyse if the sampling is biased
haul_depth <- myctobase_depth %>% select(eventID,start_eventTime, diel, 
                                         minimumDepthInMeters, maximumDepthInMeters, medianDepthInMeters, zone,
                                         avgDepth, date) %>% unique() %>% mutate(month = month(date, label = TRUE, abbr = TRUE))



# verification of ID
setdiff(event$eventID, groupOccurrence$eventID)
setdiff(groupOccurrence$eventID, event$eventID)
setdiff(groupOccurrence$eventID, individualOccurrence$eventID)
setdiff(individualOccurrence$eventID, groupOccurrence$eventID) #no dif means 
# all individuals recorded in groups

myct_depth_dn <- filter(myctobase_depth,diel == 'night' | diel == 'day') #selection of day/night data


# uncount the individual counts to facilitate analysis
expanded_myct_depth <- myctobase_depth %>%
  uncount(weight = individualCount) %>% filter(diel == 'day' | diel == 'night') %>% 
  mutate(month = month(date, label = TRUE, abbr = TRUE))






# plots -------------------------------------------------------------------

# plot of all species to select the ones that have enough data
ggplot(myct_depth_dn, 
       aes(x = month(myct_depth_dn$date, label = TRUE, abbr = TRUE), 
           y = -avgDepth))+
  facet_wrap(~scientificName, ncol = 5)+
  geom_boxplot(aes(fill = diel))+
  theme_bw()

# Loop through each species and create a separate plot
# Get unique species
species <- c("Electrona antarctica", 
             "Gymnoscopelus braueri", 
             "Krefftichthys anderssoni",
             "Protomyctophum bolini")

# creation of a results dataframe for the t.test 
results <- data.frame(month = character(), scientificName = character(), t_test_result = numeric(), stringsAsFactors = FALSE)

for (scientificName in species) {
  # Filter data for the species
  subset_data <- expanded_myct_depth %>% 
    filter(scientificName == !!scientificName) %>%  # selection of a species
    group_by(month) %>%                           
    mutate(sample_size = n()) %>%                   # count of occurrences per month
    ungroup()
  sum(unique(subset_data$sample_size))
  
  # Create a new column for month labels with sample size
  subset_data$month_label <- paste(month(subset_data$date, label = TRUE, abbr = TRUE),
                                   "\n",
                                   'n =', subset_data$sample_size)
  unique(subset_data$month_label)
  # Ensure all months are included in the factor levels
  # Create a complete list of month labels with counts
  all_months <- sapply(1:12, function(m) {
    n_samples <- sum(month(subset_data$date) == m)  # Count the samples for each month
    paste(month.abb[m], "\n", 'n =', n_samples)
  })
  
  # Set levels for the month labels to ensure correct ordering
  subset_data$month_label_2 <- factor(subset_data$month_label, 
                                      levels = all_months)
  
  
  months <- unique(subset_data$month)
  
  for (month in months) {
    tryCatch({
      valid_months <- subset_data %>%
        filter(month == !!month) %>%
        group_by(diel) %>%
        filter(n() > 1) %>%
        ungroup()
      months <- unique(valid_months$month)
      
      
      # Perform t-test for each month with valid data
      t_test_results <- valid_months %>%
        do({
          if (n_distinct(valid_months$diel) == 2) {
            tidy(wilcox.test(-avgDepth ~ diel, data = valid_months))
          } else {
            NULL
          }
        }) %>%
        ungroup()  # Remove grouping
      
      # Print t-test results
      results <- rbind(results, data.frame(month = month, 
                                           scientificName = scientificName, 
                                           t_test_result = t_test_results$p.value))
      
    },  error = function(e) {
      # Handle errors
      message(paste("Error in month:"), month) 
      
    }) 
    
  }
  
  
  
  # Create and save the plot
  print (
    ggplot(subset_data, 
           aes(x = month_label_2,
               y = -avgDepth))+
      geom_boxplot(aes(fill = diel))+
      labs(title = scientificName, x = '', y = 'Depth', fill = '')+
      theme_bw()
  )
  
  
}





# Comparison of sampling scheme between day and night ---------------------
haul_depth <- filter(haul_depth,diel == 'night' | diel == 'day') %>% group_by(month) %>% 
  mutate(sample_size = n())


# Create a new column for month labels with sample size
haul_depth$month_label <- paste(month(haul_depth$date, label = TRUE, abbr = TRUE),
                                 "\n",
                                 'n =', haul_depth$sample_size)


# Ensure all months are included in the factor levels
# Create a complete list of month labels with counts
all_months <- sapply(1:12, function(m) {
  n_samples <- sum(month(haul_depth$date) == m)  # Count the samples for each month
  paste(month.abb[m], "\n", 'n =', n_samples)
})


# Set levels for the month labels to ensure correct ordering
haul_depth$month_label_2 <- factor(haul_depth$month_label, 
                                    levels = all_months)


ggplot(haul_depth)+
  geom_boxplot(aes(y = -avgDepth, x = month_label_2, fill = diel))+
  labs(title = 'Average depth of net hauls', x = '', y = 'Depth', fill = '')+
  theme_bw()


# creation of a results dataframe for the t.test 
results <- data.frame(month = character(), t_test_result = numeric(), stringsAsFactors = FALSE)

months <- unique(haul_depth$month)


for (month in months) {
  
  tryCatch({
    valid_months <- haul_depth %>%
      filter(month == !!month) %>%
      group_by(diel) %>%
      filter(n() > 1) %>%
      ungroup()
   months <- unique(valid_months$month)
    
   
   
    
   # Perform t-test for each month with valid data
   t_test_results <- valid_months %>%
     do({
       if (n_distinct(valid_months$diel) == 2) {
         tidy(wilcox.test(-avgDepth ~ diel, data = valid_months))
       } else {
         NULL
       }
     }) %>%
      ungroup()  # Remove grouping
    
    # Print t-test results
    results <- rbind(results, data.frame(month = month, 
                                         t_test_result = t_test_results$p.value))
    
  },  error = function(e) {
    # Handle errors (optional)
    message(paste("Error in month:"), month) 
    
  }) 
}


# size-depth distribution -------------------------------------------------
# join all data
myctobase_depth_size <- full_join(myctobase_depth, individualOccurrence) %>% 
  filter(diel == 'night' | diel == 'day')


# plots of size-depth relationship and GAM results
for (scientificName in species) {
  subset_data <- myctobase_depth_size %>% 
    filter(scientificName == !!scientificName, !is.na(standard_length), 
           standard_length != 'SC 3', standard_length != 'SC 2', 
           standard_length != 'SC 1') %>% 
    mutate(standard_length_T = as.numeric(standard_length))# selection of a species

  
  print(
    ggplot(subset_data)+
    geom_point(aes(x = avgDepth, y = standard_length_T, colour = diel))+
    geom_smooth(method = "lm", aes(x = avgDepth, y = standard_length_T, colour = diel), fill = "grey", alpha = 0.3)+
    geom_smooth(method = "lm", aes(x = avgDepth, y = standard_length_T), colour = 'black', fill = "grey", alpha = 0.3)+
    labs(title = scientificName, y = 'Standard length', x = 'Depth')+
    scale_x_continuous(limits = c(0, 1000))+
    theme_bw()
  )
  
  print(scientificName)
  gam_results <- gam(standard_length_T~avgDepth+diel, data = subset_data)
  
  
  print(
    summary(gam_results)
  )
  
}



