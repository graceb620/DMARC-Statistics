
# Housekeeping Items -----------------------------------------------------------
rm(list=ls())

library(lubridate)
library(tidyverse)

# Create the main dataset from raw csv -----------------------------------------
all <- read.csv("Data/drake_export_v8_2024-02-13_100754_rev2_nolatlong.csv")

# Cleaning of dates ------------------------------------------------------------
all <- all %>% 
  mutate(
    served_date=ymd(served_date),
    dob=ymd(dob)
  )
# 57 failed to parse - will have to deal with these on a case by case basis

# Create datasets --------------------------------------------------------------
# Create visit level dataset
visit <- all %>% 
  group_by(afn, served_date) %>% 
  # in summarize, you need to really think about how to characterize a visit
  summarise( #ADD MORE HERE
    n_household = n(), #counts the number of rows within each afn, served_date
    zip = first(zip) #zip code per household during visit
  ) %>% 
  mutate (
    served_year = year(served_date),
    served_month = month(served_date),
    served_day_of_month = mday(served_date),
    round_month = round_date(served_date, "month")
  )

# create a visit count dataset
monthly_count <- visit %>% 
  group_by(round_month) %>% 
  summarise(num_VISITS = n(), #num rows (visits)
            num_PEOPLE_SERVED = sum(n_household) # number of people that month
  ) 

# AMELIA WORKED FROM HERE ----------------------
# creating how often a month per household variable

monthly_household_frequency <- visit %>% 
  group_by(round_month,afn)  %>% 
  summarise(freq=n(),
            num_PEOPLE_SERVED = sum(n_household), 
            num_households = length(unique(afn)),
            
  )%>% 
  mutate(FREQ=ifelse(freq > 1,1,0)) 
  #this will help us if we want to analyze on a house-level

monthly_total_frequency <- monthly_household_frequency %>%
  group_by(round_month) %>% 
  mutate(FREQ=ifelse(freq > 1,1,0)) %>%  
  #if a unique_afn shows up in the month more than 1, then "1"
  summarise(num_VISITS = n(), #num rows (visits)
            num_PEOPLE_SERVED = sum(num_PEOPLE_SERVED),
            num_households = length(unique(afn)),
            more_than_once=sum(FREQ)
  ) 


# exploring the zipcode variable 
# How often did each zipcode visit? how many unique households

zipcode_counts <- visit %>%
  group_by(substr(zip,1,5)) %>%
  summarise(zipcode_visits = n(),
            unique_households=length(unique(afn)))
colnames(zipcode_counts)<-c("zip","zipcode_visits","unique_households")

zipcode_counts <- zipcode_counts[order(-zipcode_counts$zipcode_visits),]

summary(zipcode_counts)

sum(zipcode_counts$unique_households < 10) 
#145 zip codes have only had 10 families, let's delete those (for privacy)

zipcode_counts <- filter(zipcode_counts, unique_households > 10)

summary(zipcode_counts) #there are 104 zip codes, including one " "
#there are some zipcodes that are not real, these are unhoused individuals

hist(zipcode_counts$unique_households) #very right skewed, long right tail
#most zipcodes are in the 0-1000 range
#this is counts for ALL the months, 
#but I can also do this for each individual month instead

