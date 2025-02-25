# Every row is a person visit combination
# afn = household ID
# served date = year-month-date 
# annual_income = household income / doesn't make sense because it doesnt consider hhsize
# federal_poverty_level / better to use because it considers hhsize 
# lnm = first initial of last name
# snap_household = if hh received snap
# create 3 different data sets:
#   visit level = 1 row per visit 
#   hhlevel = people and visits 
#   individual level = individuals
# Subset zipcodes to the first 5 digits

# Housekeeping Items -----------------------------------------------------------
rm(list=ls())

library(lubridate)
library(tidyverse)
library(dplyr)

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
    zip = first(zip), #zip code per household during visit
    first_visit = min(served_date) # First visit date
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
  
#creating monthly frequency variable

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
#create day_of_the_week_variable

all$day_of_the_week <- weekdays(all$served_date)
head(all$day_of_the_week)

# Group by 'day_of_week' and count the number of visits for each day
week_day_counts <- all %>%
  group_by(day_of_the_week) %>%
  tally()

# Print the total count for each weekday
print(week_day_counts)  

##Saturday and Sunday reflect the least number of visits
##Monday and Tuesday have the highest number of visits.

##But there's 57 dates that failed to parse when we were cleaning dates

# HH LEVEL DATASETS INFO ----------
# WARNING:
# If you use group_by(afn) and try and get the "count" of x variable in a household
# You are ONLY getting the count of the people that have gone to pick up the food
# EVEN if you use unique(), it will only count the people that have gone to pick up the food
# sometimes, more than one person of the family goes to pick up the food
# e.g. if only the parent was ever getting the food, you will only get a count of that parent
# e.g. if sometimes one parent in a family of 4 went, and another time the other parent went,
# it will seem like there are only 2 people in that family. 

# Additionally, grouping only by afn will combine the count of all the times 
# that person went to the pantry. 
# e.g. if you simply use sum(income), 
# it will calculate income of that person TIMES how often they visited 

# Because of this, I recommend ONLY taking the column that are the same no matter what
# OR, you need to specifically say that you're making counts about the people that 
# visited the pantry, not the household as a whole

# the only exception to this is if we focus on single people without children
# then, using unique() and sum() will be ok

## Create hh level dataset for ALL visits --------------

hh_data <- all %>% 
  group_by(afn) %>% 
  summarize(
    #n_household = n(), instead, should be:
    #n_people_visitng = length(unique(individual_id)), 
    #n_people_visitng only checks number of people visiting, not number of people in household.
    #to get actual household, we would need to do the "family_type"
    #but we don't know how many children they have
    snap_household=first(snap_household),
    homeless=first(homeless),
    family_type=first(family_type),
    first_visit = min(served_date),
    last_visit = max(served_date), 
    first_visit_2018 = if_else(year(first_visit) == 2018, 1, 0),
    first_visit_2019 = if_else(year(first_visit) == 2019, 1, 0),
    first_visit_2020 = if_else(year(first_visit) == 2020, 1, 0),
    first_visit_2021 = if_else(year(first_visit) == 2021, 1, 0),
    first_visit_2022 = if_else(year(first_visit) == 2022, 1, 0),
    first_visit_2023 = if_else(year(first_visit) == 2023, 1, 0),
    first_visit_2024 = if_else(year(first_visit) == 2024, 1, 0),
  ) 

## Create hh level dataset for all visits in ONLY 2023 ---------------
hh_data_2023 <- hh_data %>% 
  filter(year(last_visit) == 2023) #excluding all 2024 data
  #this will make it so that it only counts those who came in 2023
  #in 2023, what is the difference between those who came in 
  #I think that makes more sense than comparing the historic data

# Verify that it only found 2023 first visits
  hh_data %>%
  count(first_visit_2023, name = "count")

yearly_counts <- hh_data_2023 %>%
  mutate(year = year(first_visit)) %>%
  count(year, name = "count")

print(yearly_counts)
# 2023 yearly_count matches the count of 1 for first_visit_2023

# Graph the First Visits per household
ggplot(hh_data, aes(x = first_visit)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density of First Visits Over Time Per Household",
       x = "First Visit Date",
       y = "Density") 

##Graph of Family types and their homeless status
hh_data %>%
  count(homeless) %>%
  ggplot(aes(x = "", y = n, fill = homeless)) +
  geom_col() +
  coord_polar(theta = "y") +
  labs(title = "Proportion of Homeless Households")

#Visit frequency and return patterns
#visits per household
ggplot(hh_data, aes(x = "", y = n_people_visiting)) +
  geom_boxplot(fill = "blue") +
  labs(title = "Distribution of Number of Visitors Per Household", y = "Number of People Visiting")



# Clean hh_data ----------------------------------------------------------------

# Convert blank strings to NA
hh_data <- hh_data %>% 
  mutate(across(c(afn, snap_household, homeless, family_type), ~na_if(., "")))


# Convert categorical variables to factors
hh_data <- hh_data %>% 
  mutate(
    snap_household = factor(snap_household, levels = c("N", "Y")),
    homeless = factor(homeless),
    family_type = factor(family_type)
  )

# Check for NA values in first_visit and last_visit
colSums(is.na(hh_data[c("first_visit", "last_visit")]))
# 0 NA's both in "First_visit" and "last_visit"

# Check whether last_visit is always after or equal to first_visit
# This will return TRUE if all valid (non-NA) cases satisfy the condition and FALSE otherwise.
all(hh_data$last_visit >= hh_data$first_visit, na.rm = TRUE)
# Returned TRUE

# Check for missing household IDs
sum(is.na(hh_data$afn))
# 1 missing value 
hh_data[is.na(hh_data$afn), ]
# Looks like an error ?

# Check summary after cleaning
summary(hh_data)

# Explore correlations or cross-tabulations between columns
table(hh_data$family_type, hh_data$homeless)


# Create a small/intro model ---------------------------------------------------









