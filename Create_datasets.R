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

monthly_prep <- visit %>% 
  group_by(round_month,afn)  %>% 
  summarise(freq=n(),
            num_PEOPLE_SERVED = sum(n_household), 
            num_households = length(unique(afn)),
            
  )

monthly_frequency <- monthly_prep %>%
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

## Create hh level dataset ---------------
hh_data <- all %>% 
  group_by(afn) %>% 
  summarize(
    n_household = n(),
    first_visit = min(served_date),
    first_visit_2023 = if_else(year(first_visit) == 2023, 1, 0)
    ) 

# Verify that it only found 2023 first visits
hh_data %>%
  count(first_visit_2023, name = "count")

yearly_counts <- hh_data %>%
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
  

# This is a test














