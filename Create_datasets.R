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
  
#This is a test
#Test 2
#Test 3
# Test 4


















