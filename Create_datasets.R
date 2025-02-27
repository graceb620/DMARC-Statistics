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
    dob=ymd(dob),
    age=year(Sys.Date())-year(dob)
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
    
    n_people_in_household = length(unique(individual_id)),
    
    first_visit = min(served_date),
    last_visit = max(served_date), 
    first_visit_2018 = if_else(year(first_visit) == 2018, 1, 0),
    first_visit_2020 = if_else(year(first_visit) == 2020, 1, 0),
    first_visit_2019 = if_else(year(first_visit) == 2019, 1, 0),
    first_visit_2021 = if_else(year(first_visit) == 2021, 1, 0),
    first_visit_2022 = if_else(year(first_visit) == 2022, 1, 0),
    first_visit_2023 = if_else(year(first_visit) == 2023, 1, 0),
    first_visit_2024 = if_else(year(first_visit) == 2024, 1, 0),
    
    # Snap related Variables
    # SNAP Benefits in general
    snap = first(snap_household), 
    
    # SNAP Benefits by year
    snap_2018 = as.integer(any(year(served_date) == 2018 & snap_household == "Y")), 
    snap_2019 = as.integer(any(year(served_date) == 2019 & snap_household == "Y")), 
    snap_2020 = as.integer(any(year(served_date) == 2020 & snap_household == "Y")), 
    snap_2021 = as.integer(any(year(served_date) == 2021 & snap_household == "Y")), 
    snap_2022 = as.integer(any(year(served_date) == 2022 & snap_household == "Y")), 
    snap_2023 = as.integer(any(year(served_date) == 2023 & snap_household == "Y")),  
    snap_2024 = as.integer(any(year(served_date) == 2024 & snap_household == "Y")),
    
    # Receiving SNAP Benefits during first visit
    snap_first_visit = as.integer(first(snap_household[served_date == first_visit]) == "Y"),
    
    # Receiving SNAP Benefits during specific year and first visit
    snap_first_2018 = as.integer(snap_2018 == 1 & snap_first_visit == 1),
    snap_first_2019 = as.integer(snap_2019 == 1 & snap_first_visit == 1),
    snap_first_2020 = as.integer(snap_2020 == 1 & snap_first_visit == 1),
    snap_first_2021 = as.integer(snap_2021 == 1 & snap_first_visit == 1),
    snap_first_2022 = as.integer(snap_2022 == 1 & snap_first_visit == 1),
    snap_first_2023 = as.integer(snap_2023 == 1 & snap_first_visit == 1),
    snap_first_2024 = as.integer(snap_2024 == 1 & snap_first_visit == 1),
    
    # Proportion of time on SNAP
    snap_proportion = mean(snap_household == "Y", na.rm = TRUE),
    
    # First recorded household income  
    income_first = first(annual_income),  
    
    # Most recent household income  
    income_latest = last(annual_income), 
    
    # Compute household-specific median income, ignoring negative values and NA
    household_income_median = median(annual_income[annual_income >= 0], na.rm = TRUE),
    
    # Replace NA and negative values with the household median
    #annual_income = ifelse(is.na(annual_income) | annual_income < 0, household_income_median, annual_income)
  #) %>%
  #summarise(
    #n_people_in_household = length(unique(individual_id)),
    
     #Household income by year  
     income_2018 = first(annual_income[year(served_date) == 2018]),  
     income_2019 = first(annual_income[year(served_date) == 2019]),  
     income_2020 = first(annual_income[year(served_date) == 2020]),
     income_2021 = first(annual_income[year(served_date) == 2021]),  
     income_2022 = first(annual_income[year(served_date) == 2022]), 
     income_2023 = first(annual_income[year(served_date) == 2023]),  
     income_2024 = first(annual_income[year(served_date) == 2024]), 
     #This was making some error for me, so I commented it out for now - Amelia
    
    # Household income at first visit  
    income_first_visit = first(annual_income[served_date == first_visit]),
    
    # Average household income across visits  
    income_avg = mean(annual_income, na.rm = TRUE),  
    
    # Median household income across visits  
    income_median = median(annual_income, na.rm = TRUE),  
    
    # Maximum and minimum recorded household income  
    income_max = max(annual_income, na.rm = TRUE),  
    income_min = min(annual_income, na.rm = TRUE),
    
    #housing information - homeless
    last_homeless_state=first(homeless[served_date == last_visit]), na.rm = TRUE,
    first_homeless_state=first(homeless[served_date == first_visit]), na.rm = TRUE,
    one_change_homeless_state=ifelse(length(unique(homeless))==1,1,0), 
    more_than_one_change_homeless_state=ifelse(length(unique(homeless))>1,1,0),
    
    #housing information - housing_type
    last_housing_type=first(housing_type[served_date == last_visit]), na.rm = TRUE,
    first_housing_type=first(housing_type[served_date == first_visit]), na.rm = TRUE,
    one_change_housing_type=ifelse(length(unique(housing_type))==1,1,0), 
    more_than_one_change_housing_type=ifelse(length(unique(housing_type))>1,1,0),
    
    #housing information - whether they own a house
    own_or_buying=ifelse(first(housing[served_date == last_visit])=="Own/Buying",1,0),
    
    #location information - changes
    one_change_location=ifelse(length(unique(location))==1,1,0), 
    more_than_one_change_location=ifelse(unique(location)>1,1,0),
    
    #demographic - age
    elderly=ifelse(any(age>64),1,0),
    child=ifelse(any(age<18),1,0),
    working_age=ifelse(any(age>18&age<64),1,0),
    
    # Determine whether there is a person with a college degree in the household
    college_education = if_else(any(education == 'College 2 or 4 yr Degree' | education == 'College Advanced Degree'), 1, 0),
    
    # Determine the highest level of education in the household
    highest_education = case_when(
      any(education == 'College 2 or 4 yr Degree' | education == 'College Advanced Degree') ~ "Higher Education",
      any(education == 'HS Grad / Some College' | education == 'HS Grad') ~ "High School",
      TRUE ~ "Unknown / Does not apply"
    ),
    
    # Determine whether there are kids in the household
    kids = if_else(any(family_type == "Adults with Children" | family_type == "Male Adult with Children" | family_type == "Female Adult with Children"), 1, 0),
    
    # Determine whether it's a single-parent household
    single_parent = if_else(any(family_type == "Male Adult with Children" | family_type == "Female Adult with Children"), "Yes", "No / Does not apply")
    
  )

summary(hh_data)
  
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
# Is there a correlation between family type and housing situation
table(hh_data$family_type, hh_data$homeless)
table(hh_data$family_type, hh_data$homeless)


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

##create visualizations to analyze hh_level dataset ----------------------------

# Graph the First Visits per household
ggplot(hh_data, aes(x = first_visit)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density of First Visits Over Time Per Household",
       x = "First Visit Date",
       y = "Density") 

##Graph of Family types and their homeless status


hh_data_2023 %>%
  count(homeless) %>%
  ggplot(aes(x = homeless, y = n, fill = homeless)) +
  geom_col() +
  labs(
    title = "Count of Homeless vs. Non-Homeless Households in 2023",
    x = "Homeless Status",
    y = "Count"
  )


#First visit distribution by Year
hh_data_2023 %>%
  mutate(first_visit_year = year(first_visit)) %>%
  count(first_visit_year) %>%
  ggplot(aes(x = as.factor(first_visit_year), y = n, fill = as.factor(first_visit_year))) +
  geom_col() +
  labs(title = "Households' First Visit Year", x = "Year", y = "Count") +
  theme_minimal()
##More visits in 2018 followed by 2019 and 2023, the least in 2024 and 2021.

#Proportion of Households recieving SNAP benefits
hh_data %>%
  count(snap_household) %>%
  ggplot(aes(x = snap_household, y = n, fill = snap_household)) +
  geom_col() +
  labs(
    title = "Proportion of Households Receiving SNAP",
    x = "SNAP Household",
    y = "Count"
  ) +
  theme_minimal()

##Most households dont recieve SNAP benefits.



# Create a small/intro model ---------------------------------------------------





