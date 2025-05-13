# Main contributor: Grace Bero
# Other contributors: Amelia Burnell, Zofia Landowska, Lynette Ndibalekera

# Goal: Create clean datasets for modeling 

# Housekeeping Items -----------------------------------------------------------
library(lubridate)
library(tidyverse)
library(dplyr)
library(purrr)

# Few notes about the raw data: 
# - every row is a person visit combination
# - afn = household ID
# - served date = year-month-date 
# - annual_income = household income (does not consider hhsize)
# - federal_poverty_level (takes hhsize into consideration)
# - lnm = first initial of last name
# - snap_household = if hh received snap benefit

# Goal: Create 3 different data sets:
#   1. visit level 
#   2. household level 
#   3. individual level

# Subset zipcodes to the first 5 digits

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
# Create a visit level dataset
visit <- all %>% 
  group_by(afn, served_date) %>% 
  # in summarize, you need to really think about how to characterize a visit
  summarise( #ADD MORE HERE
    n_household = n(), #counts the number of rows within each afn, served_date
    zip = first(zip), #zip code per household during visit
    first_visit = min(served_date), # First visit date
    snap = as.integer(any(snap_household == "Y", na.rm = TRUE)),
    threshhold = as.integer(any(fed_poverty_level <= 160, na.rm = TRUE))
  ) %>% 
  mutate (
    served_year = year(served_date),
    served_month = month(served_date),
    served_day_of_month = mday(served_date),
    round_month = round_date(served_date, "month"),
    round_quarter = round_date(served_date, "quarter"),
  )

# Create a visit count dataset
monthly_count <- visit %>% 
  group_by(round_month) %>% 
  summarise(num_VISITS = n(), #num rows (visits)
            num_PEOPLE_SERVED = sum(n_household),# number of people that month
            num_PEOPLE_SNAP = sum(snap=="1"),
            num_threshhold=sum(threshhold=="1"),
            num_nosnap_threshhold=sum(threshhold=="1"&snap=="0")
            ) %>% mutate(percent_snap=num_PEOPLE_SNAP/num_PEOPLE_SERVED*100,
                         percent_nosnap_threshhold=num_nosnap_threshhold/num_threshhold*100,
                         percent_threshhold=num_threshhold/num_PEOPLE_SERVED*100
                         )

quarter_count <- visit %>% 
  group_by(round_quarter) %>% 
  summarise(num_VISITS = n(), #num rows (visits)
            num_PEOPLE_SERVED = sum(n_household),# number of people that month
            num_PEOPLE_SNAP = sum(snap=="1"),
            num_threshhold=sum(threshhold=="1"),
            num_nosnap_threshhold=sum(threshhold=="1"&snap=="0")
  ) %>% mutate(percent_snap=num_PEOPLE_SNAP/num_PEOPLE_SERVED*100,
               percent_nosnap_threshhold=num_nosnap_threshhold/num_threshhold*100,
               percent_threshhold=num_threshhold/num_PEOPLE_SERVED*100
  )

  
# Create a monthly frequency variable

monthly_household_frequency <- visit %>% 
  group_by(round_month,afn)  %>% 
  summarise(freq=n(),
            num_PEOPLE_SERVED = sum(n_household), 
            num_households = length(unique(afn)),
            
  )%>% 
  mutate(FREQ=ifelse(freq > 1,1,0)) 
# This will help us with a house-level analysis

monthly_total_frequency <- monthly_household_frequency %>%
  group_by(round_month) %>% 
  mutate(FREQ=ifelse(freq > 1,1,0)) %>%  
  #if a unique_afn shows up in the month more than 1, then "1"
  summarise(num_VISITS = n(), #num rows (visits)
            num_PEOPLE_SERVED = sum(num_PEOPLE_SERVED),
            num_households = length(unique(afn)),
            more_than_once=sum(FREQ)
  ) 

# Create day_of_the_week variable

all$day_of_the_week <- weekdays(all$served_date)
head(all$day_of_the_week)

# Group by 'day_of_week' and count the number of visits for each day
week_day_counts <- all %>%
  group_by(day_of_the_week) %>%
  tally()

# Print the total count for each weekday
print(week_day_counts)  


##Saturday and Sunday reflect the least number of visits.
##Monday and Tuesday have the highest number of visits.

## There were 57 dates that failed to parse when we were cleaning dates


# HH LEVEL DATASETS ------------------------------------------------------------

## Create a household level dataset for ALL visits --------------

hh_data <- all %>% 
  group_by(afn) %>% 
  summarize(
    zip = zip,
    
    n_people_in_household = n_distinct(individual_id),
    
    first_visit = min(served_date),
    last_visit = max(served_date), 
    first_visit_2018 = if_else(year(first_visit) == 2018, 1, 0),
    first_visit_2020 = if_else(year(first_visit) == 2020, 1, 0),
    first_visit_2019 = if_else(year(first_visit) == 2019, 1, 0),
    first_visit_2021 = if_else(year(first_visit) == 2021, 1, 0),
    first_visit_2022 = if_else(year(first_visit) == 2022, 1, 0),
    first_visit_2023 = if_else(year(first_visit) == 2023, 1, 0),
    first_visit_2024 = if_else(year(first_visit) == 2024, 1, 0),
    
    last_visit_2023 = if_else(year(last_visit) == 2023, 1, 0),
    
    first_visit_zip = first(zip), # zip code during first visit
    
    # Snap related Variables
    # SNAP Benefits in general
    snap = as.integer(any(snap_household == "Y", na.rm = TRUE)),
    
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
    snap_first_2018 = as.integer(first_visit_2018 == 1 & snap_first_visit == 1),
    snap_first_2019 = as.integer(first_visit_2019 == 1 & snap_first_visit == 1),
    snap_first_2020 = as.integer(first_visit_2020 == 1 & snap_first_visit == 1),
    snap_first_2021 = as.integer(first_visit_2021 == 1 & snap_first_visit == 1),
    snap_first_2022 = as.integer(first_visit_2022 == 1 & snap_first_visit == 1),
    snap_first_2023 = as.integer(first_visit_2023 == 1 & snap_first_visit == 1),
    snap_first_2024 = as.integer(first_visit_2024 == 1 & snap_first_visit == 1),
    
    snap_last_visit = as.integer(first(snap_household[served_date == last_visit]) == "Y"),
    snap_last_2023 = as.integer(last_visit_2023 == 1 & snap_last_visit == 1),
    
    # If they came off of SNAP at any point during a given year
    # snap_change_2018 = as.integer(year(served_date)==2018 & length(unique(snap_household)>1)), na.rm = TRUE,
    # snap_change_2019 = as.integer(year(served_date)==2019 & length(unique(snap_household)>1)), na.rm = TRUE,
    # snap_change_2020 = as.integer(year(served_date)==2020 & length(unique(snap_household)>1)), na.rm = TRUE,
    # snap_change_2021 = as.integer(year(served_date)==2021 & length(unique(snap_household)>1)), na.rm = TRUE,
    # snap_change_2022 = as.integer(year(served_date)==2022 & length(unique(snap_household)>1)), na.rm = TRUE,
    # snap_change_2023 = as.integer(year(served_date)==2023 & length(unique(snap_household)>1)), na.rm = TRUE,
    # snap_change_2024 = as.integer(year(served_date)==2024 & length(unique(snap_household)>1)), na.rm = TRUE,
    snap_change_2018 = as.integer(n_distinct(snap_household[year(served_date) == 2018]) > 1),
    snap_change_2019 = as.integer(n_distinct(snap_household[year(served_date) == 2019]) > 1),
    snap_change_2020 = as.integer(n_distinct(snap_household[year(served_date) == 2020]) > 1),
    snap_change_2021 = as.integer(n_distinct(snap_household[year(served_date) == 2021]) > 1),
    snap_change_2022 = as.integer(n_distinct(snap_household[year(served_date) == 2022]) > 1),
    snap_change_2023 = as.integer(n_distinct(snap_household[year(served_date) == 2023]) > 1),
    snap_change_2024 = as.integer(n_distinct(snap_household[year(served_date) == 2024]) > 1),
    
    # Proportion of time on SNAP
    snap_proportion = mean(snap_household == "Y", na.rm = TRUE),
    # Proportion of time on SNAP/Year
    snap_proportion_2018 = ifelse(any(year(served_date) == 2018), 
                                  mean(snap_household[year(served_date) == 2018] == "Y", na.rm = TRUE), 0),
    snap_proportion_2019 = ifelse(any(year(served_date) == 2019), 
                                  mean(snap_household[year(served_date) == 2019] == "Y", na.rm = TRUE), 0),
    snap_proportion_2020 = ifelse(any(year(served_date) == 2020), 
                                  mean(snap_household[year(served_date) == 2020] == "Y", na.rm = TRUE), 0),
    snap_proportion_2021 = ifelse(any(year(served_date) == 2021), 
                                  mean(snap_household[year(served_date) == 2021] == "Y", na.rm = TRUE), 0),
    snap_proportion_2022 = ifelse(any(year(served_date) == 2022), 
                                  mean(snap_household[year(served_date) == 2022] == "Y", na.rm = TRUE), 0),
    snap_proportion_2023 = ifelse(any(year(served_date) == 2023), 
                                  mean(snap_household[year(served_date) == 2023] == "Y", na.rm = TRUE), 0),
    snap_proportion_2024 = ifelse(any(year(served_date) == 2024), 
                                  mean(snap_household[year(served_date) == 2024] == "Y", na.rm = TRUE), 0),
    
    # First recorded household income
    income_first = first(na.omit(annual_income)),   
    
    # Most recent household income  
    income_last = last(na.omit(annual_income)), 
    
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
    
    # Household income at first visit | Commented out because of redundancy | Same as income_first | From Grace 
    # income_first_visit = first(annual_income[served_date == first_visit]),
    
    # Average household income across visits  
    income_avg = mean(annual_income, na.rm = TRUE),  
    
    # Median household income across visits  
    # income_median = median(annual_income, na.rm = TRUE),  
    
    # Maximum and minimum recorded household income  
    income_max = max(annual_income, na.rm = TRUE),  
    income_min = min(annual_income, na.rm = TRUE),
    
    # fed_poverty_level related variables
    # First recorded fed poverty level
    fed_poverty_level_first = first(na.omit(fed_poverty_level)),  
    
    # Most recent fed_poverty_level  
    recent_fed_poverty_level = last(na.omit(fed_poverty_level)),
    
    #fed_poverty_level by year  
    fed_poverty_level_2018 = first(fed_poverty_level[year(served_date) == 2018]),  
    fed_poverty_level_2019 = first(fed_poverty_level[year(served_date) == 2019]),  
    fed_poverty_level_2020 = first(fed_poverty_level[year(served_date) == 2020]),
    fed_poverty_level_2021 = first(fed_poverty_level[year(served_date) == 2021]),  
    fed_poverty_level_2022 = first(fed_poverty_level[year(served_date) == 2022]), 
    fed_poverty_level_2023 = first(fed_poverty_level[year(served_date) == 2023]),  
    fed_poverty_level_2024 = first(fed_poverty_level[year(served_date) == 2024]), 
    
    #med_fed_poverty_level by year  
    med_fed_poverty_level_2018 = median(fed_poverty_level[year(served_date) == 2018]),  
    med_fed_poverty_level_2019 = median(fed_poverty_level[year(served_date) == 2019]),  
    med_fed_poverty_level_2020 = median(fed_poverty_level[year(served_date) == 2020]),
    med_fed_poverty_level_2021 = median(fed_poverty_level[year(served_date) == 2021]),  
    med_fed_poverty_level_2022 = median(fed_poverty_level[year(served_date) == 2022]), 
    med_fed_poverty_level_2023 = median(fed_poverty_level[year(served_date) == 2023]),  
    med_fed_poverty_level_2024 = median(fed_poverty_level[year(served_date) == 2024]), 
  
    # Household income at first visit  
    fed_poverty_level_first_visit = first(fed_poverty_level[served_date == first_visit]),
    
    # Average household income across visits  
    fed_poverty_level_avg = mean(fed_poverty_level, na.rm = TRUE),  
    
    # Median household income across visits  
    fed_poverty_level_median = median(annual_income, na.rm = TRUE),  
    
    # Maximum and minimum recorded household income  
    fed_poverty_level_max = max(fed_poverty_level, na.rm = TRUE),  
    fed_poverty_level_min = min(fed_poverty_level, na.rm = TRUE),
  
    #housing information - homeless
    last_homeless_state=first(homeless[served_date == last_visit]), na.rm = TRUE,
    first_homeless_state=first(homeless[served_date == first_visit]), na.rm = TRUE,
    # one_change_homeless_state=ifelse(length(unique(homeless))==1,1,0), 
    one_change_homeless_state = as.integer(n_distinct(homeless) == 1),
    # more_than_one_change_homeless_state=ifelse(length(unique(homeless))>1,1,0),
    more_than_one_change_homeless_state = as.integer(n_distinct(homeless) > 1),
    
    #housing information - housing_type
    last_housing_type=first(housing_type[served_date == last_visit]), na.rm = TRUE,
    first_housing_type=first(housing_type[served_date == first_visit]), na.rm = TRUE,
    # one_change_housing_type=ifelse(length(unique(housing_type))==1,1,0), 
    one_change_housing_type = as.integer(n_distinct(housing_type) == 1),
    # more_than_one_change_housing_type=ifelse(length(unique(housing_type))>1,1,0),
    more_than_one_change_housing_type = as.integer(n_distinct(housing_type) > 1),
    
    #housing information - whether they own a house
    own_or_buying=ifelse(first(housing[served_date == last_visit])=="Own/Buying",1,0),
    
    #location information - changes
    # one_change_location=ifelse(length(unique(location))==1,1,0), 
    # more_than_one_change_location=ifelse(length(unique(location))>1,1,0),
    one_change_location = as.integer(n_distinct(location) == 1), 
    more_than_one_change_location = as.integer(n_distinct(location) > 1),
    
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

### Cleaning Removal of NA values
# There are many NA values in these columns. This could be due to there being
# no visits during those years - replacing with median of the value
hh_data <- hh_data %>%
  mutate(across(starts_with("income_"), ~ coalesce(., household_income_median)),
         across(starts_with("fed_poverty_level_20"), ~ coalesce(., fed_poverty_level_median)))

# Create a hh level dataset for all people who's first visit was 2023 ----------
hh_first_visit_2023 <- hh_data %>%
  filter(year(first_visit) == 2023) %>%
  select(zip, afn, n_people_in_household, first_visit, last_visit,
         first_visit_zip, snap, snap_first_visit, snap_last_visit,
         snap_first_2023,snap_last_2023,
         snap_change_2023, snap_proportion_2023,
         income_first, income_last, income_2023, income_avg, 
         income_max, income_min, household_income_median,
         fed_poverty_level_first, fed_poverty_level_2023,
         fed_poverty_level_first_visit, fed_poverty_level_avg, 
         fed_poverty_level_max, fed_poverty_level_min, 
         med_fed_poverty_level_2023,
         first_homeless_state, last_homeless_state, 
         first_housing_type, last_housing_type, own_or_buying,
         one_change_location, more_than_one_change_location,
         elderly, child, working_age, college_education, 
         highest_education, kids, single_parent)

# Create a hh level dataset for all people who visited in 2023 -----------------
hh_23 <- hh_data %>%
  filter(year(last_visit) == 2023) %>%
  select(afn, zip, n_people_in_household, first_visit, last_visit, first_visit_2023,
         first_visit_zip, snap, snap_first_visit, snap_first_2023,
         snap_change_2023, snap_proportion_2023,snap_last_2023,
         income_first, income_last, income_2023, income_avg, 
         income_max, income_min, household_income_median,
         fed_poverty_level_first, fed_poverty_level_2023,
         fed_poverty_level_first_visit, fed_poverty_level_avg, 
         fed_poverty_level_max, fed_poverty_level_min, 
         med_fed_poverty_level_2023,
         first_homeless_state, last_homeless_state, 
         first_housing_type, last_housing_type, own_or_buying,
         one_change_location, more_than_one_change_location,
         elderly, child, working_age, college_education, 
         highest_education, kids, single_parent, first_visit_2023)

# Create a hh level dataset for all people whose first visit was in 2022 -------
hh_first_visit_2022 <- hh_data %>%
  filter(year(first_visit) == 2022) %>%
  select(afn, n_people_in_household, first_visit, last_visit,
         first_visit_zip, snap, snap_first_visit, snap_first_2022,
         snap_change_2022, snap_proportion_2022,
         income_first, income_last, income_2022, income_avg, 
         income_max, income_min, household_income_median,
         fed_poverty_level_first, fed_poverty_level_2022,
         fed_poverty_level_first_visit, fed_poverty_level_avg, 
         fed_poverty_level_max, fed_poverty_level_min, 
         med_fed_poverty_level_2022,
         first_homeless_state, last_homeless_state, 
         first_housing_type, last_housing_type, own_or_buying,
         one_change_location, more_than_one_change_location,
         elderly, child, working_age, college_education, 
         highest_education, kids, single_parent)

# Create a hh level dataset for all people who visited in 2022 -----------------
hh_22 <- hh_data %>%
  filter(year(last_visit) == 2022) %>%
  select(afn, n_people_in_household, first_visit, last_visit,
         first_visit_zip, snap, snap_first_visit, snap_first_2022,
         snap_change_2022, snap_proportion_2022,
         income_first, income_last, income_2022, income_avg, 
         income_max, income_min, household_income_median,
         fed_poverty_level_first, fed_poverty_level_2022,
         fed_poverty_level_first_visit, fed_poverty_level_avg, 
         fed_poverty_level_max, fed_poverty_level_min, 
         med_fed_poverty_level_2022,
         first_homeless_state, last_homeless_state, 
         first_housing_type, last_housing_type, own_or_buying,
         one_change_location, more_than_one_change_location,
         elderly, child, working_age, college_education, 
         highest_education, kids, single_parent, first_visit_2022)

# Create a hh level dataset for all people whose first visit was in 2021 -------
hh_first_visit_2021 <- hh_data %>%
  filter(year(first_visit) == 2021) %>%
  select(afn, n_people_in_household, first_visit, last_visit,
         first_visit_zip, snap, snap_first_visit, snap_first_2021,
         snap_change_2021, snap_proportion_2021,
         income_first, income_last, income_2021, income_avg, 
         income_max, income_min, household_income_median,
         fed_poverty_level_first, fed_poverty_level_2021,
         fed_poverty_level_first_visit, fed_poverty_level_avg, 
         fed_poverty_level_max, fed_poverty_level_min, 
         med_fed_poverty_level_2021,
         first_homeless_state, last_homeless_state, 
         first_housing_type, last_housing_type, own_or_buying,
         one_change_location, more_than_one_change_location,
         elderly, child, working_age, college_education, 
         highest_education, kids, single_parent)

# Create a hh level dataset for all people who visited in 2021 -----------------
hh_21 <- hh_data %>%
  filter(year(last_visit) == 2021) %>%
  select(afn, n_people_in_household, first_visit, last_visit,
         first_visit_zip, snap, snap_first_visit, snap_first_2021,
         snap_change_2021, snap_proportion_2021,
         income_first, income_last, income_2021, income_avg, 
         income_max, income_min, household_income_median,
         fed_poverty_level_first, fed_poverty_level_2021,
         fed_poverty_level_first_visit, fed_poverty_level_avg, 
         fed_poverty_level_max, fed_poverty_level_min, 
         med_fed_poverty_level_2021,
         first_homeless_state, last_homeless_state, 
         first_housing_type, last_housing_type, own_or_buying,
         one_change_location, more_than_one_change_location,
         elderly, child, working_age, college_education, 
         highest_education, kids, single_parent, first_visit_2021)

# Save new datasets
write.csv(hh_data, "Data/hh_data.csv", row.names = FALSE)
write.csv(hh_first_visit_2023, "Data/hh_first23.csv", row.names = FALSE)
write.csv(hh_23, "Data/hh_data23.csv", row.names = FALSE)
write.csv(hh_first_visit_2022, "Data/hh_first22.csv", row.names = FALSE)
write.csv(hh_22, "Data/hh_data22.csv", row.names = FALSE)
write.csv(hh_first_visit_2021, "Data/hh_first21.csv", row.names = FALSE)
write.csv(hh_21, "Data/hh_data21.csv", row.names = FALSE)
write.csv(monthly_count, "Data/monthly_count.csv", row.names = FALSE)
write.csv(quarter_count, "Data/quarter_count.csv", row.names = FALSE)

# Verify that it only found 2023 first visits
#hh_data %>%
#  count(first_visit_2023, name = "count")

#yearly_counts <- hh_data_2023 %>%
#  mutate(year = year(first_visit)) %>%
#  count(year, name = "count")

#print(yearly_counts)
# 2023 yearly_count matches the count of 1 for first_visit_2023

### PUlling zipcodes out
zip_codes <- all %>%
  distinct(zip)
write.csv(zip_codes, "Data/zip_codes.csv")

