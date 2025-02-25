rm(list = ls())
library(tidyverse)
library(lubridate)
library(haven)
library(dplyr)

all <- read.csv("C:/Users/lynne/OneDrive/Desktop/casestudy/drake_export_v8_2024-02-13_100754_rev2_nolatlong.csv")

#View(all)
head(all)

dim(all)

#clean dates
all <- all %>%
  mutate(
    served_date = ymd(served_date),
    dob = ymd(dob)
  )

#57 failed to parse, have to deal with these on a case to case basis

#create visit level dataset.

visit <- all %>%
  group_by(afn, served_date) %>%
  summarise(
    n_household = n(), #couts the number of rows within each afn. served_date
    zip = first(zip)
  ) %>%
  mutate(
    served_year = year(served_date),
    served_month = month(served_date),
    served_day_of_month = mday(served_date),
    round_month = round_date(served_date, "month")
    
  )
head(visit)

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
 
 ##But there's 57 dates that failed to parse when we were cleaning dates**
 
 hh_data <- all %>% 
   group_by(afn) %>% 
   summarize(
     #n_household = n(), instead, should be:
     #n_people_visitng = length(unique(individual_id)), 
     #n_people_visitng only checks number of people visiting, not number of people in household.
     #to get actual household, we would need to do the "family_type"
     #but we don't know how many children they have
     n_people_visiting = n_distinct(individual_id),
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
 
 ##############################

 
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


 
 
 
 
 
 
 
 