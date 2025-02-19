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
    served_day_of_month = may(served_day),
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