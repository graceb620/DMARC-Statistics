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
   served_date = served_date,
   first_visit = min(served_date)
  ) %>% 
  mutate (
    served_year = year(served_date),
    served_month = month(served_date),
    served_day_of_month = mday(served_date),
    round_month = round_date(served_date, "month")
  )

ggplot(visit, aes(x = first_visit)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density of First Visits Over Time",
       x = "First Visit Date",
       y = "Density") 

# OBSERVATIONS: 
# There appear to be a lot of first visits in late 2023.
#   Possibly due to inflation? 
# There was large dip, then peak, then dip from mid 2020 to
# mid 2021
#   COVID?
# Could be interesting to isolate the individual peaks?





