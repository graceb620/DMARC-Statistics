rm(list =ls())
library(tidyverse)
library(lubridate)


#note i use a relative file path here

all <- read.csv("data_raw/drake_export_v8_2024-02-13_100754_rev2_nolatlong.csv")

#View(all)

dim(all)

#little bit of cleaning of dates
all <- all %>%
  mutate(
    served_date = ymd(served_date),
    dob = ymd(dob)
  )

#57 failed to parse - will have to deal with these on a case by case basis (user error, use your best judgement)

#Create a visit level data set

visit <- all %>%
  group_by(afn, served_date) %>%
  #in summarise, you need to really think about how to characterise a visit
  summarise(
    n_household = n(), #counts the number of rows within each afn, served_date 
    zip = first(zip)
  ) %>%
  mutate(
    served_year = year(served_date),
    served_month = month(served_date),
    served_day_of_month = mday(served_date),
    round_month = round_date(served_date, "month")
  )

head(visit)

#num of poeple in a household is not a constant attribute to the family

#now we're in a place where we can count visits 
monthly_counts <- visit %>%
  group_by(round_moth) %>%
  summarise(num_VISITS = n(), #number of rows (visits)
            num_PEOPLE_SERVED = sum(n_household)) # number of people that month 
            

#profile the people and identify trends 


#explore family_type

str(all$family_type) #"Adults with Children" "Adults with Children" "Adults with Children" "Single Person" ...
summary(all$family_type) #character type obvi
sum(is.na(all$family_type)) #0 NA's
unique(all$family_type)
#"Adults with Children"; "Single Person"; "Male Adult with Children"; "Adults Without Children"; "Female Adult with Children"; "Not Selected"; "Other"; ""
table(all$family_type)
max(table(all$family_type)) #564750 Adults with Children
min(table(all$family_type)) #175 Not Selected

table(all$family_type, all$snap_household)  # Compare family types by SNAP eligibility

all %>%
  group_by(family_type) %>%
  summarize(mean_income = mean(annual_income, na.rm = TRUE)) #average income per family type

#na.rm = TRUE tells R to ignore missing values


