rm(list = ls())
library(tidyverse)
library(lubridate)
library(haven)

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

#count visits
monthly_counts <- visit %>%
  group_by(round_month) %>%
  summarise(num_VISITS = n(), # number of rows
            num_PEOPLE_sERVED = sum(n_household))

colnames(all)

#deal with the annual income
summary(all$annual_income)  #get the summary of the annual income
#minimum is -42339, we shall have to deal with the negative, and the maximum is 1000000

#check for na values
sum(is.na(all$annual_income)) #no na values
unique(all$annual_income)
max(all$annual_income)  
min(all$annual_income)
#check how many families or people make more than 50k as their annual_income
sum(all$annual_income > 50000, na.rm = TRUE)
