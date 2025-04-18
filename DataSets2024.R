library(lubridate)
library(tidyverse)
library(dplyr)
library(purrr)

newData <- read.csv("Data/DMARC Data 2018-2024 copy.csv")
colnames(newData)

# Cleaning the date related variables
all2 <- newData %>% 
  mutate(
    servedDate = ymd_hms(servedDate), #converts to desired date format
    servedDate = as_date(servedDate), #only grabs date
    dob=ymd(dob),
    age=year(Sys.Date())-year(dob)
  )
colnames(all2)

### --- Create a visit level data set -----------------------------------------
visit2 <- all2 %>% 
  group_by(servedDate, houseHoldIdAfn) %>% 
  summarize(
    hhMembers_visit = n(), 
    first_visit = min(servedDate),
    service = first(service), 
    location = first(location), 
    .groups = "keep"
  ) %>% 
  mutate(
    year = year(servedDate),
    month = month(servedDate),
    day_of_month = mday(servedDate),
    round_month = round_date(servedDate, "month")
  )


### --- Create an Individual Level Dataset -------------------------------------
individuals <- all2 %>% 
  group_by(clientId) %>% 
  summarize(
    afn = first(houseHoldIdAfn),
    dob = first(dob),
    age = first(age),
    gender = first(gender),
    race = first(race),
    education = first(education),
    foodstamps = first(foodstamps),
    dietaryIssue = first(dietaryIssue), 
    veteran = first(veteran),
    category = first(category),
    incomeSource = first(incomeSource)
  )

### --- Create a household level dataset ---------------------------------------
hh_data2 <- all2 %>% 
  group_by(houseHoldIdAfn) %>% 
  summarize(
    
    householdMembers = max(householdMembers), #Number of people in household
    
    first_visit = min(servedDate),
    last_visit = max(servedDate),
    first_visit_2023 = if_else(year(first_visit) == 2023, 1, 0),
    first_visit_2024 = if_else(year(first_visit) == 2024, 1, 0),
    visit_in_2024 = as.integer(any(year(servedDate) == 2024)), #if there was a visit in 2024
    visit_count_2024 = sum(year(servedDate) == 2024),
    
    IncomeSource = first(incomeSource),
    fedPovertyLevel = first(fedPovertyLevel),
    annualIncome = first(annualIncome),
    
    foodstamps = as.integer(any(foodstamps == "Yes")),
    
    primary_visit_location = names(which.max(table(location))), #location most frequently visited
    primary_service = names(which.max(table(service))), #service most frequently used
    primary_visitor_occupation = names(which.max(table(category))), #most frequent occupation
    
    dietary_issue = as.integer(any(!dietaryIssue %in% c("None", "Unknown"))),
    
    oldest_member = max(age),
    youngest_member = min(age),
    
    veteran = as.integer(any(veteran == "Yes"))
  )
  















