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

summary(all2) # Checking to make sure that all of the columns are in the desired
# format and of reasonable value 
# Noticed that the age range was from -7974.49 and 2024 i want to check the entries
# where the age is <0 and >120
test <- all2 %>%
  group_by(clientId) %>%
  filter(age < 0 | age > 120)

# For cleaning age, although some might be obvious as to how they would be changed, 
# the original data had some people >100 years old that were "currently pre-school or younger"
# Because of this, it will be more difficult to clean as we no longer trust the original to be correct
# and we are unsure what assumptions should be made

#decision for age: do not include column. can use education instead

### --- Create a visit level data set -----------------------------------------
visit2 <- all2 %>% 
  group_by(servedDate, houseHoldIdAfn) %>% 
  summarize(
    hhMembers_visit = n(), 
    first_visit = min(servedDate),
    service = first(service), 
    location = first(location),
    snap = as.integer(any(foodstamps == "Yes", na.rm = TRUE)),
    threshold = as.integer(any(fedPovertyLevel <= 160, na.rm = TRUE)),
    .groups = "keep"
  ) %>% 
  mutate(
    year = year(servedDate),
    month = month(servedDate),
    day_of_month = mday(servedDate),
    round_month = round_date(servedDate, "month"),
    round_quarter = round_date(servedDate, "quarter")
  )

# --- Create a month and quarter level data set -----------------------------------------

monthly_count2 <- visit2 %>% 
  group_by(round_month) %>% 
  summarise(num_VISITS = n(), #num rows (visits)
            num_PEOPLE_SERVED = sum(hhMembers_visit),# number of people that month
            num_PEOPLE_SNAP = sum(snap=="1"),
            num_threshold=sum(threshold=="1"),
            num_nosnap_threshold=sum(snap=="0"&threshold=="1")
  ) %>% mutate(percent_snap=num_PEOPLE_SNAP/num_PEOPLE_SERVED*100,
               percent_nosnap_threshold=num_nosnap_threshold/num_threshold*100,
               percent_threshold=num_threshold/num_PEOPLE_SERVED*100
  )

quarter_count2 <- visit2 %>% 
  group_by(round_quarter) %>% 
  summarise(num_VISITS = n(), #num rows (visits)
            num_PEOPLE_SERVED = sum(hhMembers_visit),# number of people that month
            num_PEOPLE_SNAP = sum(snap=="1"),
            num_threshold=sum(threshold=="1"),
            num_nosnap_threshold=sum(snap=="0"&threshold=="1")
  ) %>% mutate(percent_snap=num_PEOPLE_SNAP/num_PEOPLE_SERVED*100,
               percent_nosnap_threshold=num_nosnap_threshold/num_threshold*100,
               percent_threshold=num_threshold/num_PEOPLE_SERVED*100
  )

#during the dataset creation, they changed into character
monthly_count2$round_month <- as.Date(monthly_count2$round_month) # I have no clue why round_quarter is a character 
quarter_count2$round_quarter <- as.Date(quarter_count2$round_quarter) # I have no clue why round_quarter is a character

### --- Create an Individual Level Dataset -------------------------------------
individuals <- all2 %>% 
  group_by(clientId) %>% 
  summarize(
    afn = first(houseHoldIdAfn),
    gender = first(gender),
    race = first(race),
    education = first(education),
    foodstamps = first(foodstamps),
    dietaryIssue = first(dietaryIssue), 
    veteran = first(veteran),
    category = first(category),
    incomeSource = first(incomeSource)
  )

### --- Create a household level dataset: Grace ---------------------------------------
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
    
    veteran = as.integer(any(veteran == "Yes"))
  )

hh2_2024 <- hh_data2 %>%
  filter(visit_in_2024=="1") %>% 
  select(householdMembers, visit_count_2024, IncomeSource, fedPovertyLevel,
         annualIncome, foodstamps, primary_visit_location, primary_service,
         primary_visitor_occupation, dietary_issue, veteran)

### --- Create a household level dataset for 2024: Amelia ---------------------------------------

# General household-level summary (all years)
hh_summary_all <- all2 %>%
  group_by(houseHoldIdAfn) %>%
  summarize(
    householdMembers = max(householdMembers, na.rm = TRUE),
    first_visit = min(servedDate, na.rm = TRUE),
    first_visit24 = if_else(year(first_visit)==2024,1,0),
    last_visit = max(servedDate, na.rm = TRUE)
  ) %>% ungroup()

# 2024-specific metrics
hh_summary_2024 <- all2 %>%
  filter(year(servedDate) == 2024) %>%
  group_by(houseHoldIdAfn) %>%
  summarize(
    visit_count_2024 = n_distinct(servedDate),
    
    meantimeemployed = mean(incomeSource == "Employed", na.rm = TRUE),
    unstableemployement24 = if_else(meantimeemployed<1,1,0),
    #if there is not at least one employed person at all times, then meantime will be less than one
    
    medfedPovertyLevel = median(fedPovertyLevel),
    medannualIncome = median(annualIncome),
    
    proportiontimeonfoodstamps = mean(foodstamps == "Yes", na.rm = TRUE), #foodstamp value the same across household
    foodstampschange = as.integer(n_distinct(foodstamps) > 1), #if there are more than 1 distinct values for foodstamps - means it changed
    foodstamps = as.integer(any(foodstamps == "Yes")),
    
    primary_visit_location = names(which.max(table(location))),
    primary_service = names(which.max(table(service))),
    primary_visitor_occupation = names(which.max(table(category))),
    
    dietary_issue = as.integer(any(!dietaryIssue %in% c("None", "Unknown"))),
    
    anyveteran = as.integer(any(veteran == "Yes", na.rm = TRUE)),
    anycollege = as.integer(any(education=="College",  na.rm = TRUE)),
    anyschoolkid = as.integer(any(education=="Currently",  na.rm = TRUE))
  ) %>% ungroup()

# Merge both
hh_data24 <- hh_summary_all %>%
  left_join(hh_summary_2024, by = "houseHoldIdAfn") %>% filter(year(last_visit) == 2024)


### --- Create CSV's -----------------------------------------------------------
write.csv(hh_data2, "Data/hh_data2.csv", row.names = FALSE)
write.csv(individuals, "Data/individual.csv", row.names=FALSE)
write.csv(visit2, "Data/visit2.csv", row.names=FALSE)
write.csv(hh2_2024, "Data/hh2_2024.csv", row.names=FALSE)
write.csv(hh_data24, "Data/amelia_hh2_2024.csv", row.names=FALSE)
write.csv(monthly_count2, "Data/monthly_count2.csv", row.names = FALSE)
write.csv(quarter_count2, "Data/quarter_count2.csv", row.names = FALSE)
write.csv(hh2_2024, "Data/hh2_2024", row.names=FALSE)
















