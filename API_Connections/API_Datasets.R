source("API_Connections/API_Connection.R")
### --- API Related Datasets ---------------------------------------------------
library(lubridate)
library(tidyverse)
library(dplyr)
library(purrr)

# --- Merging the columns -----------------
API_data <- reduce(list(API_HH_Results, API_SNAP_Results), 
                   full_join, by = c("zip code tabulation area", 
                                     "year", "state", "NAME")) 

# --- Creating large dataset
API_data <- API_data %>% rename(
  NumHH = B11001_001E,
  SnapHH = B22003_002E,
  zip_code = `zip code tabulation area`
) %>% 
  mutate(
    SnapRatio = SnapHH / NumHH
  ) %>% 
  filter(year > 2019)
# --- Filtering to Iowa Zip codes Only -------
# Because of how the API is, post 2020 results does not let you pull by state
# However, pre 2020 does.
# FIX: Pull the zip codes from 2019 and filter so that each dataframe only include
# the iowa zip codes
zipcodes_2019 <- API_HH_Results %>% 
  filter(year == 2019) %>% 
  select(`zip code tabulation area`) %>%
  distinct()  # Ensure unique ZIP codes
API_data <- API_data %>% 
  filter(zip_code %in% zipcodes_2019$`zip code tabulation area`) 

# --- Creating Datasets for each individual year -------
API_2023 <- API_data %>% filter(year==2023)


# Creating CSV's 
write.csv(API_data, "Data/API_data.csv", row.names = FALSE)
write.csv(API_2023, "Data/API_2023.csv", row.names = FALSE)

