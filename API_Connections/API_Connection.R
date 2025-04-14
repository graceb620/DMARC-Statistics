### READ THIS: THis doc is not meant to be run,
### There is a line that will run this doc on the Create_datasets.R

library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

# Base URL for ACS 5-Year Data
BASE_URL <- "https://api.census.gov/data"

### --- Pulling API_SnapHH -----------------------------------------------------
API_SnapHH <- "B22003_001E"  # Number of SNAP households
API_SnapHH_DataFrame <- data.frame()

# Loop through years from 2020 to 2023
for (year in 2019:2023) {
  
  url <- paste0(BASE_URL, "/", year, "/acs/acs5")
  
  # Set query parameters
  if (year < 2020) {
    params <- list(
      get = paste("NAME", API_SnapHH, sep = ","),
      `for` = "zip code tabulation area:*",
      `in` = "state:19"  # Iowa (FIPS code 19)
    )
  } else {
    params <- list(
      get = paste("NAME", API_SnapHH, sep = ","),
      `for` = "zip code tabulation area:*"
    )
  }
  
  # Make API request
  response <- GET(url, query = params)
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    data <- content(response, as = "text", encoding = "UTF-8")
    json_data <- fromJSON(data)
    curr_year <- as.data.frame(json_data)
    colnames(curr_year) <- curr_year[1, ] # use first row as headers
    curr_year <- curr_year[-1, ] # reset to all but the first row
    curr_year$year <- year # add a new column for the year
    curr_year[[API_SnapHH]] <- as.numeric(curr_year[[API_SnapHH]])# make the value column into a number
    API_SnapHH_DataFrame <- bind_rows(API_SnapHH_DataFrame,curr_year) # concatenate onto running dataframe
    
  } else {
    print(paste("Error fetching data for", year, ":", status_code(response)))
  }
}

### --- Pulling APINumHH -------------------------------------------------
API_NumHH <- "B11016_001E" # Number of HH per zipcode
API_NumHH_DataFrame <- data.frame()

# Loop through years from 2020 to 2023
for (year in 2019:2023) {
  
  url <- paste0(BASE_URL, "/", year, "/acs/acs5")
  
  # Set query parameters
  if (year < 2020) {
    params <- list(
      get = paste("NAME", API_NumHH, sep = ","),
      `for` = "zip code tabulation area:*",
      `in` = "state:19"  # Iowa (FIPS code 19)
    )
  } else {
    params <- list(
      get = paste("NAME", API_NumHH, sep = ","),
      `for` = "zip code tabulation area:*"
    )
  }
  
  # Make API request
  response <- GET(url, query = params)
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    data <- content(response, as = "text", encoding = "UTF-8")
    json_data <- fromJSON(data)
    curr_year <- as.data.frame(json_data)
    colnames(curr_year) <- curr_year[1, ] # use first row as headers
    curr_year <- curr_year[-1, ] # reset to all but the first row
    curr_year$year <- year # add a new column for the year
    curr_year[[API_NumHH]] <- as.numeric(curr_year[[API_NumHH]])# make the value column into a number
    API_NumHH_DataFrame <- bind_rows(API_NumHH_DataFrame,curr_year) # concatenate onto running dataframe
    
  } else {
    print(paste("Error fetching data for", year, ":", status_code(response)))
  }
}

### --- Pulling API_MedHHIncome -------------------------------------------------
API_HHIncome <- "B19013_001E" # Single Parent household
API_HHIncome_DataFrame <- data.frame()

# Loop through years from 2020 to 2023
for (year in 2019:2023) {
  
  url <- paste0(BASE_URL, "/", year, "/acs/acs5")
  
  # Set query parameters
  if (year < 2020) {
    params <- list(
      get = paste("NAME", API_HHIncome, sep = ","),
      `for` = "zip code tabulation area:*",
      `in` = "state:19"  # Iowa (FIPS code 19)
    )
  } else {
    params <- list(
      get = paste("NAME", API_HHIncome, sep = ","),
      `for` = "zip code tabulation area:*"
    )
  }
  
  # Make API request
  response <- GET(url, query = params)
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    data <- content(response, as = "text", encoding = "UTF-8")
    json_data <- fromJSON(data)
    curr_year <- as.data.frame(json_data)
    colnames(curr_year) <- curr_year[1, ] # use first row as headers
    curr_year <- curr_year[-1, ] # reset to all but the first row
    curr_year$year <- year # add a new column for the year
    curr_year[[API_HHIncome]] <- as.numeric(curr_year[[API_HHIncome]])# make the value column into a number
    API_HHIncome_DataFrame <- bind_rows(API_HHIncome_DataFrame,curr_year) # concatenate onto running dataframe
    
  } else {
    print(paste("Error fetching data for", year, ":", status_code(response)))
  }
}

### --- Looking at the 1 year census data --------------------------------------


































