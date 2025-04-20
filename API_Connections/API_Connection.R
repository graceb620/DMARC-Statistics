### READ THIS: THis doc is not meant to be run,
### There is a line that will run this doc on the Create_datasets.R


### --- Test --------------------------------------------------------------------
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

# Base URL for ACS 5-Year Data
BASE_URL <- "https://api.census.gov/data"

# Choose the variable
API_SNAP <- "B22003_002E"  # Number of SNAP households

# Initialize an empty dataframe to store results
API_SNAP_Results <- data.frame()

# Loop through years from 2012 to 2023
for (year in 2012:2023) {
  
  url <- paste0(BASE_URL, "/", year, "/acs/acs5")
  
  # Set query parameters
  if (year < 2020) {
    params <- list(
      get = paste("NAME", API_SNAP, sep = ","),
      `for` = "zip code tabulation area:*",
      `in` = "state:19"  # Iowa (FIPS code 19)
    )
  } else {
    params <- list(
      get = paste("NAME", API_SNAP, sep = ","),
      `for` = "zip code tabulation area:*"
    )
  }
  
  # Make API request
  API_SNAP_Response <- GET(url, query = params)
  
  # Check if the request was successful
  if (status_code(API_SNAP_Response) == 200) {
    API_SNAP_data <- content(API_SNAP_Response, as = "text", encoding = "UTF-8")
    json_data <- fromJSON(API_SNAP_data)
    curr_year <- as.data.frame(json_data)
    colnames(curr_year) <- curr_year[1, ] # use first row as headers
    curr_year <- curr_year[-1, ] # reset to all but the first row
    curr_year$year <- year # add a new column for the year
    curr_year[[API_SNAP]] <- as.numeric(curr_year[[API_SNAP]])# make the value column into a number
    API_SNAP_Results <- bind_rows(API_SNAP_Results,curr_year) # concatenate onto running dataframe
    
  } else {
    print(paste("Error fetching data for", year, ":", status_code(API_SNAP_Response)))
  }
}

print(API_SNAP_Results)

### --- ------------------------------------
# Choose the variable
API_HH <- "B11001_001E"  # Number of SNAP households

# Initialize an empty dataframe to store results
API_HH_Results <- data.frame()

# Loop through years from 2012 to 2023
for (year in 2012:2023) {
  
  url <- paste0(BASE_URL, "/", year, "/acs/acs5")
  
  # Set query parameters
  if (year < 2020) {
    params <- list(
      get = paste("NAME", API_HH, sep = ","),
      `for` = "zip code tabulation area:*",
      `in` = "state:19"  # Iowa (FIPS code 19)
    )
  } else {
    params <- list(
      get = paste("NAME", API_HH, sep = ","),
      `for` = "zip code tabulation area:*"
    )
  }
  
  # Make API request
  API_HH_Response <- GET(url, query = params)
  
  # Check if the request was successful
  if (status_code(API_HH_Response) == 200) {
    API_HH_data <- content(API_HH_Response, as = "text", encoding = "UTF-8")
    json_data <- fromJSON(API_HH_data)
    curr_year <- as.data.frame(json_data)
    colnames(curr_year) <- curr_year[1, ] # use first row as headers
    curr_year <- curr_year[-1, ] # reset to all but the first row
    curr_year$year <- year # add a new column for the year
    curr_year[[API_HH]] <- as.numeric(curr_year[[API_HH]])# make the value column into a number
    API_HH_Results <- bind_rows(API_HH_Results,curr_year) # concatenate onto running dataframe
    
  } else {
    print(paste("Error fetching data for", year, ":", status_code(API_HH_Response)))
  }
}

print(API_HH_Results)
