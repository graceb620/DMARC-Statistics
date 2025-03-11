library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

# Base URL for ACS 5-Year Data
BASE_URL <- "https://api.census.gov/data"

# Choose the variable
API_SnapHH <- "B22003_001E"  # Number of SNAP households

# Initialize an empty dataframe to store results
results <- data.frame()

# Loop through years from 2012 to 2023
for (year in 2012:2023) {
  
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
    results <- bind_rows(results,curr_year) # concatenate onto running dataframe
    
  } else {
    print(paste("Error fetching data for", year, ":", status_code(response)))
  }
}

print(results)