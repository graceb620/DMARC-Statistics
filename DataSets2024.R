library(lubridate)
library(tidyverse)
library(dplyr)
library(purrr)

newData <- read.csv("Data/DMARC Data 2018-2024 copy.csv")

colnames(newData)
colnames(all)
# The new updated data does not have any SNAP related variables