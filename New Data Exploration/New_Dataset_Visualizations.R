### --- Create Datasets and Load in Packages
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(tidyverse)
library(lubridate)

hh_data24 <- read.csv("Data/amelia_hh2_2024.csv")
hh_data2 <- read.csv("Data/hh_data2.csv")

### --- Making visualizations -------------------------------------------------
ggplot(hh_data24, aes(x = dietary_issue)) +
  geom_bar(fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Frequency of Categories",
       x = "Category",
       y = "Count")

hh_data2 <- hhdata2 %>% 
  