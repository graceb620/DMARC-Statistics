rm(list=ls()) 
source("Create_datasets.R") 
library(ggplot2)
library(reshape2)
library(RColorBrewer)


# Visualization #1 - Variable Importance Plot (Random Forest) ------------------

# Message: Identifies the most influential factors in predicting first-time visits in 2023.
# Why it’s useful: Helps prioritize which characteristics are most important in understanding first-time visits.
# Audience: Stakeholders who make decisions about DMARC's future operations.

# code in random_forest.R



# Visualization #2 - Household Characteristics by First Visit Status -----------

# Message: Highlights the relationship between household size and income.
# Why it’s useful: Helps identify how income varies with household size, which can inform policies on resource allocation and support programs.
# Audience: Stakeholders who make decisions about DMARC's future operations ; Stakeholders interested in social demographics.

#Household Income vs. Number of People in Household
ggplot(hh_23, aes(x = n_people_in_household, y = income_first)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Household Income vs. Household Size",
       x = "Number of People in Household",
       y = "Household Income") +
  theme_minimal()

# Household Income vs. Federal Poverty Level
ggplot(hh_23, aes(x = fed_poverty_level_first, y = income_first)) +
  geom_point(alpha = 0.5, color = "red") +
  labs(title = "Household Income vs. Federal Poverty Level",
       x = "Federal Poverty Level",
       y = "Household Income") +
  theme_minimal()

# Used ChatGPT to help with formatting. 























