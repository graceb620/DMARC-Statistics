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



# Visualization #2 - Number of Households with Children with First Visit in 2023 ------

# Message: Compares the number of households with and without children that made their first visit in 2023.
# Why it’s useful: Helps identify trends in household demographics, informing decisions on how to allocate resources and address needs.
# Audience: Stakeholders who make decisions about DMARC's future operations.


ggplot(hh_first_visit_2023, aes(x = first_visit, fill = as.factor(kids))) +
  geom_histogram(binwidth = 30, position = "identity", alpha = 0.5, color = "black") +
  scale_fill_brewer(labels = c("No Children", "Has Children")) +
  labs(
    title = "Proportion of First-Time Visitors with and without Children",
    x = "First Visit Date",
    y = "Number of Households",
    fill = ""
  ) +
  theme_minimal()


# Used ChatGPT to help with formatting.





















