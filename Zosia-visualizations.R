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

#2023

# Aggregate Data to Monthly Counts
hh_first_visit_2023_summary <- hh_first_visit_2023 %>%
  mutate(visit_month = floor_date(first_visit, "month")) %>%
  count(visit_month, kids) %>%
  group_by(visit_month) %>%
  mutate(percent = (n / sum(n)) * 100)  # Ensure percentage calculation is correct

# Create Stacked Bar Chart with Percentages
ggplot(hh_first_visit_2023_summary, aes(x = visit_month, y = percent, fill = factor(kids, levels = c(0, 1), labels = c("No", "Yes")))) +  # Mapping 0/1 to Yes/No
  geom_bar(stat = "identity", position = "stack", color = "black") +  # Stacked bars
  scale_fill_manual(values = c("Yes" = "#E69F00", "No" = "#56B4E9")) +  # Colorblind-friendly colors
  geom_text(aes(label = round(percent, 1)), position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 3) +  # Smaller text size for labels
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(
    title = "Proportion of First-Time Pantry Visits by Household Type (2023)",
    subtitle = "Focused on whether the household has children or not",
    x = "First Visit Month",
    y = "Percentage of Visiting Households",
    fill = "Children in the Household"
  ) +
  theme_minimal() +  # Keep default theme for minimal styling
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Angle x-axis labels for readability
  )


#2022

# Aggregate Data to Monthly Counts
hh_first_visit_2022_summary <- hh_first_visit_2022 %>%
  mutate(visit_month = floor_date(first_visit, "month")) %>%
  count(visit_month, kids) %>%
  group_by(visit_month) %>%
  mutate(percent = (n / sum(n)) * 100)  # Ensure percentage calculation is correct

# Create Stacked Bar Chart with Percentages
ggplot(hh_first_visit_2022_summary, aes(x = visit_month, y = percent, fill = factor(kids, levels = c(0, 1), labels = c("No", "Yes")))) +  # Mapping 0/1 to Yes/No
  geom_bar(stat = "identity", position = "stack", color = "black") +  # Stacked bars
  scale_fill_manual(values = c("Yes" = "#E69F00", "No" = "#56B4E9")) +  # Colorblind-friendly colors
  geom_text(aes(label = round(percent, 1)), position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 3) +  # Smaller text size for labels
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(
    title = "Proportion of First-Time Pantry Visits by Household Type (2022)",
    subtitle = "Focused on whether the household has children or not",
    x = "First Visit Month",
    y = "Percentage of Visiting Households",
    fill = "Children in the Household"
  ) +
  theme_minimal() +  # Keep default theme for minimal styling
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Angle x-axis labels for readability
  )

# Used ChatGPT to help with formatting.





















