rm(list=ls()) 
source("Create_datasets.R") 
library(ggplot2)
library(reshape2)
library(RColorBrewer)

#Proportion of First-Time Pantry Visits by Household Type

# Message: Shows the proportion of first-time visitors with and without children.
# Why it’s useful: Helps identify trends in household demographics, informing decisions on how to allocate resources and address needs.
# Audience: Stakeholders who make decisions about DMARC's future operations.


# Aggregate Data to Yearly Counts
hh_data_summary <- hh_data %>%
  filter(year(first_visit) > 2019 & year(first_visit) < 2024) %>%
  mutate(visit_year = floor_date(first_visit, "year")) %>%
  count(visit_year, kids) %>%
  group_by(visit_year) %>%
  mutate(percent = (n / sum(n)) * 100)  # Ensure percentage calculation is correct

# Create Stacked Bar Chart with Percentages
ggplot(hh_data_summary, aes(x = visit_year, y = percent, fill = factor(kids, levels = c(0, 1), labels = c("No", "Yes")))) +  # Mapping 0/1 to Yes/No
  geom_bar(stat = "identity", position = "stack", color = "black") +  # Stacked bars
  scale_fill_manual(values = c("Yes" = "#E69F00", "No" = "#56B4E9")) +  # Colorblind-friendly colors
  geom_text(aes(label = round(percent, 1)), position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 3) +  # Smaller text size for labels
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Proportion of First-Time Pantry Visits by Household Type",
    subtitle = "Focused on whether the household has children or not",
    x = "First Visit Year",
    y = "Percentage of Visiting Households",
    fill = "Children in the Household"
  ) +
  theme_minimal() +  # Keep default theme for minimal styling
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)  # Angle x-axis labels for readability
  )

# How many of them are Single Parents?
hh_data_summary_single_parent <- hh_data %>%
  filter(between(year(first_visit), 2020, 2023) & kids == 1) %>%
  mutate(visit_year = floor_date(first_visit, "year")) %>%
  count(visit_year, single_parent) %>%
  group_by(visit_year) %>%
  mutate(percent = (n / sum(n)) * 100)

# Create Stacked Bar Chart with Percentages
ggplot(hh_data_summary_single_parent, aes(x = visit_year, y = percent, fill = factor(single_parent, levels = c("Yes", "No / Does not apply"), labels = c("Yes", "No")))) +
  geom_bar(stat = "identity", position = "stack", color = "black") +  
  geom_text(aes(label = round(percent, 1)), position = position_stack(vjust = 0.5), 
            color = "white", fontface = "bold", size = 3) +  
  scale_fill_manual(values = c("Yes" = "#E69F00", "No" = "#56B4E9")) +  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Proportion of First-Time Pantry Visitors by Single Parent Status",
    subtitle = "Data from 2020 to 2023",
    x = "First Visit Year",
    y = "Percentage of Visiting Households",
    fill = "Single Parent"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# What does this look like for returners? 
hh_data_summary_single_parent_returners <- hh_data %>%
  filter(between(year(last_visit), 2020, 2023) & kids == 1 & first_visit != last_visit) %>%
  mutate(visit_year = floor_date(last_visit, "year")) %>%
  count(visit_year, single_parent) %>%
  group_by(visit_year) %>%
  mutate(percent = (n / sum(n)) * 100)

# Create Stacked Bar Chart with Percentages
ggplot(hh_data_summary_single_parent_returners, aes(x = visit_year, y = percent, fill = factor(single_parent, levels = c("Yes", "No / Does not apply"), labels = c("Yes", "No")))) +
  geom_bar(stat = "identity", position = "stack", color = "black") +  
  geom_text(aes(label = round(percent, 1)), position = position_stack(vjust = 0.5), 
            color = "white", fontface = "bold", size = 3) +  
  scale_fill_manual(values = c("Yes" = "#E69F00", "No" = "#56B4E9")) +  
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Proportion of Returner Pantry Visitors by Single Parent Status",
    subtitle = "Data from 2020 to 2023",
    x = "Last Visit Year",
    y = "Percentage of Visiting Households",
    fill = "Single Parent"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Used ChatGPT to help with formatting.

# Additional Visualizations

# Most common housing areas:

top_zips <- visit %>%
  count(zip, sort = TRUE) %>%
  top_n(10, wt = n)

ggplot(top_zips, aes(x = reorder(zip, n), y = n)) +
  geom_col(fill = "#3498DB") +
  coord_flip() +
  labs(
    title = "Top 10 Zip Codes of Households Served",
    x = "Zip Code",
    y = "Number of Visits"
  ) +
  theme_minimal()

# Distribution of Household Sizes
ggplot(hh_data, aes(x = n_people_in_household)) +
  geom_histogram(binwidth = 1, fill = "#F39C12", color = "white") +
  labs(
    title = "Distribution of Household Sizes",
    x = "Number of People in Household",
    y = "Count of Households"
  ) +
  theme_minimal()

visit %>%
  count(housing_status, sort = TRUE) %>%
  ggplot(aes(x = reorder(housing_status, n), y = n)) +
  geom_col(fill = "#1ABC9C") +
  coord_flip() +
  labs(
    title = "Visit Counts by Housing Status",
    x = "Housing Status",
    y = "Number of Visits"
  ) +
  theme_minimal()

hh_data <- hh_data %>%
  mutate(metro_status = if_else(first_visit_zip %in% c("50311", "50310", "50312"), "Metro", "Non-Metro"))

# Summarize household counts over years by metro status
hh_trends <- hh_data %>%
  mutate(metro_status = if_else(first_visit_zip %in% c("50311", "50310", "50312"), "Metro", "Non-Metro")) %>%
  filter(between(year(first_visit), 2020, 2023)) %>%
  mutate(visit_year = year(first_visit)) %>%
  group_by(visit_year, metro_status) %>%
  summarise(hh_count = n()) %>%
  ungroup()

# Create a bar chart of household count over years by metro status
ggplot(hh_trends, aes(x = factor(visit_year), y = hh_count, fill = metro_status)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar chart with dodged bars
  labs(title = "Household Count Over Time by Metro Status",
       x = "Year",
       y = "Number of Households",
       fill = "Metro Status") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


# Look at pantry loactions
# Top 10 most used locations (pantries)
top_10_locations <- all %>%
  count(location, name = "count") %>%
  arrange(desc(count)) %>%
  slice_head(n = 10)

# Plot it
ggplot(top_10_locations, aes(x = reorder(location, count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Most Used Pantries",
       x = "Location",
       y = "Number of Records") +
  scale_y_continuous(labels = scales::comma) +  # Formats y-axis with commas
  coord_flip() +  # Horizontal bar chart for better readability
  theme_minimal()





















