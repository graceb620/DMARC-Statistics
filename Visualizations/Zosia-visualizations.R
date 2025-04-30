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

#Goal: Show how common it is for households to visit multiple pantry locations and whether that behavior changed over time.
all %>%
  semi_join(hh_23, by = "afn") %>%
  group_by(afn) %>%
  summarise(more_than_one_location = as.integer(n_distinct(location) > 1)) %>%
  ggplot(aes(factor(more_than_one_location))) +
  geom_bar(fill = "steelblue") +
  labs(x = "Visited More Than One Location", 
       y = "Number of Households",
       title = "Households Visiting More Than One Pantry Location")

# Summarize: Did a household visit more than one location, and in what year?
multi_location_by_year <- all %>%
  semi_join(hh_23, by = "afn") %>%
  group_by(afn) %>%
  summarise(
    more_than_one_location = as.integer(n_distinct(location) > 1),
    first_visit_year = year(min(date))
  ) %>%
  filter(first_visit_year > 2019 & first_visit_year < 2024) %>%
  count(first_visit_year, more_than_one_location) %>%
  group_by(first_visit_year) %>%
  mutate(percent = (n / sum(n)) * 100)

# Plot: Percent of households visiting more than one location per year
ggplot(multi_location_by_year, aes(x = factor(first_visit_year), y = percent, 
                                   fill = factor(more_than_one_location, levels = c(0, 1), 
                                                 labels = c("No", "Yes")))) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(values = c("Yes" = "#E69F00", "No" = "#56B4E9")) +
  geom_text(aes(label = round(percent, 1)), 
            position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 3) +
  labs(
    title = "Proportion of Households Visiting More Than One Pantry Location",
    subtitle = "Grouped by Year of First Visit",
    x = "First Visit Year",
    y = "Percentage of Households",
    fill = "Visited >1 Location"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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





















