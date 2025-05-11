# Main contributor: Zofia Landowska 
rm(list=ls()) 
source("Create_datasets.R") 
source("DataSets2024.R") 
library(ggplot2)
library(reshape2)
library(RColorBrewer)


# Visualize the Proportion of First-Time Pantry Visits by Household Type

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


# Distribution of Household Sizes
ggplot(hh_data, aes(x = n_people_in_household)) +
  geom_histogram(binwidth = 1, fill = "#F39C12", color = "white") +
  labs(
    title = "Distribution of Household Sizes",
    x = "Number of People in Household",
    y = "Count of Households"
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

multi_location_by_year <- hh_data %>%
  filter(year(first_visit) > 2019 & year(first_visit) < 2024) %>%
  mutate(first_visit_year = year(first_visit)) %>%
  count(first_visit_year, more_than_one_change_location) %>%
  group_by(first_visit_year) %>%
  mutate(percent = (n / sum(n)) * 100)

ggplot(multi_location_by_year, aes(x = factor(first_visit_year), y = percent,
                                   fill = factor(more_than_one_change_location, levels = c(0, 1),
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

# Count how many times each household visited each location
location_visits <- all %>%
  group_by(afn, location) %>%
  summarise(visits = n(), .groups = "drop")

# Identify (afn, location) pairs where the household visited exactly once
# Then count how many unique households visited each location more than once
locations_with_repeat_visits <- location_visits %>%
  filter(visits > 1) %>%
  distinct(location)

# Now, find locations that are NOT in the list above
locations_visited_only_once <- all %>%
  distinct(location) %>%
  anti_join(locations_with_repeat_visits, by = "location")

# View result
locations_visited_only_once

all %>%
  filter(location == "DMARC Mobile - Capitol View") %>%
  summarise(unique_households = n_distinct(afn),
            total_visits = n())

# Only one household visited this pantry ONLY ONCE


#Explore whether SNAP recipients are more or less likely to switch pantry locations.

# Step 1: Summarize the data for households that have switched locations
snapping_behavior <- hh_data %>%
  group_by(snap) %>%
  summarise(
    switched_location = mean(more_than_one_change_location, na.rm = TRUE),
    n = n()
  )

# Step 2: Plot the comparison
ggplot(snapping_behavior, aes(x = factor(snap), y = switched_location)) +
  geom_bar(stat = "identity", fill = c("#56B4E9", "#E69F00"), color = "black") +
  geom_text(aes(label = scales::percent(switched_location, accuracy = 1)), 
            vjust = -0.3, color = "black", fontface = "bold", size = 4) +
  labs(
    title = "Likelihood of Switching Pantry Locations: SNAP vs Non-SNAP Households",
    x = "SNAP Recipient",
    y = "Percentage of Households Switching Locations",
    fill = "SNAP Status"
  ) +
  scale_x_discrete(labels = c("0" = "Non-SNAP", "1" = "SNAP")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Visually reinforce that SNAP status and location-switching behavior have become more prevalent among new visitors in 2023.
# Step 1: Summarize SNAP and location-switching behavior for new visitors in 2023
summary_2023 <- hh_first_visit_2023 %>%
  summarise(
    snap_recipient = mean(snap_first_2023, na.rm = TRUE),
    location_switching = mean(more_than_one_change_location, na.rm = TRUE),
    n = n()
  )

# Step 2: Compare SNAP and location-switching behavior in 2023 with past data (if desired)
# This can be used if you want to show a comparison with prior years
yearly_summary <- hh_data %>%
  filter(year(first_visit) <= 2023) %>%
  group_by(year(first_visit)) %>%
  summarise(
    snap_recipient = mean(snap_first_visit, na.rm = TRUE),
    location_switching = mean(more_than_one_change_location, na.rm = TRUE)
  )

# Step 3: Create a plot to highlight trends in 2023
ggplot(yearly_summary, aes(x = `year(first_visit)`, y = location_switching, color = "Location Switching")) +
  geom_line(size = 1) + 
  geom_point(aes(size = 3), color = "steelblue") +
  geom_point(data = summary_2023, aes(x = 2023, y = location_switching), 
             color = "red", size = 4, shape = 16) +
  geom_text(data = summary_2023, aes(x = 2023, y = location_switching, label = "2023 Highlighted"), 
            color = "black", hjust = -0.2, vjust = 0) +
  geom_line(aes(x = `year(first_visit)`, y = snap_recipient, color = "SNAP Recipients"), 
            size = 1, linetype = "dashed") +
  scale_color_manual(values = c("Location Switching" = "blue", "SNAP Recipients" = "orange")) +
  labs(
    title = "Prevalence of Location-Switching and SNAP Among New Visitors in 2023",
    subtitle = "Comparing behavior trends over years with emphasis on 2023",
    x = "Year of First Visit",
    y = "Proportion of Households",
    color = "Behavior Type",
    size = "Size"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )


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


# 2024 Data
hh_data24 <- read.csv("Data/amelia_hh2_2024.csv")

#Goal: Show how common it is for households to visit multiple pantry locations in 2024.
# Cleaned and labeled data for plotting
multi_location_by_child <- hh_data24 %>%
  mutate(
    visited_multiple_locations = ifelse(visit_location_change == 1, "Multiple Locations", "One Location"),
    has_child = ifelse(anyschoolchild == 1, "Has Child", "No Child")
  ) %>%
  count(has_child, visited_multiple_locations) %>%
  group_by(has_child) %>%
  mutate(percent = n / sum(n) * 100)

# Plot
ggplot(multi_location_by_child,
       aes(x = visited_multiple_locations, y = percent, fill = has_child)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(percent, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.3, size = 3) +
  scale_fill_manual(values = c("Has Child" = "#E69F00", "No Child" = "#56B4E9")) +
  labs(
    title = "Households Visiting Multiple Pantry Locations in 2024",
    x = "Pantry Visit Pattern",
    y = "Percentage of Households",
    fill = "Household Has School-Age Child"
  ) +
  theme_minimal()













