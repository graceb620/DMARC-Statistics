### --- Create Datasets and Load in Packages
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(tidyverse)
library(lubridate)
library(scales)

hh_data24 <- read.csv("Data/hh2_only2024calc.csv")
hh_data2 <- read.csv("Data/hh_data2.csv")

### --- Making visualizations -------------------------------------------------
# Summarize the data
hh_summary <- hh_data2 %>%
  filter(year(first_visit) > 2018) %>%
  mutate(visit_year = year(first_visit)) %>%
  group_by(visit_year) %>%
  summarize(count = sum(dietary_issue, na.rm = TRUE))

# Plot the data
ggplot(hh_summary, aes(x = factor(visit_year), y = count)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Number of Households with Dietary Issues by Year",
    x = "Year",
    y = "Number of Households with Dietary Issues"
  ) +
  theme_minimal()


# Prepare data
hh_summary <- hh_data2 %>%
  filter(year(first_visit) > 2018) %>%
  mutate(visit_year = year(first_visit)) %>%
  group_by(visit_year) %>%
  summarize(
    total_visitors = n(),
    dietary_issues = sum(dietary_issue, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(total_visitors, dietary_issues),
               names_to = "category", values_to = "count")

# Plot with side-by-side bars
ggplot(hh_summary, aes(x = factor(visit_year), y = count, fill = category)) +
  geom_col(position = "dodge") +
  labs(
    title = "Total Visitors vs. Dietary Issues by Year",
    x = "Year",
    y = "Count",
    fill = "Category"
  ) +
  scale_fill_manual(values = c("total_visitors" = "grey70", "dietary_issues" = "steelblue")) +
  theme_minimal()  


# Prepare data with proportion
hh_summary <- hh_data2 %>%
  filter(year(first_visit) > 2018) %>%
  mutate(visit_year = year(first_visit)) %>%
  group_by(visit_year) %>%
  summarize(
    total_visitors = n(),
    dietary_issues = sum(dietary_issue, na.rm = TRUE)
  ) %>%
  mutate(proportion = dietary_issues / total_visitors)

# Plot proportions
ggplot(hh_summary, aes(x = factor(visit_year), y = proportion)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of Households with Dietary Issues by Year",
    x = "Year",
    y = "Proportion of Households with Dietary Issues"
  ) +
  theme_minimal()


# Summarize data
hh_summary <- hh_data2 %>%
  filter(year(first_visit) > 2018) %>%
  mutate(visit_year = year(first_visit)) %>%
  group_by(visit_year) %>%
  summarize(
    total_visitors = n(),
    dietary_issues = sum(dietary_issue, na.rm = TRUE)
  ) %>%
  mutate(proportion = dietary_issues / total_visitors)


# Plot proportions with dynamic scaling
ggplot(hh_summary, aes(x = factor(visit_year), y = proportion)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(dietary_issues, "/", total_visitors)), 
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Percent of Households with Dietary Issues by Year",
    x = "Year",
    y = "Percent of Households with Dietary Issues"
  ) +
  theme_minimal()

### --- Visualizations on Location ----------------------------------------
# Summarize and get top 5 locations
hh_24_locations <- hh_data24 %>%
  group_by(primary_visit_location) %>%
  summarise(count = n()) %>%
  slice_max(order_by = count, n = 5)

# Plot the top 5
ggplot(hh_24_locations, aes(x = reorder(primary_visit_location, count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 5 Visit Locations",
       x = "Primary Visit Location",
       y = "Count") +
  coord_flip() +  # Makes the plot horizontal for readability
  theme_minimal()















