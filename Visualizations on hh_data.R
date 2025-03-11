library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

hh_data_2023<-read.csv('Data/hh_data23.csv', stringsAsFactors=FALSE)
hh_data <- read.csv('Data/hh_data.csv', stringsAsFactors=FALSE)

##create visualizations to analyze hh_level dataset ----------------------------

# Graph the First Visits per household
ggplot(hh_data, aes(x = first_visit)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density of First Visits Over Time Per Household",
       x = "First Visit Date",
       y = "Density") 

##Graph of Family types and their homeless status


hh_data_2023 %>%
  count(homeless) %>%
  ggplot(aes(x = homeless, y = n, fill = homeless)) +
  geom_col() +
  labs(
    title = "Count of Homeless vs. Non-Homeless Households in 2023",
    x = "Homeless Status",
    y = "Count"
  )


#First visit distribution by Year
hh_data_2023 %>%
  mutate(first_visit_year = year(first_visit)) %>%
  count(first_visit_year) %>%
  ggplot(aes(x = as.factor(first_visit_year), y = n, fill = as.factor(first_visit_year))) +
  geom_col() +
  labs(title = "Households' First Visit Year", x = "Year", y = "Count") +
  theme_minimal()
##More visits in 2018 followed by 2019 and 2023, the least in 2024 and 2021.

#Proportion of Households recieving SNAP benefits
hh_data %>%
  count(snap) %>%
  ggplot(aes(x = snap, y = n, fill = snap)) +
  geom_col() +
  labs(
    title = "Proportion of Households Receiving SNAP",
    x = "SNAP Household",
    y = "Count"
  ) +
  theme_minimal()

##Most households dont recieve SNAP benefits.

## If the hh was on snap during the first visit
hh_data %>%
  group_by(year = year(first_visit)) %>%
  summarize(count = sum(snap_first_visit, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(year), y = count)) +  # Convert year to factor for proper labeling
  geom_col(fill = "steelblue") +
  labs(title = "Number of Households on SNAP During their First Visit per Year",
       x = "Year",
       y = "Number of Households") +
  theme_minimal()

##### Proportion of HH on snap during their first visit
hh_data %>%
  mutate(year = year(first_visit)) %>%
  filter(!year %in% c(2018, 2019, 2024)) %>%  # Remove unwanted years
  group_by(year) %>%
  summarize(
    count = sum(snap_first_visit, na.rm = TRUE),  # Total SNAP households
    total_households = n()  # Total households per year
  ) %>%
  mutate(proportion = count / total_households) %>%  # Calculate proportion
  ggplot(aes(x = factor(year), y = proportion)) +  # Convert year to factor for proper labeling
  geom_col(fill = "steelblue") +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)), vjust = -0.5) +  # Show percentage labels
  labs(title = "Proportion of Households on SNAP During First Visit per Year",
       x = "Year",
       y = "Proportion of Households") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +  # Format y-axis as percentages
  theme_minimal()
#Edited version
hh_data %>%
  mutate(year = year(first_visit),
         snap_status = ifelse(snap_first_visit == 1, "SNAP", "Non-SNAP")) %>%
  filter(!year %in% c(2018, 2019, 2024)) %>%  # Remove unwanted years
  group_by(year, snap_status) %>%
  summarize(total = n(), .groups = "drop") %>%
  ggplot(aes(x = factor(year), y = total, fill = snap_status)) +  # Stacked bars
  geom_col(color = "black") +  # Black outlines for contrast
  scale_fill_manual(values = c("SNAP" = "#E69F00", "Non-SNAP" = "#0072B2")) +  # Colorblind-friendly colors (orange & blue)
  geom_text(aes(label = total), position = position_stack(vjust = 0.5), color = "white", fontface = "bold") +  # Labels inside bars
  labs(title = "New Households on SNAP vs Non-SNAP by year",
       x = "Year",
       y = "Total Households",
       fill = "Household Type") +
  theme_minimal() 






