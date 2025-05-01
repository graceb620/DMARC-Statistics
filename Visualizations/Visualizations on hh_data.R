library(ggplot2)
library(dplyr)
library(lubridate)

hh_data_2023<-read.csv('Data/hh_data23.csv', stringsAsFactors=FALSE)
hh_data <- read.csv('Data/hh_data.csv', stringsAsFactors = FALSE)

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
  count(snap_household) %>%
  ggplot(aes(x = snap_household, y = n, fill = snap_household)) +
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
  mutate(
    year = year(first_visit),
    snap_status = ifelse(snap_first_visit == 1, "SNAP", "Non-SNAP")
  ) %>%
  filter(!year %in% c(2018, 2019, 2024)) %>%
  group_by(year, snap_status) %>%
  summarize(total = n(), .groups = "drop") %>%
  group_by(year) %>%
  mutate(
    proportion = total / sum(total),
    label_text = paste0(total, "\n(", scales::percent(proportion, accuracy = 1), ")")
  ) %>%
  ggplot(aes(x = factor(year), y = total, fill = snap_status)) +
  geom_col(color = "black") +
  scale_fill_manual(
    values = c("SNAP" = "#D55E00", "Non-SNAP" = "#0072B2")
  ) +
  geom_text(
    aes(label = label_text),
    position = position_stack(vjust = 0.5),
    color = "white", fontface = "bold", size = 4
  ) +
  labs(
    title = "First Time Household Visits to DMARC by Year",
    subtitle = "Focused on if they were receiving SNAP benefits during the first visit",
    x = "Year",
    y = "Total Households",
    fill = "Household Type"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

