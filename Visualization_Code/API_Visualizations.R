### --- API Visualizations ----------------------------------------------------
# Primary Authors: Grace Bero
# 
# This was used to create visualizations for the API data

library(tidyr)
library(ggplot2)
library(dplyr)
library(scales)

###----------- Load in the Datasets --------------------------------------------
API_data <- read.csv("Data/API_data.csv")
API_2023 <- read.csv("Data/API_2023.csv")
zip_codes <- read.csv("Data/zip_codes.csv")

# Narrow the entries to only zipcodes that are included in original dataset. 
API_data <- API_data %>% 
  filter(zip_code %in% zip_codes$zip)

API_2023 <- API_2023 %>% 
  filter(zip_code %in% zip_codes$zip) %>% 
  mutate(
    nonSNAPhh = NumHH - SnapHH
  )
# This brought the number of observations from 933 to 896

# --- Creating bar graph -----------------------------------------------
# Summarize and transform your data
API_df <- API_data %>%
  group_by(year) %>%
  summarize(
    total_households = sum(NumHH, na.rm = TRUE),
    total_snap_households = sum(SnapHH, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(non_snap_households = total_households - total_snap_households) %>%
  select(year, SNAP = total_snap_households, `Non-SNAP` = non_snap_households) %>%
  pivot_longer(cols = c("SNAP", "Non-SNAP"), names_to = "snap_status", values_to = "total") %>%
  filter(!year %in% c(2018, 2019, 2024)) %>%
  group_by(year) %>%
  mutate(
    proportion = total / sum(total),
    label_text = paste0(total, "\n(", percent(proportion, accuracy = 1), ")")
  )

# Plot
ggplot(API_df, aes(x = factor(year), y = total, fill = snap_status)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("SNAP" = "#D55E00", "Non-SNAP" = "#0072B2")) +
  geom_text(
    aes(label = label_text),
    position = position_stack(vjust = 0.5),
    color = "white", fontface = "bold", size = 3
  ) +
  labs(
    title = "Households on SNAP vs. Non-SNAP per Year",
    subtitle = "A breakdown of household participation in SNAP over time",
    x = "Year",
    y = "Total Households",
    fill = "Household Type"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

