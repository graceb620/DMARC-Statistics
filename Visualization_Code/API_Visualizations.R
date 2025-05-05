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

## --- Chlorpleth Maps --------------------------------------------------------
#for chloropleth map
library(sf)
library(ggplot2)
library(dplyr)
library(tigris)   # for fetching shapefiles
options(tigris_use_cache = TRUE)

# Get 2020 national ZCTAs (ZIP Code Tabulation Areas)
zips_2020 <- zctas(cb = TRUE, year = 2020)

# Re-download states with correct format
iowa_state <- tigris::states(cb = TRUE, year = 2020)

# Make sure it's an sf object, then filter for Iowa
iowa_state <- iowa_state[iowa_state$STUSPS == "IA", ]

zips_2020 <- zctas(cb = TRUE, year = 2020)
iowa_zips <- st_filter(zips_2020, iowa_state)

zip_data <- data.frame(
  ZCTA5CE20 = API_2023$zip_code,
  value = API_2023$nonSNAPhh
)

zip_data <- zip_data %>%
  mutate(ZCTA5CE20 = as.character(ZCTA5CE20))

iowa_zips_merged <- iowa_zips %>%
  left_join(zip_data, by = "ZCTA5CE20")

ggplot(iowa_zips_merged) +
  geom_sf(aes(fill = value), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = "Density of Non-SNAP Households in Iowa",
    fill = "Non-SNAP\nHouseholds"
  )

### --- TEST ------------------------------------------------------------------
library(sf)
library(dplyr)
library(tigris)
library(leaflet)

options(tigris_use_cache = TRUE)

# Load the shapefiles
zips_2020 <- zctas(cb = TRUE, year = 2020)
iowa_state <- states(cb = TRUE, year = 2020)
iowa_state <- iowa_state[iowa_state$STUSPS == "IA", ]
iowa_zips <- st_filter(zips_2020, iowa_state)

# Merge your data
zip_data <- data.frame(
  ZCTA5CE20 = API_2023$zip_code,
  value = API_2023$nonSNAPhh
)
zip_data <- zip_data %>% mutate(ZCTA5CE20 = as.character(ZCTA5CE20))
iowa_zips_merged <- left_join(iowa_zips, zip_data, by = "ZCTA5CE20")

# Create color palette
pal <- colorNumeric(palette = "plasma", domain = iowa_zips_merged$value, na.color = "grey90")

# Make the leaflet map
leaflet(data = iowa_zips_merged) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal(value),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    popup = ~paste0("ZIP: ", ZCTA5CE20, "<br>Value: ", value),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) %>%
  addLegend("bottomright", pal = pal, values = ~value,
            title = "Non-SNAP Households")

