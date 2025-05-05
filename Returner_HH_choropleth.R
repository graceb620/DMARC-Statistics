library(sf)
library(dplyr)
library(tigris)
library(leaflet)

options(tigris_use_cache = TRUE)
### --- Making the Datasets ---------------------------------------------------
hh_returner_23 <- read.csv("Data/hh_data23.csv")

hh_returner_23 <- hh_returner_23 %>%
  filter(first_visit_2023 == 0) %>% 
  group_by(zip) %>%
  summarize(
    count = n()
  )

### -- Making the map ----------------------------------------------------------

# Load the shapefiles
zips_2020 <- zctas(cb = TRUE, year = 2020)
iowa_state <- states(cb = TRUE, year = 2020)
iowa_state <- iowa_state[iowa_state$STUSPS == "IA", ]
iowa_zips <- st_filter(zips_2020, iowa_state)

# Merge your data
zip_data <- data.frame(
  ZCTA5CE20 = hh_returner_23$zip,
  value = hh_returner_23$count
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
            title = "Returning Households\n2023")
