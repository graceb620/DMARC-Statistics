
rm(list = ls())

#install.packages(c("ggplot2", "dplyr", "sf", "tigris"))
library(ggplot2)
library(dplyr)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE) 

hh_first_visit_2023<-read.csv('Data/hh_first23.csv', stringsAsFactors=FALSE)

#Load ZIP code boundaries for Dsm
zip_counts <- hh_first_visit_2023 %>%
  count(first_visit_zip) %>%
  rename(ZCTA5CE10 = first_visit_zip)  #match column name in shape


#Get Iowa ZIP code boundaries 
zip_shapes <- zctas(cb = FALSE, state = "IA", year = 2010)

#merge ZIP code shapes with visitor count data
zip_map_data <- zip_shapes %>%
  inner_join(zip_counts, by = "ZCTA5CE10")

zip_towns <- data.frame(
  ZCTA5CE10 = c("50010", "50014", "50309"),  # Example ZIP codes
  town_name = c("Ames", "Ames", "Des Moines")  # Corresponding town names
)

# Add town names to the map data
zip_map_data <- zip_map_data %>%
  left_join(zip_towns, by = "ZCTA5CE10")  # Add town names



# Transform to a projected coordinate system for better visualization
zip_map_data <- st_transform(zip_map_data, crs = 3857)

# Get Iowa state boundary
iowa_state <- states(cb = TRUE) %>% 
  filter(STUSPS == "IA")


# Plot the map without grid and latitude/longitude labels
ggplot(data = zip_map_data) +
  geom_sf(data = iowa_state, fill = "gray95", color = "black", size = 0.3) +  
  geom_sf(aes(fill = n), color = "white", size = 0.2) +  # Fill ZIP based on count
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Visitors") +  # Gradient fill
  theme_minimal() +  # Minimal theme for a clean look
  ggtitle("Map of First-Time Visitors of 2023 in Iowa") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Center and enlarge title
    legend.position = "right",  # Place legend on the right
    panel.grid = element_blank(),  # Remove grid lines
    axis.text = element_blank(),  # Remove latitude/longitude labels
    axis.ticks = element_blank(),  # Remove axis ticks
    axis.title = element_blank()  # Remove axis titles
  )
