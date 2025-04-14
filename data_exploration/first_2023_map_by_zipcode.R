
rm(list = ls())

#install.packages(c("ggplot2", "dplyr", "sf", "tigris"))
library(ggplot2)
library(dplyr)
library(sf)
library(tigris)

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

# Make the map plot
ggplot(data = zip_map_data) +
  geom_sf(aes(fill = n), color = "black") +  # Fill ZIP based on count
  geom_sf_text(aes(label = town_name), size = 3, color = "black") +  # Add town names
  scale_fill_gradient(low = "yellow", high = "red", name = "Visitors") +
  theme_minimal() + 
  ggtitle("Map of First-Time Visitors of 2023 in Iowa")

# Transform to a projected coordinate system for better visualization
zip_map_data <- st_transform(zip_map_data, crs = 3857)

# Plot the map
ggplot(data = zip_map_data) +
  geom_sf(aes(fill = n), color = "white", size = 0.2) +  # Fill ZIP based on count
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Visitors") +  # Gradient fill
  theme_minimal() +  # Minimal theme for a clean look
  ggtitle("Map of First-Time Visitors of 2023 in Iowa") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Center and enlarge title
    legend.position = "right"  # Place legend on the right
  )

# Plot the map without grid and latitude/longitude labels
ggplot(data = zip_map_data) +
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







# Get Iowa county boundaries
county_shapes <- counties(state = "IA", cb = TRUE)

# Extract centroids for labeling counties
county_centroids <- st_centroid(county_shapes)

# Transform ZIP code data and county data to the same CRS for alignment
zip_map_data <- st_transform(zip_map_data, crs = st_crs(county_shapes))
county_shapes <- st_transform(county_shapes, crs = st_crs(county_shapes))

# Plot the map
ggplot() +
  # Add the outer boundary of Iowa (no internal county lines)
  geom_sf(data = st_union(county_shapes), fill = NA, color = "black", size = 0.5) +
  # Add ZIP code data
  geom_sf(data = zip_map_data, aes(fill = n), color = "white", size = 0.2) +
  # Customize fill gradient for ZIP code data
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Visitors") +
  # Add title and theme
  ggtitle("Map of First-Time Visitors of 2023 in Iowa") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Center and enlarge title
    legend.position = "right",  # Place legend on the right
    panel.grid = element_blank(),  # Remove grid lines
    axis.text = element_blank(),  # Remove latitude/longitude labels
    axis.ticks = element_blank(),  # Remove axis ticks
    axis.title = element_blank()  # Remove axis titles
  )





# Filter counties for the areas of interest
selected_counties <- county_shapes %>%
  filter(NAME %in% c("Polk", "Dallas", "Warren"))  # Counties containing Des Moines, Johnston, etc.

# Filter ZIP codes for the areas of interest (if needed)
selected_zip_map_data <- zip_map_data %>%
  filter(ZCTA5CE10 %in% c("50309", "50310", "50322", "50265", "50023"))  # Example ZIP codes

# Add city coordinates for labeling
city_labels <- data.frame(
  city = c("Des Moines", "Johnston", "Urbandale", "West Des Moines", "Ankeny"),
  lon = c(-93.6091, -93.6977, -93.7122, -93.7356, -93.6001),  # Longitudes
  lat = c(41.6005, 41.6734, 41.6267, 41.5772, 41.7318)        # Latitudes
)

# Plot the map
ggplot() +
  # Add the full Iowa boundary for context
  geom_sf(data = county_shapes, fill = NA, color = "black", size = 0.5) +
  # Highlight the selected counties with their boundaries
  geom_sf(data = selected_counties, fill = NA, color = "red", size = 1) +
  # Add ZIP code data for the selected areas
  geom_sf(data = selected_zip_map_data, aes(fill = n), color = "white", size = 0.2) +
  # Add city labels
  geom_point(data = city_labels, aes(x = lon, y = lat), color = "red", size = 2) +
  geom_text(data = city_labels, aes(x = lon, y = lat, label = city), size = 3, hjust = 0.5, vjust = -1) +
  # Customize fill gradient for ZIP code data
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Visitors") +
  # Keep the full Iowa shape while zooming into the selected area
  coord_sf(default_crs = st_crs(county_shapes)) +
  # Add title and theme
  ggtitle("Map of First-Time Visitors in Selected Areas of Iowa") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Center and enlarge title
    legend.position = "right",  # Place legend on the right
    panel.grid = element_blank(),  # Remove grid lines
    axis.text = element_blank(),  # Remove latitude/longitude labels
    axis.ticks = element_blank(),  # Remove axis ticks
    axis.title = element_blank()  # Remove axis titles
  )



# Plot the map
ggplot() +
  # Add the outer boundary of Iowa (no internal grid lines)
  geom_sf(data = st_union(county_shapes), fill = NA, color = "black", size = 0.5) +
  # Add the internal grid lines for the selected cities' ZIP codes
  geom_sf(data = selected_zip_map_data, aes(fill = n), color = "black", size = 0.5) +
  # Add city labels
  geom_point(data = city_labels, aes(x = lon, y = lat), color = "red", size = 2) +
  geom_text(data = city_labels, aes(x = lon, y = lat, label = city), size = 3, hjust = 0.5, vjust = -1) +
  # Customize fill gradient for ZIP code data
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Visitors") +
  # Keep the full Iowa shape while zooming into the selected area
  coord_sf(xlim = c(-94, -93.4), ylim = c(41.4, 41.8)) +  # Adjust limits to focus on the area
  # Add title and theme
  ggtitle("Map of First-Time Visitors in Selected Cities of Iowa") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),  # Center and enlarge title
    legend.position = "right",  # Place legend on the right
    panel.grid = element_blank(),  # Remove grid lines
    axis.text = element_blank(),  # Remove latitude/longitude labels
    axis.ticks = element_blank(),  # Remove axis ticks
    axis.title = element_blank()  # Remove axis titles
  )



