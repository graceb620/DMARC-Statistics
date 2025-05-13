
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


# Transform to a projected coordinate system for better visualization
zip_map_data <- st_transform(zip_map_data, crs = 3857)

# Calculate the bounding box around the ZIPs with visitors
bbox <- st_bbox(zip_map_data)


# Get Iowa state boundary
iowa_state <- states(cb = TRUE) %>% 
  filter(STUSPS == "IA")

# Calculate bounding box and center
bbox <- st_bbox(zip_map_data)
center_x <- (bbox$xmin + bbox$xmax) / 2
center_y <- (bbox$ymin + bbox$ymax) / 2

# Define a fixed zoom width/height — adjust these to control how tight the zoom is
zoom_width <- (bbox$xmax - bbox$xmin) * 0.4  # 40% of full width
zoom_height <- (bbox$ymax - bbox$ymin) * 0.4  # 40% of full height

# Define new bounding box around center
xlim <- c(center_x - zoom_width/2, center_x + zoom_width/2)
ylim <- c(center_y - zoom_height/2, center_y + zoom_height/2)

# Load Iowa counties
iowa_counties <- counties(state = "IA", cb = TRUE, year = 2020)
iowa_counties <- st_transform(iowa_counties, crs = st_crs(zip_map_data))  # match projection


# Get centroids for placing text
county_centroids <- st_centroid(iowa_counties)
##########better plot
ggplot(data = zip_map_data) +
  geom_sf(data = iowa_state, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(aes(fill = n), color = "white", size = 0.2) +
  geom_sf_text(data = county_centroids, aes(label = NAME), size = 3.0, color = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Visitors") +
  coord_sf(xlim = xlim, ylim = ylim) +
  theme_minimal() +
  ggtitle("Map of First-Time Visitors of 2023 in different counties") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

summary(hh_first_visit_2023)





#################
library(ggplot2)
library(dplyr)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)

hh_first_visit_2023 <- read.csv('Data/hh_first23.csv', stringsAsFactors = FALSE)

zip_counts <- hh_first_visit_2023 %>%
  count(first_visit_zip) %>%
  rename(ZCTA5CE10 = first_visit_zip)

zip_shapes <- zctas(cb = FALSE, state = "IA", year = 2010)

zip_map_data <- zip_shapes %>%
  inner_join(zip_counts, by = "ZCTA5CE10")

zip_map_data <- st_transform(zip_map_data, crs = 3857)

# Calculate bounding box around visitor ZIPs
bbox <- st_bbox(zip_map_data)
center_x <- (bbox$xmin + bbox$xmax) / 2
center_y <- (bbox$ymin + bbox$ymax) / 2

zoom_width <- (bbox$xmax - bbox$xmin) * 0.6  # little wider than before
zoom_height <- (bbox$ymax - bbox$ymin) * 0.6

xlim <- c(center_x - zoom_width/2, center_x + zoom_width/2)
ylim <- c(center_y - zoom_height/2, center_y + zoom_height/2)

# Generate labels for each visitor ZIP (you could attach city names manually if you want)
zip_labels <- zip_map_data %>%
  mutate(label = ZCTA5CE10)  # or join with a ZIP-to-city lookup if you have it

# Get centroids for ZIP labels
zip_centroids <- st_centroid(zip_map_data)

# Plot
ggplot(data = zip_map_data) +
  geom_sf(data = iowa_state, fill = "gray95", color = "black", size = 0.3) +
  geom_sf(aes(fill = n), color = "white", size = 0.2) +
  geom_sf_text(data = zip_centroids, aes(label = ZCTA5CE10), size = 5, color = "black", fontface = "bold") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Visitors") +
  coord_sf(xlim = xlim, ylim = ylim) +
  theme_minimal() +
  ggtitle("First-Time Visitors by ZIP Code - 2023") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
