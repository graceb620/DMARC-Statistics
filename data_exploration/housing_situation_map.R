

rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(tigris)


hh_first_visit_2023<-read.csv('Data/hh_first23.csv', stringsAsFactors=FALSE)

# Step 1: Get count of each housing type by ZIP
housing_counts <- hh_first_visit_2023 %>%
  group_by(first_visit_zip, first_housing_type) %>%
  summarise(count = n(), .groups = "drop")

# Step 2: Calculate total per ZIP for proportions
housing_props <- housing_counts %>%
  group_by(first_visit_zip) %>%
  mutate(total = sum(count),
         prop = count / total) %>%
  ungroup() %>%
  rename(ZCTA5CE10 = first_visit_zip,
         housing_type = first_housing_type)

# Step 3: Get ZIP shapes and filter to Des Moines region
zip_shapes <- zctas(cb = FALSE, state = "IA", year = 2010)
des_moines_counties <- counties(state = "IA", cb = TRUE, year = 2020) %>%
  filter(NAME %in% c("Polk", "Dallas", "Warren", "Jasper", "Madison"))
des_moines_counties <- st_transform(des_moines_counties, crs = st_crs(zip_shapes))

# Optional: Clip ZIPs to Des Moines counties
zip_shapes <- st_transform(zip_shapes, crs = st_crs(des_moines_counties))
zip_shapes <- st_intersection(zip_shapes, des_moines_counties)

# Step 4: Join proportions to ZIP shapes
zip_housing_map <- zip_shapes %>%
  inner_join(housing_props, by = "ZCTA5CE10")

# Step 5: Plot faceted map
ggplot(zip_housing_map) +
  geom_sf(aes(fill = prop), color = "white", size = 0.2) +
  scale_fill_gradient(low = "lightyellow", high = "red", name = "Proportion") +
  facet_wrap(~ housing_type) +
  theme_minimal() +
  ggtitle("Housing Situations by ZIP Code area") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    strip.text = element_text(size = 12),
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )




