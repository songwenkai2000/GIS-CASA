# Load necessary packages
install.packages(c("dplyr", "sf", "ggplot2", "countrycode", "usethis"))
library(dplyr)
library(sf)
library(ggplot2)
library(countrycode)
library(usethis)

# Step 1: Read world spatial data (GeoJSON format)
world_geo <- st_read("World_Countries_(Generalized)_9029012925078512962.geojson")

# Step 2: Convert ISO 2-letter codes in world_geo to 3-letter codes
world_geo <- world_geo %>%
  mutate(ISO3 = countrycode(ISO, origin = 'iso2c', destination = 'iso3c'))

# Step 3: Read gender inequality data for 2010 and 2019, ensuring countryIsoCode is uppercase
data_2010 <- read.csv("gender_inequality_2010.csv") %>%
  rename(gender_inequality_index_2010 = value) %>%
  mutate(countryIsoCode = toupper(as.character(countryIsoCode)))

data_2019 <- read.csv("gender_inequality_2019.csv") %>%
  rename(gender_inequality_index_2019 = value) %>%
  mutate(countryIsoCode = toupper(as.character(countryIsoCode)))

# Step 4: Merge 2010 and 2019 data and calculate the inequality difference
gender_inequality_diff <- data_2010 %>%
  inner_join(data_2019, by = "countryIsoCode") %>%
  mutate(difference = gender_inequality_index_2019 - gender_inequality_index_2010)

# Step 5: Merge world_geo with gender inequality data using ISO3 codes
world_data <- world_geo %>%
  left_join(gender_inequality_diff, by = c("ISO3" = "countryIsoCode"))

# Step 6: Create and save the map
map_plot <- ggplot(world_data) +
  geom_sf(aes(fill = difference), color = "white", size = 0.2) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, 
                       name = "Gender Inequality Difference\n(2019 - 2010)") +
  theme_minimal() +
  labs(title = "Global Gender Inequality Index Difference (2019 - 2010)",
       caption = "Data source: Your data source here") +
  theme(legend.position = "bottom")

print(map_plot)

# Save the map as a PNG file
ggsave("gender_inequality_map.png", plot = map_plot, width = 10, height = 6)

