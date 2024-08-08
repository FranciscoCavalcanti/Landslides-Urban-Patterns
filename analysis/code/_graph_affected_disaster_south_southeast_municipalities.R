# Load required libraries
library(tidyverse)       # For data manipulation and visualization
library(ggplot2)         # For creating plots
library(geobr)           # For accessing Brazilian geospatial data
library(sf)              # For handling spatial data
library(ggspatial)       # For adding spatial elements to ggplot2
library(tmap)            # For thematic maps

# Set the paths for input and output files
path_output <- paste0(DROPBOX_PATH, "/build/output/") 
path_output_github <- paste0(GITHUB_PATH, "/analysis/output/") 

#### Open Databases ####

# Load datasets from the specified file paths
dados <- readRDS(paste0(path_output, "database_panel.rds"))
psm <- readRDS(paste0(path_output, "restricted_PSM_database.rds"))

# Merge the main dataset with the PSM dataset using the "code" column
psm <- merge(dados, psm[, c("code", "weights")], by = "code")

# Load municipality and state geospatial data for the year 2020
mun <- geobr::read_municipality(year = 2020, simplified = FALSE) %>% select(c(code = code_muni))
uf <- geobr::read_state()

# Prepare data for mapping affected municipalities
df <- dados %>% select(c(code, year, first_year_landslide))

df <- df %>% 
  # Filter municipalities based on the first digit of their code (only considering those starting with '3' or '4')
  filter(substr(code, 1, 1) %in% c('3', '4')) %>% 
  group_by(code) %>% 
  # Get the maximum year of the first landslide occurrence for each municipality
  dplyr::summarise(first_year_landslide = max(first_year_landslide)) %>% 
  # Merge with municipality data
  left_join(mun) %>% 
  # Convert to an sf object for spatial operations
  st_as_sf()

# Calculate centroids of the municipalities for better visualization
df <- cbind(df, st_coordinates(st_centroid(df$geom)))

# Categorize the first year of landslide occurrences into intervals
df$interval <- ifelse(between(df$first_year_landslide, 2001, 2005), '2001 - 2005',
               ifelse(between(df$first_year_landslide, 2006, 2010), '2006 - 2010',
               ifelse(between(df$first_year_landslide, 2011, 2015), '2011 - 2015',
               ifelse(between(df$first_year_landslide, 2016, 2020), '2016 - 2020', ''))))

# Filter out rows with empty intervals
df <- filter(df, interval != '')

# Set tmap mode to "plot" for static maps
tmap_mode("plot")

# Ensure that all geometries are valid
tmap_options(check.and.fix = TRUE)

# Create the thematic map
plot <- tm_shape(df) +
  tm_polygons(col = "interval", scale = 0.5, n = 5, border.col = "white", 
              palette = "Spectral", title = "Year of the First Landslide Occurrence") +
  tm_shape(uf) +
  tm_borders(col = "grey40", lwd = 1) +
  tm_layout(inner.margins = c(0.15, 0.15, 0.15, 0.1), legend.stack = "horizontal") +
  tmap_style("col_blind")

# Save the map to a file
tmap_save(plot, file = paste0(path_output_github, "_graph_affected_disaster_south_southeast_municipalities.png"), 
          dpi = 500, height = 14, width = 17, units = "cm")
