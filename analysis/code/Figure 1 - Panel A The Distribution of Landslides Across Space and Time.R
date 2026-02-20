# Load required libraries
library(tidyverse)       # For data manipulation and visualization
library(ggplot2)         # For creating plots
library(geobr)           # For accessing Brazilian geospatial data
library(sf)              # For handling spatial data
library(ggspatial)       # For adding spatial elements to ggplot2
library(tmap)            # For thematic maps

# Set the paths for input and output files
path_input <- paste0(DROPBOX_PATH, "/build/input/")              # Input path
path_output <- paste0(DROPBOX_PATH, "/build/output/")            # Output path
path_output_github <- paste0(GITHUB_PATH, "/analysis/output/")   # GitHub output path

#### Open Databases ####

# Load datasets from the specified file paths
dados <- readRDS(paste0(path_output, "database_panel.rds"))              # Main dataset
psm   <- readRDS(paste0(path_output, "restricted_PSM_database.rds"))     # PSM dataset

# Merge the main dataset with the PSM dataset using the "code" column
psm <- merge(dados, psm[, c("code", "weights")], by = "code")            # Add weights

# Load municipality and state geospatial data for the year 2020
mun <- geobr::read_municipality(year = 2020, simplified = FALSE, showProgress = FALSE) %>%
  dplyr::select(code = code_muni)

uf <- geobr::read_state(year = 2020, simplified = TRUE, showProgress = FALSE)

# Prepare data for mapping affected municipalities
df <- dados %>% dplyr::select(code, year, first_year_landslide)

df <- df %>%
  # Collapse to one row per municipality
  group_by(code) %>%
  dplyr::summarise(first_year_landslide = max(first_year_landslide, na.rm = TRUE), .groups = "drop") %>%
  left_join(mun, by = "code") %>%
  st_as_sf()

# Ensure geometries are valid (works for both tmap v3 and v4)
df <- sf::st_make_valid(df)
uf <- sf::st_make_valid(uf)

# Calculate centroids of the municipalities for better visualization
df <- cbind(df, st_coordinates(st_centroid(df$geom)))

# Categorize the first year of landslide occurrences into intervals
df$interval <- ifelse(between(df$first_year_landslide, 2001, 2005), "2001 - 2005",
                      ifelse(between(df$first_year_landslide, 2006, 2010), "2006 - 2010",
                             ifelse(between(df$first_year_landslide, 2011, 2015), "2011 - 2015",
                                    ifelse(between(df$first_year_landslide, 2016, 2020), "2016 - 2020", ""))))

# Filter out rows with empty intervals
df <- dplyr::filter(df, interval != "")

# Set tmap mode to "plot" for static maps
tmap_mode("plot")

# Create the thematic map (all points in gray, no legend)
plot <- tm_shape(df) +
  tm_bubbles(col = "grey55", scale = 0.5, border.col = "white") +
  tm_shape(uf) +
  tm_borders(col = "grey40", lwd = 1) +
  tm_layout(
    inner.margins = c(0.15, 0.15, 0.15, 0.1),
    legend.show = FALSE,
    frame = TRUE,
    bg.color = "white"
  ) +
  tmap_style("white")

# Save the map to a file
tmap_save(
  plot,
  file = paste0(path_output_github, "graph_affected_disaster_municipalities_points.png"),
  dpi = 500, height = 14, width = 17, units = "cm"
)
