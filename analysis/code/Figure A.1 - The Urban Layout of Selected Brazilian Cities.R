# Load necessary packages
library(dplyr)       # Data manipulation
library(sf)          # Spatial data
library(terra)       # Raster data manipulation
library(ggplot2)     # Plot creation
library(furrr)       # Parallel function execution

# Define paths for input and output files
path_input <- paste0(DROPBOX_PATH, "/build/input/")      # Path for input files
path_output <- paste0(DROPBOX_PATH, "/build/output/")    # Path for output files
path_output_git <- paste0(GITHUB_PATH, "/analysis/output/")  # Path for output files on GitHub

# Function to generate example maps
fun <- function(code){
  
  # Load state data and select necessary columns
  name_state = as.data.frame(geobr::read_state()) %>% select(c(name_state, abbrev_state))
  
  # Load municipality data, filter by municipality code, join with state data, and transform CRS
  mun <- geobr::read_municipality() %>% 
    filter(code_muni %in% c(code)) %>% 
    left_join(name_state) %>% 
    mutate(name_muni = paste0(name_muni, " (", abbrev_state, ")")) %>% 
    st_transform(32723)  # Transform to UTM zone 23S CRS (EPSG:32723)
  
  # Manipulate state names to match raster file names
  name_state = name_state %>% filter(abbrev_state %in% mun$abbrev_state) %>% 
    mutate(name_state = gsub(' ', '', tolower(stringi::stri_trans_general(str = name_state, id = "Latin-ASCII"))))
  name_state = mutate(name_state, name_state = ifelse(name_state == 'amapa', 'amap', name_state))
  name_state = name_state$name_state
  
  # Read raster file
  raster <- "./MAPBIOMAS-EXPORT/"
  temp <- terra::rast(paste0(raster, "mapbiomas-brazil-collection-80-", name_state, "-", 2010, ".tif"))
  
  # Transform values that are not urban areas (24) to zero
  temp[temp != 24] <- 0
  
  # Transform the raster CRS to the desired system (EPSG:32723)
  temp <- project(temp, "EPSG:32723")
  
  # Apply municipality mask to the raster
  temp <- mask(temp, mun)
  
  # Define title based on the municipality
  title <- ifelse(mun$name_muni == 'Belo Horizonte (MG)', 'Urban size: 26,118 (ha)\nFragmentation index: 0.19',
                  ifelse(mun$name_muni == 'Brasília (DF)', 'Urban size: 52,631 (ha)\nFragmentation index: 0.58',
                         ifelse(mun$name_muni == 'Osasco (SP)', 'Urban size: 5,628 (ha)\nFragmentation index: 0.14',
                                ifelse(mun$name_muni == 'Blumenau (SC)', 'Urban size: 6,055 (ha)\nFragmentation index: 0.75', NA))))
  
  # Create the map using ggplot2
  map_mapbiomas_2010 <- ggplot() +
    geom_raster(data = as.data.frame(temp, xy = TRUE), 
                aes(x = x, y = y, fill = classification_2010)) + 
    scale_fill_gradient2(low = 'white', high = 'black') +
    geom_sf(data = mun, fill = NA, color = 'black') +
    labs(title = title) + 
    theme_classic() +
    theme(legend.position = 'none',
          plot.title = element_text(size = 25),
          axis.title = element_blank(), 
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank()) 
  
  # Memory cleanup
  gc()
  
  # Save the generated map
  ggsave(paste0('_graph_', gsub(' |\\(|\\)', '_', tolower(stringi::stri_trans_general(str = mun$name_muni, id = "Latin-ASCII"))), ".png"), 
         map_mapbiomas_2010, width = 10, height = 10, units = "in", dpi = 300)
}

# Apply the function in parallel to generate maps for multiple municipalities
future::plan("multisession")  # Set parallel execution plan
furrr::future_map(.x = c(5300108, 3106200, 4202404, 3534401),  # Municipality codes
                  .f = fun,  # Function to be applied
                  .progress = TRUE)  # Show progress
