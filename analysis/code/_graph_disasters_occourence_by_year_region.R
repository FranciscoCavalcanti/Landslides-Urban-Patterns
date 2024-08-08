# Load necessary packages
library(tidyverse)   # Data manipulation and visualization
library(ggplot2)     # Plot creation
library(geobr)       # Geospatial data for Brazil
library(sf)          # Spatial data manipulation
library(ggspatial)   # Spatial data visualization

# Set the path for the output file
path_output <- paste0(DROPBOX_PATH, "/build/output/")  # Path for output files
path_output_github <- paste0(GITHUB_PATH, "/analysis/output/")  # Path for GitHub output files

#### Open Databases ####
dados <- readRDS(paste0(path_output, "database_panel.rds"))  # Load dataset

# Create a dataframe for plotting the number of disasters by year
df <- dados %>% 
  filter(year == first_year_landslide | first_year_landslide == 0) %>%  # Filter rows where the year is the first year of landslide or no landslide
  mutate(first_year_landslide = ifelse(first_year_landslide == 0, 0, 1)) %>%  # Convert first year landslide to binary
  group_by(year) %>%  # Group data by year
  dplyr::summarise(value = sum(first_year_landslide, na.rm = TRUE))  # Summarize the number of landslides per year

# Add a label column for the plot
df$label = ifelse(df$value == 0, '', df$value)  # If the value is 0, label is empty, otherwise use the value

# Create the bar plot
ggplot(data = df, aes(x = as.character(year), y = value)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +  # Create bar plot with black borders
  geom_text(aes(label = label), vjust = -0.5, hjust = 0.5, color = "black", position = position_dodge(width = 1), size = 4) +  # Add text labels above the bars
  theme_bw() +  # Use a white background theme
  scale_y_continuous(limits = c(0, max(df$value * 1.05)), expand = c(0, 0)) +  # Adjust y-axis limits
  labs(x = 'Year', y = 'Count', fill = "", title = "") +  # Add labels for axes
  theme(panel.grid.major.x = element_blank(),  # Remove major x-axis gridlines
        panel.grid.minor.x = element_blank(),  # Remove minor x-axis gridlines
        legend.position = "bottom",  # Position legend at the bottom
        text = element_text(size = 12),  # Global text size adjustment
        axis.text.x = element_text(size = 12),  # x-axis text size
        axis.text.y = element_text(size = 12),  # y-axis text size
        legend.title = element_text(size = 12),  # Legend title text size
        legend.text = element_text(size = 12))  # Legend text size

# Save the plot to a file
ggsave(filename = paste0(path_output_github, '_graph_disasters_occourence_by_year_region.png'), dpi = 300, width = 30, height = 15, units = 'cm')
