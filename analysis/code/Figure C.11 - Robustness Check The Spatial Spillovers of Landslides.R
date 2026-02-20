# Load required libraries
library(dplyr)       # For data manipulation
library(fixest)      # For fixed effects estimation
library(did)         # For difference-in-differences estimation
library(remotes)     # For installing R packages from remote repositories
library(did2s)       # For alternative DiD estimators
library(staggered)   # For staggered DiD designs
library(ggplot2)     # For creating plots
library(gridExtra)   # For arranging multiple plots
library(broom)

# Set Seed
set.seed(123)

# Set the paths for input and output files
path_input <- paste0(DROPBOX_PATH, "/build/input/")
path_output <- paste0(DROPBOX_PATH, "/build/output/") 
path_output_git <- paste0(GITHUB_PATH, "/analysis/output/") 

#### Open Databases ####

# Load datasets from the specified file paths
dados <- readRDS(paste0(path_output, "database_panel.rds"))
psm <- readRDS(paste0(path_output, "restricted_PSM_database.rds"))

#### Selecting the Treated/Control Group Using PSM ####

# Merge the main dataset with the PSM dataset using the "code" column
dados2 <- merge(dados, psm[, c("code", "weights")], by = "code")

#### Robustness 1 - Sample removing Neighbour Municipalities affected by Landslides ####

# Function to create plots for robustness checks
ggplot_paper <- function(x){
  
  ## Paired results
  # Estimate ATT for the matched sample using the dynamic DiD approach
  mw.dyn_p <- aggte(
    att_gt(yname = x,
           gname = "first_year_landslide",
           idname = "code",
           bstrap = TRUE,
           clustervars = 'code',
           base_period="universal",
           tname = "year",
           data = dados2_robustness), type = "dynamic")
  
  ## Avg effect
  # Tidy up the results and select relevant columns
  est_p <- broom::tidy(mw.dyn_p) %>% select(c(event.time, estimate, std.error, conf.low, conf.high)) %>% 
    mutate(est = 'Matched Sample')
  
  # Combine both results into one data frame
  est <- est_p
  

  # Z values for different confidence levels
  z_99 <- 2.576
  z_95 <- 1.96
  z_90 <- 1.645
  

  # Check significance for the paired sample
  IC_99 <- mw.dyn_p$overall.att + c(-z_99 * mw.dyn_p$overall.se, z_99 * mw.dyn_p$overall.se)
  IC_95 <- mw.dyn_p$overall.att + c(-z_95 * mw.dyn_p$overall.se, z_95 * mw.dyn_p$overall.se)
  IC_90 <- mw.dyn_p$overall.att + c(-z_90 * mw.dyn_p$overall.se, z_90 * mw.dyn_p$overall.se)
  
  ATT_significance_p <- ifelse(all(IC_99 < 0) | all(IC_99 > 0), paste0(round(mw.dyn_p$overall.att, 4), "***"),
                               ifelse(all(IC_95 < 0) | all(IC_95 > 0), paste0(round(mw.dyn_p$overall.att, 4), "**"),
                                      ifelse(all(IC_90 < 0) | all(IC_90 > 0), paste0(round(mw.dyn_p$overall.att, 4), "*"),
                                             paste(round(mw.dyn_p$overall.att, 4)))))
  
  # Create the table as a grob (graphical object)
  dados_tabela <- data.table::data.table(
    `ATT` = c(ATT_significance_p, paste0("(", round(mw.dyn_p$overall.se, 4), ")"))
  )
  
  value = c(abs(0 - summary(est$conf.low)[1]), abs(0 - summary(est$conf.high)[6]))
  sequencia <- seq(min(est$conf.low, na.rm = T), max(est$conf.high, na.rm = T), length.out = 1000)
  # Calculate the quantiles at 10% and 75%
  quantil_10 <- quantile(sequencia, probs = 0.18)
  quantil_75 <- quantile(sequencia, probs = 0.91)
  
  table_pos_y = ifelse(value[1] < value[2], quantil_75, quantil_10)  
  lengend_pos_y = ifelse(value[1] < value[2], 0.75, 0.1)
  
  # Generate the table grob
  tabela_grob <- tableGrob(dados_tabela, 
                           rows = NULL, 
                           theme = ttheme_minimal(core = list(fg_params = list(fontsize = 30)), 
                                                  colhead = list(fg_params = list(fontsize = 30, fontface = "bold")), 
                                                  rowhead = list(fg_params = list(fontsize = 30)))) 
  
  # Create the plot
  graph <- ggplot(data = est, aes(y = event.time, x = estimate)) +
    geom_pointrange(
      aes(xmax = conf.high, xmin = conf.low),
      linewidth = 0.5, position = position_dodge(width = 0.5), linetype = 'blank'
    ) +
    geom_errorbar(
      aes(xmax = conf.high, xmin = conf.low),
      linewidth = 0.5, width = 0.5, position = position_dodge(width = 0.5)
    ) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = -1) +
    labs(x = 'Coefficient', y = 'Period',
         color = "", linetype = "",
         title = "") +
    # scale_color_manual(name = "", values = c("black", "grey20"),
    #                    labels = c('Broader Sample', 'Matched Sample')) +
    # scale_linetype_manual(name = "", values = c("solid", "dashed"),
    #                       labels = c('Broader Sample', 'Matched Sample')) + 
    scale_y_continuous(breaks = seq(-16, 16, by = 2)) +
    coord_flip() +
    theme_minimal() + 
    theme(text = element_text(size = 25),
          legend.text = element_text(size = 25),
          legend.title = element_text(size = 25),
          legend.key.width = unit(1.5, "cm"),
          legend.key.height = unit(1, "cm"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = c(0.15, lengend_pos_y))
  
  # Combine the plot and table
  graph <- graph + annotation_custom(grob=tabela_grob, 
                                     xmin=table_pos_y, 
                                     xmax=table_pos_y, 
                                     ymin=-15, ymax=-11)
  
  return(graph)
}

### Including first year a neighbor had a disaster ####
library(sf)          # Simple features (sf) for spatial operations
# Reading municipality geometries for the year 2020 with detailed geometry
mun_geom <- geobr::read_municipality(year = 2020, simplified = FALSE) %>%
  select(code = code_muni) %>%  # Select and rename the municipality code column
  left_join(dados %>% distinct(code,first_year_landslide))

# Calculate adjacency matrix (which municipalities are neighbors)
# st_touches identifies neighbors as those sharing a boundary
matriz_adj <- st_touches(mun_geom)

# Adding a unique ID to each row to facilitate referencing
# This ID is helpful for indexing and mapping operations later
mun_geom$id <- 1:nrow(mun_geom)

# Create a function to extract the minimum disaster year from neighbors
df_neighborhood <- function(id) {
  
  print(id)
  # Extract neighbor IDs from the adjacency matrix for the given municipality
  neighborhoods_ids <- matriz_adj[[id]]
  
  if(length(neighborhoods_ids) == 0){
    
  } else {
   
    # Filter the geometry data frame for neighbor municipalities
    neighborhoods_ids <- filter(mun_geom, id %in% neighborhoods_ids)
    
    # Filter the data frame with disaster data for these neighboring codes
    temp1 <- dados %>% distinct(code,first_year_landslide) %>% 
      filter(code %in% neighborhoods_ids$code)
    temp2 <- data.frame(mun_geom[id,]) %>% select(c(code,first_year_landslide))
    
    df <- data.frame(code_city = temp2$code, first_year_landslide_city = temp2$first_year_landslide,
                     code_neighborhood = temp1$code, first_year_landslide_neighborhood = temp1$first_year_landslide)
    
    
    return(df)
     
  }
  

}

# Apply the function to each municipality
# This step calculates the minimum disaster year for the neighborhood of each municipality
output <- lapply(1:nrow(mun_geom), df_neighborhood)
output_append <- bind_rows(output) %>% 
  filter(first_year_landslide_city != 0 & first_year_landslide_neighborhood == 0)

# Subset the data to exclude neighboring municipalities affected by landslides
dados2_robustness <- dados2 %>% subset(!code %in% output_append$code_city)

# Using loop to apply the function to specific variables
output_robustness <- lapply(c('lurban_size', 'sprawl_index'), ggplot_paper)

## Saving DiD plot
output_path <- paste0(path_output_git, "_graph_robustness_checks_urban_size_neighbour.jpg")
ggsave(output_path, output_robustness[[1]], width = 20, height = 10, units = "in", dpi = 100)

output_path <- paste0(path_output_git, "_graph_robustness_checks_sprawl_index_neighbour.jpg")
ggsave(output_path, output_robustness[[2]], width = 20, height = 10, units = "in", dpi = 100)
