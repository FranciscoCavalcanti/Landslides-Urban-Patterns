# Load required libraries
library(dplyr)       # For data manipulation
library(fixest)      # For fixed effects estimation
library(did)         # For difference-in-differences estimation
library(remotes)     # For installing R packages from remote repositories
library(did2s)       # For alternative DiD estimators
library(staggered)   # For staggered DiD designs
library(ggplot2)     # For creating plots
library(gridExtra)   # For arranging multiple plots

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

#### Alternative Estimators ####

# Define a function to create plots using different estimators
ggplot_paper <- function(x, estimator){
  
  print(x)
  print(estimator)
  
  ## Paired results
  # Perform event study analysis using the given estimator on the full dataset
  mw.dyn_np  <- event_study(
    data = dados,
    yname = x,
    idname = "code",
    gname = "first_year_landslide",
    tname = "year",
    weights = NULL,
    estimator = estimator)
  
  ## Avg effect
  # Transform the result into a tidy data frame and label it as 'Never-Treated Control Group'
  mw.dyn_np <- mw.dyn_np %>% 
    rename(overall.att = estimate,
           overall.se = std.error,
           event.time = term) %>% 
    mutate(conf.low  = overall.att - (1.96 * overall.se),
           conf.high = overall.att + (1.96 * overall.se)) %>% 
    select(c(event.time, overall.att, overall.se, conf.low, conf.high)) %>% 
    mutate(est = 'Never-Treated Control Group')
  
  # Perform event study analysis using the given estimator on the matched dataset
  mw.dyn_p  <- event_study(
    data = dados2,
    yname = x,
    idname = "code",
    gname = "first_year_landslide",
    tname = "year",
    weights = NULL,
    estimator = estimator)
  
  ## Avg effect
  # Transform the result into a tidy data frame and label it as 'Matched Never-Treated Control Group'
  mw.dyn_p <-  mw.dyn_p %>% 
    rename(overall.att = estimate,
           overall.se = std.error,
           event.time = term) %>% 
    mutate(conf.low  = overall.att - (1.96 * overall.se),
           conf.high = overall.att + (1.96 * overall.se)) %>% 
    select(c(event.time, overall.att, overall.se, conf.low, conf.high)) %>% 
    mutate(est = 'Matched Never-Treated Control Group')
  
  # Combine both results into one data frame
  est <- bind_rows(mw.dyn_p, mw.dyn_np)
  
  # Define the title based on the variable name
  title <- ifelse(grepl('urban_size', x), "A) Effect of Landslides on Urban Size",
                  ifelse(grepl('sprawl', x), "B) Effect of Landslides on Sprawl Index",
                         ifelse(grepl('water_size', x), "A) Effect of Landslides on Water Surface",
                                ifelse(grepl('forest_size', x), "B) Effect of Landslides on Forest Formation",
                                       ifelse(grepl('natural_size', x), "C) Effect of Landslides on Other Natural Land Uses", NA)))))
  
  # Calculate the absolute values for positioning the legend
  value = c(abs(0 - summary(est$conf.low)[1]), abs(0 - summary(est$conf.high)[6]))
  lengend_pos_y = ifelse(value[1] < value[2], 0.8, 0.1)
  
  # Create the plot
  graph <- ggplot(data = est, aes(y = event.time, x = overall.att, color = est, linetype = est)) +
    geom_pointrange(
      aes(xmax = conf.high, xmin = conf.low),
      linewidth = 0.5, position = position_dodge(width = 0.5)
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
    scale_color_manual(name = "", values = c("black", "grey20"),
                       labels = c('Matched Never-Treated Control Group', 'Never-Treated Control Group')) +
    scale_linetype_manual(name = "", values = c("solid", "dashed"),
                          labels = c('Matched Never-Treated Control Group', 'Never-Treated Control Group')) + 
    scale_y_continuous(breaks = seq(-16, 16, by = 2)) +
    coord_flip() +
    theme_minimal() + 
    theme(text = element_text(size = 15),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = c(0.15, lengend_pos_y))
  
  return(graph)
  
}

# Using a loop to generate plots for urban_size and sprawl_index using different estimators
output_lurban_size <- Map(x = c('lurban_size'),
                          estimator = c('did', 'TWFE', 'did2s', 'impute'),
                          ggplot_paper)

output_sprawl_index <- Map(x = c('sprawl_index.x'),
                           estimator = c('did', 'TWFE', 'did2s', 'impute'),
                           ggplot_paper)

# Save the plots for Callaway and Sant'Anna (2020) estimator
lurban_size_output_path <- paste0(path_output_git, "_graph_alternative_urban_size_callaway.jpg")
ggsave(lurban_size_output_path, output_lurban_size[[1]], width = 20, height = 10, units = "in", dpi = 300)

sprawl_index_output_path <- paste0(path_output_git, "_graph_alternative_sprawl_index_callaway.jpg")
ggsave(sprawl_index_output_path, output_sprawl_index[[1]], width = 20, height = 10, units = "in", dpi = 300)

# Save the plots for TWFE model
lurban_size_output_path <- paste0(path_output_git, "_graph_alternative_urban_size_twfe.jpg")
ggsave(lurban_size_output_path, output_lurban_size[[2]], width = 20, height = 10, units = "in", dpi = 300)

sprawl_index_output_path <- paste0(path_output_git, "_graph_alternative_sprawl_index_twfe.jpg")
ggsave(sprawl_index_output_path, output_sprawl_index[[2]], width = 20, height = 10, units = "in", dpi = 300)

# Save the plots for Gardner (2021) estimator
lurban_size_output_path <- paste0(path_output_git, "_graph_alternative_urban_size_gardner.jpg")
ggsave(lurban_size_output_path, output_lurban_size[[3]], width = 20, height = 10, units = "in", dpi = 300)

sprawl_index_output_path <- paste0(path_output_git, "_graph_alternative_sprawl_index_gardner.jpg")
ggsave(sprawl_index_output_path, output_sprawl_index[[3]], width = 20, height = 10, units = "in", dpi = 300)

# Save the plots for Borusyak, Jaravel, Spiess (2021) estimator
lurban_size_output_path <- paste0(path_output_git, "_graph_alternative_urban_size_borusyak.jpg")
ggsave(lurban_size_output_path, output_lurban_size[[4]], width = 20, height = 10, units = "in", dpi = 300)

sprawl_index_output_path <- paste0(path_output_git, "_graph_alternative_sprawl_index_borusyak.jpg")
ggsave(sprawl_index_output_path, output_sprawl_index[[4]], width = 20, height = 10, units = "in", dpi = 300)
