# Load required libraries
library(dplyr)       # For data manipulation
library(fixest)      # For fixed effects estimation
library(did)         # For difference-in-differences estimation
library(remotes)     # For installing R packages from remote repositories
library(did2s)       # For alternative DiD estimators
library(staggered)   # For staggered DiD designs
library(ggplot2)     # For creating plots
library(gridExtra)   # For arranging multiple plots

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

#### Group Heterogeneity - Staggered DiD with PSM ####

# Define a function to create plots
ggplot_paper <- function(x){
  
  ## Paired results
  # Estimate ATT for the matched sample
  mw.dyn_p <- att_gt(yname = x,
                     gname = "first_year_landslide",
                     idname = "code",
                     bstrap = TRUE,
                     tname = "year",
                     data = dados2)
  
  ## Avg effect
  # Tidy up the results and select relevant columns
  est_p <- broom::tidy(mw.dyn_p) %>% select(c(event.time = time, event.group = group,
                                              estimate = estimate, std.error, conf.low, conf.high)) %>% 
    mutate(est = 'Matched Sample')
  
  # Combine both results into one data frame
  est <- est_p
  est$event.point <- est$event.group
  est$event.group <- paste0("Group ", est$event.group)
  
  # Function to create individual plots for each event group
  plot <- function(x){
    
    df <- est %>% 
      filter(event.group == x)
    
    event.point <- unique(df$event.point)
    
    ggplot(data = df, aes(y = event.time, x = estimate, color = est, linetype = est)) +
      geom_pointrange(
        aes(xmax = conf.high, xmin = conf.low),
        linewidth = 0.5, position = position_dodge(width = 0.5), linetype = 'blank'
      ) +
      geom_errorbar(
        aes(xmax = conf.high, xmin = conf.low),
        linewidth = 0.5, width = 0.5, position = position_dodge(width = 0.5)
      ) +
      geom_hline(yintercept = event.point-0.5) +
      geom_vline(xintercept = 0) +
      labs(x = 'Coefficient', y = 'Period',
           color = "", linetype = "",
           subtitle = x) +
      scale_color_manual(name = "", values = c("black", "grey20"),
                         labels = c('Broader Sample', 'Matched Sample')) +
      scale_linetype_manual(name = "", values = c("solid", "dashed"),
                            labels = c('Broader Sample', 'Matched Sample')) + 
      scale_y_continuous(breaks = seq(2004, 2020, by = 2)) +
      coord_flip() +
      theme_minimal() + 
      theme(legend.key.width = unit(1.5, "cm"),
            legend.key.height = unit(1, "cm"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            legend.position = 'bottom')
  }
  
  # Create a list of plots for each unique event group
  graph <- lapply(unique(est$event.group), plot) %>% 
    ggpubr::ggarrange(plotlist = ., nrow = 5, ncol = 4,
                      common.legend = T, legend = "bottom")
  
  return(graph)
}

# Generate plots for 'lurban_size' and 'sprawl_index.x' using the defined function
output <- lapply(c('lurban_size', 'sprawl_index.x'), ggplot_paper)

#### Saving DiD plot ####

# Save the plot for Urban Size
lurban_size_output_path <- paste0(path_output_git, "_graph_appendix_group_hetero_urban_size.jpg")
ggsave(lurban_size_output_path, output[[1]], width = 20, height = 10, units = "in", dpi = 100)

# Save the plot for Sprawl Index
sprawl_index_output_path <- paste0(path_output_git, "_graph_appendix_group_hetero_sprawl_index.jpg")
ggsave(sprawl_index_output_path, output[[2]], width = 20, height = 10, units = "in", dpi = 100)
