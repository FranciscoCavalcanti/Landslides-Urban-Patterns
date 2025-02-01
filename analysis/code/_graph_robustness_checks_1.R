# Load required libraries
library(dplyr)          # For data manipulation
library(fixest)         # For fixed effects models
library(did)            # For Difference-in-Differences analysis
library(remotes)        # For installing R packages from remote repositories
library(did2s)          # For two-step Difference-in-Differences estimation
library(staggered)      # For staggered adoption Difference-in-Differences analysis
library(ggplot2)        # For data visualization
library(gridExtra)      # For arranging multiple grid-based plots

# Define file paths using the specified directories
path_input <- paste0(DROPBOX_PATH, "/build/input/")
path_output <- paste0(DROPBOX_PATH, "/build/output/") 
path_output_git <- paste0(GITHUB_PATH, "/analysis/output/") 

#### Open Databases ####

# Load datasets from the specified file paths
dados <- readRDS(paste0(path_output,"database_panel.rds"))
psm <- readRDS(paste0(path_output,"restricted_PSM_database.rds"))

#### Selecting the Treated/Control Group Using PSM ####

# Merge the main dataset with the PSM dataset using the "code" column
dados2 <- merge(dados, psm[,c("code","weights")], by="code")

#### Robustness 1 - Sample considering only Southeast ####

# Define a function to create ggplot visualizations for robustness checks
ggplot_paper <- function(x){
  

  ## Paired results
  # Calculate the dynamic ATT for the given variable (x) with matching
  mw.dyn_p <- aggte(
    att_gt(yname = x,
           gname = "first_year_landslide",
           idname = "code",
           bstrap = TRUE,
           tname = "year",
           data = dados_robustness), type = "dynamic")
  
  ## Avg effect
  # Transform the result into a tidy data frame and label it as 'Matched Never-Treated Control Group'
  est_p <- broom::tidy(mw.dyn_p) %>% select(c(event.time, estimate,std.error,conf.low,conf.high))
  
  # Combine both results into one data frame
  est <- est_p
  

  # Define z-values for different confidence levels
  z_99 <- 2.576
  z_95 <- 1.96
  z_90 <- 1.645
  

  # Check significance for paired results
  IC_99 <- mw.dyn_p$overall.att + c(-z_99 * mw.dyn_p$overall.se, z_99 * mw.dyn_p$overall.se)
  IC_95 <- mw.dyn_p$overall.att + c(-z_95 * mw.dyn_p$overall.se, z_95 * mw.dyn_p$overall.se)
  IC_90 <- mw.dyn_p$overall.att + c(-z_90 * mw.dyn_p$overall.se, z_90 * mw.dyn_p$overall.se)
  
  # Assign significance levels based on confidence intervals
  ATT_significance_p <- ifelse(all(IC_99) < 0 | all(IC_99) > 0, paste0(round(mw.dyn_p$overall.att,4), "***"),
                        ifelse(all(IC_95) < 0 | all(IC_95) > 0, paste0(round(mw.dyn_p$overall.att,4), "**"),
                        ifelse(all(IC_90) < 0 | all(IC_90) > 0, paste0(round(mw.dyn_p$overall.att,4), "*"),
                        paste(round(mw.dyn_p$overall.att,4)))))
  
  # Create a table for displaying results
  dados_tabela <- data.table::data.table(
    `ATT` = c(ATT_significance_p, paste0("(",round(mw.dyn_p$overall.se,4),")"))
  )
  
  value = c(abs(0 - summary(est$conf.low)[1]),abs(0 - summary(est$conf.high)[6]))
  sequencia <- seq(min(est$conf.low), max(est$conf.high), length.out = 1000)
  # Calculate the 10th and 75th percentiles
  quantil_10 <- quantile(sequencia, probs = 0.14)
  quantil_75 <- quantile(sequencia, probs = 0.91)
  
  # Define table and legend positions based on the values
  table_pos_y = ifelse(value[1] < value[2],quantil_75,quantil_10)  
  lengend_pos_y = ifelse(value[1] < value[2],0.8,0.1)
  
  # Create a grob for the table
  tabela_grob <- tableGrob(dados_tabela, 
                           rows = NULL, 
                           theme = ttheme_minimal(core = list(fg_params = list(fontsize = 30)), 
                                                  colhead = list(fg_params = list(fontsize = 30, fontface = "bold")), 
                                                  rowhead = list(fg_params = list(fontsize = 30)))) 
  
  # Create the plot
  graph <- ggplot(data=est, aes(y = event.time, x = estimate)) +
    geom_pointrange(
      aes(xmax = conf.high, xmin = conf.low),
      linewidth = 0.5 ,position = position_dodge(width=0.5)
    ) +
    geom_errorbar(
      aes(xmax = conf.high, xmin = conf.low),
      linewidth = 0.5 , width = 0.5, position = position_dodge(width=0.5)
    ) +
    geom_vline(xintercept=0) +
    geom_hline(yintercept = -.5) +
    labs( x='Coefficient', y='Period',
          color = "",linetype = "",
          title = "") +
    # scale_color_manual(name ="", values = c("black","grey20"),
    #                    labels = c('Matched Never-Treated Control Group','Never-Treated Control Group')) +
    # scale_linetype_manual(name ="", values = c("solid","dashed"),
    #                       labels = c('Matched Never-Treated Control Group','Never-Treated Control Group')) + 
    scale_y_continuous(breaks=seq(-16,16, by = 2)) +
    coord_flip() +
    theme_minimal() + 
    theme(text = element_text(size = 15),
          panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank(),
          legend.position = c(0.15, lengend_pos_y))
  
  # Combine the plot and table
  graph <- graph + annotation_custom(grob=tabela_grob, 
                                     xmin=table_pos_y, 
                                     xmax=table_pos_y, 
                                     ymin=-15, ymax=-11)
  return(graph)
  
}

# Filter data for Southeast region and apply the function
#dados2_robustness <- dados2 %>% filter(region %in% c("Southeast"))
dados_robustness  <- dados  %>% filter(region %in% c("Southeast"))
output_robustness1 <- lapply(c('lurban_size','sprawl_index.x'), ggplot_paper)

# Save the plots as jpg files
output_path <- paste0(path_output_git, "_graph_robustness_checks_urban_size_southeast.jpg")
ggsave(output_path, output_robustness1[[1]], width = 20, height = 10, units = "in", dpi = 300)

output_path <- paste0(path_output_git, "_graph_robustness_checks_sprawl_index_southeast.jpg")
ggsave(output_path, output_robustness1[[2]], width = 20, height = 10, units = "in", dpi = 300)

#### Robustness 2 - Sample of High-Risk Municipalities ####

# Load cemaden data and merge with the main datasets
cemaden <- read.csv(paste0(path_input,"monitored_cemaden.csv"),sep=";",header=TRUE)
dados_robustness  <- merge(dados, cemaden, by="code")
#dados2_robustness <- merge(dados2, cemaden, by="code")
output_robustness2 <- lapply(c('lurban_size','sprawl_index.x'), ggplot_paper)

# Save the plots as jpg files
output_path <- paste0(path_output_git, "_graph_robustness_checks_urban_size_high_risk.jpg")
ggsave(output_path, output_robustness2[[1]], width = 20, height = 10, units = "in", dpi = 300)

output_path <- paste0(path_output_git, "_graph_robustness_checks_sprawl_index_high_risk.jpg")
ggsave(output_path, output_robustness2[[2]], width = 20, height = 10, units = "in", dpi = 300)

#### Robustness 3 - Not Yet Treated as a Control Group ####

# Define a function for robustness check with 'notyettreated' control group
ggplot_paper <- function(x){
  
  ## Paired results
  # Calculate the dynamic ATT for the given variable (x) without matching
  mw.dyn_p <- aggte(
    att_gt(yname = x,
           gname = "first_year_landslide",
           idname = "code",
           bstrap = TRUE,
           tname = "year",
           data = subset(dados2, first_year_landslide != 0),
           control_group = "notyettreated"), type = "dynamic", na.rm = T)
  
  ## Avg effect
  # Transform the result into a tidy data frame and label it as 'Never-Treated Control Group'
  est_p <- broom::tidy(mw.dyn_p) %>% select(c(event.time, estimate,std.error,conf.low,conf.high))
  

  
  # Combine both results into one data frame
  est <- est_p
  

  # Define z-values for different confidence levels
  z_99 <- 2.576
  z_95 <- 1.96
  z_90 <- 1.645
  

  # Check significance for paired results
  IC_99 <- mw.dyn_p$overall.att + c(-z_99 * mw.dyn_p$overall.se, z_99 * mw.dyn_p$overall.se)
  IC_95 <- mw.dyn_p$overall.att + c(-z_95 * mw.dyn_p$overall.se, z_95 * mw.dyn_p$overall.se)
  IC_90 <- mw.dyn_p$overall.att + c(-z_90 * mw.dyn_p$overall.se, z_90 * mw.dyn_p$overall.se)
  
  # Assign significance levels based on confidence intervals
  ATT_significance_p <- ifelse(all(IC_99) < 0 | all(IC_99) > 0, paste0(round(mw.dyn_p$overall.att,4), "***"),
                               ifelse(all(IC_95) < 0 | all(IC_95) > 0, paste0(round(mw.dyn_p$overall.att,4), "**"),
                                      ifelse(all(IC_90) < 0 | all(IC_90) > 0, paste0(round(mw.dyn_p$overall.att,4), "*"),
                                             paste(round(mw.dyn_p$overall.att,4)))))
  
  # Create a table for displaying results
  dados_tabela <- data.table::data.table(
    `ATT` = c(ATT_significance_p, paste0("(",round(mw.dyn_p$overall.se,4),")"))
  )
  
  value = c(abs(0 - summary(est$conf.low)[1]),abs(0 - summary(est$conf.high)[6]))
  sequencia <- seq(min(est$conf.low), max(est$conf.high), length.out = 1000)
  # Calculate the 10th and 75th percentiles
  quantil_10 <- quantile(sequencia, probs = 0.14)
  quantil_75 <- quantile(sequencia, probs = 0.91)
  
  # Define table and legend positions based on the values
  table_pos_y = ifelse(value[1] < value[2],quantil_75,quantil_10)  
  lengend_pos_y = ifelse(value[1] < value[2],0.8,0.1)
  
  # Create a grob for the table
  tabela_grob <- tableGrob(dados_tabela, 
                           rows = NULL, 
                           theme = ttheme_minimal(core = list(fg_params = list(fontsize = 30)), 
                                                  colhead = list(fg_params = list(fontsize = 30, fontface = "bold")), 
                                                  rowhead = list(fg_params = list(fontsize = 30)))) 
  
  # Create the plot
  graph <- ggplot(data=est, aes(y = event.time, x = estimate)) +
    geom_pointrange(
      aes(xmax = conf.high, xmin = conf.low),
      linewidth = 0.5 ,position = position_dodge(width=0.5)
    ) +
    geom_errorbar(
      aes(xmax = conf.high, xmin = conf.low),
      linewidth = 0.5 , width = 0.5, position = position_dodge(width=0.5)
    ) +
    geom_vline(xintercept=0) +
    geom_hline(yintercept = -.5) +
    labs( x='Coefficient', y='Period',
          color = "",linetype = "",
          title = "") +
    # scale_color_manual(name ="", values = c("black","grey20"),
    #                    labels = c('Matched Never-Treated Control Group','Never-Treated Control Group')) +
    # scale_linetype_manual(name ="", values = c("solid","dashed"),
    #                       labels = c('Matched Never-Treated Control Group','Never-Treated Control Group')) + 
    scale_y_continuous(breaks=seq(-16,16, by = 2)) +
    coord_flip() +
    theme_minimal() + 
    theme(text = element_text(size = 25),
          panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank(),
          legend.position = c(0.15, lengend_pos_y))
  
  # Combine the plot and table
  graph <- graph + annotation_custom(grob=tabela_grob, 
                                     xmin=table_pos_y, 
                                     xmax=table_pos_y, 
                                     ymin=-15, ymax=-11)
  return(graph)
  
}

# Apply the function for robustness check with 'notyettreated' control group
output_robustness3 <- lapply(c('lurban_size','sprawl_index.x'), ggplot_paper)

# Save the plots as jpg files
output_path <- paste0(path_output_git, "_graph_robustness_checks_urban_size_not_yet_treated.jpg")
ggsave(output_path, output_robustness3[[1]], width = 20, height = 10, units = "in", dpi = 300)

output_path <- paste0(path_output_git, "_graph_robustness_checks_sprawl_index_not_yet_treated.jpg")
ggsave(output_path, output_robustness3[[2]], width = 20, height = 10, units = "in", dpi = 300)