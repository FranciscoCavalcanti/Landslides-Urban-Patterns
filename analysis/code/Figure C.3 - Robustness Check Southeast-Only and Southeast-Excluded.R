# Load required libraries
library(dplyr)          # For data manipulation
library(fixest)         # For fixed effects models
library(did)            # For Difference-in-Differences analysis
library(remotes)        # For installing R packages from remote repositories
library(did2s)          # For two-step Difference-in-Differences estimation
library(staggered)      # For staggered adoption Difference-in-Differences analysis
library(ggplot2)        # For data visualization
library(gridExtra)      # For arranging multiple grid-based plots
library(broom)

# Set Seed
set.seed(123)

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
           base_period = "universal",
           tname = "year",
           clustervars = 'code',
           data = dados_robustness), type = "dynamic")
  
  ## Avg effect
  # Transform the result into a tidy data frame
  est_p <- broom::tidy(mw.dyn_p) %>% select(c(event.time, estimate, std.error, conf.low, conf.high))
  
  # Combined results
  est <- est_p
  
  # z-values for different confidence levels
  z_99 <- 2.576
  z_95 <- 1.96
  z_90 <- 1.645
  
  # Significance checks
  IC_99 <- mw.dyn_p$overall.att + c(-z_99 * mw.dyn_p$overall.se, z_99 * mw.dyn_p$overall.se)
  IC_95 <- mw.dyn_p$overall.att + c(-z_95 * mw.dyn_p$overall.se, z_95 * mw.dyn_p$overall.se)
  IC_90 <- mw.dyn_p$overall.att + c(-z_90 * mw.dyn_p$overall.se, z_90 * mw.dyn_p$overall.se)
  
  ATT_significance_p <- ifelse(all(IC_99 < 0) | all(IC_99 > 0), paste0(round(mw.dyn_p$overall.att,4), "***"),
                               ifelse(all(IC_95 < 0) | all(IC_95 > 0), paste0(round(mw.dyn_p$overall.att,4), "**"),
                                      ifelse(all(IC_90 < 0) | all(IC_90 > 0), paste0(round(mw.dyn_p$overall.att,4), "*"),
                                             paste(round(mw.dyn_p$overall.att,4)))))
  
  # Small table for displaying results
  dados_tabela <- data.table::data.table(
    `ATT` = c(ATT_significance_p, paste0("(",round(mw.dyn_p$overall.se,4),")"))
  )
  
  value = c(abs(0 - summary(est$conf.low)[1]),abs(0 - summary(est$conf.high)[6]))
  sequencia <- seq(min(est$conf.low, na.rm = TRUE), max(est$conf.high, na.rm = TRUE), length.out = 1000)
  quantil_10 <- quantile(sequencia, probs = 0.14)
  quantil_75 <- quantile(sequencia, probs = 0.91)
  
  table_pos_y = ifelse(value[1] < value[2],quantil_75,quantil_10)  
  lengend_pos_y = ifelse(value[1] < value[2],0.8,0.1)
  
  tabela_grob <- tableGrob(dados_tabela, 
                           rows = NULL, 
                           theme = ttheme_minimal(core = list(fg_params = list(fontsize = 30)), 
                                                  colhead = list(fg_params = list(fontsize = 30, fontface = "bold")), 
                                                  rowhead = list(fg_params = list(fontsize = 30)))) 
  
  # Plot
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
    geom_hline(yintercept = -1) +
    labs( x='Coefficient', y='Period', color = "", linetype = "", title = "") +
    scale_y_continuous(breaks=seq(-16,16, by = 2)) +
    coord_flip() +
    theme_minimal() + 
    theme(text = element_text(size = 15),
          panel.grid.major.x=element_blank(),
          panel.grid.minor.x=element_blank(),
          legend.position = c(0.15, lengend_pos_y))
  
  graph <- graph + annotation_custom(grob=tabela_grob, 
                                     xmin=table_pos_y, 
                                     xmax=table_pos_y, 
                                     ymin=-15, ymax=-11)
  return(graph)
}

# --- Robustness using ONLY Southeast municipalities ---
dados_robustness  <- dados  %>% dplyr::filter(region %in% c("Southeast"))
output_robustness_SE <- lapply(c('lurban_size','sprawl_index'), ggplot_paper)

# Save figures (original names)
output_path <- paste0(path_output_git, "_graph_robustness_checks_urban_size_southeast.jpg")
ggsave(output_path, output_robustness_SE[[1]], width = 20, height = 10, units = "in", dpi = 300)

output_path <- paste0(path_output_git, "_graph_robustness_checks_sprawl_index_southeast.jpg")
ggsave(output_path, output_robustness_SE[[2]], width = 20, height = 10, units = "in", dpi = 300)


#### Robustness 2 - Sample EXCLUDING Southeast ####

# Reuse the SAME ggplot_paper() (it reads from `dados_robustness`)
dados_robustness  <- dados %>% dplyr::filter(!region %in% c("Southeast"))
output_robustness_noSE <- lapply(c('lurban_size','sprawl_index'), ggplot_paper)

# Save figures (keep your previous naming pattern)
output_path <- paste0(path_output_git, "_graph_robustness_checks_urban_size_no_southeast.jpg")
ggsave(output_path, output_robustness_noSE[[1]], width = 20, height = 10, units = "in", dpi = 300)

output_path <- paste0(path_output_git, "_graph_robustness_checks_sprawl_index_no_southeast.jpg")
ggsave(output_path, output_robustness_noSE[[2]], width = 20, height = 10, units = "in", dpi = 300)

