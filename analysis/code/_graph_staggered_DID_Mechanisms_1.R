# Load required libraries
library(dplyr)
library(fixest)
library(did)
library(remotes)
library(did2s)
library(staggered)
library(ggplot2)
library(gridExtra)

# Set the paths for input and output files
path_output <- paste0(DROPBOX_PATH, "/build/output/") 
path_output_git <- paste0(GITHUB_PATH, "/analysis/output/") 

#### Open Databases ####

# Load datasets from the specified file paths
dados <- readRDS(paste0(path_output, "database_panel.rds"))
dados$gdp_pc  <- dados$gdp / dados$population
dados$lgdp_pc <- log(dados$gdp_pc + 1) 
psm <- readRDS(paste0(path_output, "restricted_PSM_database.rds"))

#### Selecting the Treated/Control Group Using PSM ####

# Merge the main dataset with the PSM dataset using the "code" column
dados2 <- merge(dados, psm[, c("code", "weights")], by = "code")

#### Main Result - Staggered DiD with PSM ####

# Function to create plots for different variables
ggplot_paper <- function(x){
  
  ## No Paired results
  # Estimate ATT (Average Treatment Effect on the Treated) for the broader sample using the dynamic DiD approach
  mw.dyn_np <- aggte(
    att_gt(yname = x,
           gname = "first_year_landslide",
           idname = "code",
           bstrap = TRUE,
           tname = "year",
           data = dados), type = "dynamic")
  
  ## Avg effect
  # Tidy up the results and select relevant columns
  est_np <- broom::tidy(mw.dyn_np) %>% select(c(event.time, estimate, std.error, conf.low, conf.high)) %>% 
    mutate(est = 'Broader Sample')
  
  ## Paired results
  # Estimate ATT for the matched sample using the dynamic DiD approach
  mw.dyn_p <- aggte(
    att_gt(yname = x,
           gname = "first_year_landslide",
           idname = "code",
           bstrap = TRUE,
           tname = "year",
           data = dados2), type = "dynamic")
  
  ## Avg effect
  # Tidy up the results and select relevant columns
  est_p <- broom::tidy(mw.dyn_p) %>% select(c(event.time, estimate, std.error, conf.low, conf.high)) %>% 
    mutate(est = 'Matched Sample')
  
  # Combine both results into one data frame
  est <- bind_rows(est_p, est_np)
  
  # Determine the title based on the variable name
  title <- ifelse(grepl('urban_size', x), "A) Effect of Landslides on Urban Size",
                  ifelse(grepl('sprawl', x), "B) Effect of Landslides on Sprawl Index",
                         ifelse(grepl('water_size', x), "A) Effect of Landslides on Water Surface",
                                ifelse(grepl('forest_size', x), "B) Effect of Landslides on Forest Formation",
                                       ifelse(grepl('natural_size', x), "C) Effect of Landslides on Other Natural Land Uses", NA)))))
  
  # Z values for different confidence levels
  z_99 <- 2.576
  z_95 <- 1.96
  z_90 <- 1.645
  
  # Check significance for the non-paired sample
  IC_99 <- mw.dyn_np$overall.att + c(-z_99 * mw.dyn_np$overall.se, z_99 * mw.dyn_np$overall.se)
  IC_95 <- mw.dyn_np$overall.att + c(-z_95 * mw.dyn_np$overall.se, z_95 * mw.dyn_np$overall.se)
  IC_90 <- mw.dyn_np$overall.att + c(-z_90 * mw.dyn_np$overall.se, z_90 * mw.dyn_np$overall.se)
  
  ATT_significance_np <- ifelse(all(IC_99 < 0) | all(IC_99 > 0), paste0(round(mw.dyn_np$overall.att, 4), "***"),
                                ifelse(all(IC_95 < 0) | all(IC_95 > 0), paste0(round(mw.dyn_np$overall.att, 4), "**"),
                                       ifelse(all(IC_90 < 0) | all(IC_90 > 0), paste0(round(mw.dyn_np$overall.att, 4), "*"),
                                              paste(round(mw.dyn_np$overall.att, 4)))))
  
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
    `ATT\n(Broader Sample)` = c(ATT_significance_np, paste0("(", round(mw.dyn_np$overall.se, 4), ")")),
    `ATT\n(Matched Sample)` = c(ATT_significance_p, paste0("(", round(mw.dyn_p$overall.se, 4), ")"))
  )
  
  value = c(abs(0 - summary(est$conf.low)[1]), abs(0 - summary(est$conf.high)[6]))
  sequencia <- seq(min(est$conf.low), max(est$conf.high), length.out = 1000)
  # Calculate the quantiles at 10% and 75%
  quantil_10 <- quantile(sequencia, probs = 0.18)
  quantil_75 <- quantile(sequencia, probs = 0.91)
  
  table_pos_y = ifelse(value[1] < value[2], quantil_75, quantil_10)  
  lengend_pos_y = ifelse(value[1] < value[2], 0.75, 0.1)
  
  # Generate the table grob
  tabela_grob <- tableGrob(dados_tabela, 
                           rows = NULL, 
                           theme = ttheme_minimal(core = list(fg_params = list(fontsize = 18)), 
                                                  colhead = list(fg_params = list(fontsize = 18, fontface = "bold")), 
                                                  rowhead = list(fg_params = list(fontsize = 18)))) 
  
  # Create the plot
  graph <- ggplot(data = est, aes(y = event.time, x = estimate, color = est, linetype = est)) +
    geom_errorbar(
      aes(xmax = conf.high, xmin = conf.low),
      linewidth = 0.5, width = 0.5, position = position_dodge(width = 0.5)
    ) +
    geom_pointrange(
      aes(xmax = conf.high, xmin = conf.low),
      linewidth = 0, position = position_dodge(width = 0.5), linetype = 'blank'
    ) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = -1) +
    labs(x = 'Coefficient', y = 'Period',
         color = "", linetype = "",
         title = "") +
    scale_color_manual(name = "", values = c("black", "grey20"),
                       labels = c('Broader Sample', 'Matched Sample')) +
    scale_linetype_manual(name = "", values = c("solid", "dashed"),
                          labels = c('Broader Sample', 'Matched Sample')) + 
    scale_y_continuous(breaks = seq(-16, 16, by = 2)) +
    coord_flip() +
    theme_minimal() + 
    theme(text = element_text(size = 25),
          legend.text = element_text(size = 25), # Adjusts the size of the legend text
          legend.title = element_text(size = 25), # Adjusts the size of the legend title
          legend.key.width = unit(1.5, "cm"),
          legend.key.height = unit(1, "cm"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = c(0.15, lengend_pos_y))
  
  # Combine the plot and the table
  if(x %in% c('lextensao_km','area_rodovia')){
    graph <- graph + annotation_custom(grob = tabela_grob,
                                       xmin = table_pos_y,
                                       xmax = table_pos_y,
                                       ymin = -4, ymax = -2)
  } else {
    graph <- graph + annotation_custom(grob = tabela_grob,
                                       xmin = table_pos_y,
                                       xmax = table_pos_y,
                                       ymin = -15, ymax = -11)
  }
  
  return(graph)
}

# Using loop to apply the function to multiple variables
output <- lapply(c('dum_financ', 'lfinanc_imob', 'lgdp_pc', 'lpopulation'), ggplot_paper)

# Save the plots for each variable
dum_financ_output_path <- paste0(path_output_git, "_graph_mechanisms_dum_financ.jpg")
ggsave(dum_financ_output_path, output[[1]], width = 20, height = 10, units = "in", dpi = 100)

lfinanc_imob_output_path <- paste0(path_output_git, "_graph_mechanisms_lfinanc_imob.jpg")
ggsave(lfinanc_imob_output_path, output[[2]], width = 20, height = 10, units = "in", dpi = 100)

lgdp_pc_output_path <- paste0(path_output_git, "_graph_mechanisms_lgdp_pc.jpg")
ggsave(lgdp_pc_output_path, output[[3]], width = 20, height = 10, units = "in", dpi = 100)

lpopulation_output_path <- paste0(path_output_git, "_graph_mechanisms_lpopulation.jpg")
ggsave(lpopulation_output_path, output[[4]], width = 20, height = 10, units = "in", dpi = 100)
