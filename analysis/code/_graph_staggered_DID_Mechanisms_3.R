# Load necessary packages
library(dplyr)        # Data manipulation
library(fixest)       # Fixed effects models
library(did)          # Difference-in-differences estimation
library(remotes)      # Install packages from remote repositories
library(did2s)        # DID estimation with two-stage least squares
library(staggered)    # Staggered adoption DID
library(ggplot2)      # Data visualization
library(gridExtra)    # Arrange multiple grid-based plots

# Define paths for input and output files
path_output <- paste0(DROPBOX_PATH, "/build/output/")    # Output file path
path_output_git <- paste0(GITHUB_PATH, "/analysis/output/")  # GitHub output file path

#### Open Databases ####

# Load the dataset
dados <- readRDS(paste0(path_output,"database_panel.rds"))

# Log-transform some variables
dados$employment <- log(dados$employment) 
dados$firms      <- log(dados$firms) 
dados$income     <- log(dados$income)
dados$avg_income <- log(dados$income/dados$employment) 

# Load the PSM (Propensity Score Matching) dataset
psm <- readRDS(paste0(path_output,"restricted_PSM_database.rds"))

#### Selecting the Treated/Control Group Using PSM ####

# Merge the datasets to include the PSM weights
dados2 <- merge(dados, psm[,c("code","weights")], by="code")

#### Main Result - Staggered DiD with PSM ####

# Define a function to create the plots
ggplot_paper <- function(x){
  
  ## No Paired results
  mw.dyn_np <- aggte(
    att_gt(yname = x,
           gname = "first_year_landslide",
           idname = "code",
           bstrap = TRUE,
           tname = "year",
           data = dados), type = "dynamic")
  
  ## Average effect
  est_np <- broom::tidy(mw.dyn_np) %>% 
    select(c(event.time, estimate, std.error, conf.low, conf.high)) %>% 
    mutate(est = 'Broader Sample')
  
  ## Paired results
  mw.dyn_p <- aggte(
    att_gt(yname = x,
           gname = "first_year_landslide",
           idname = "code",
           bstrap = TRUE,
           tname = "year",
           data = dados2), type = "dynamic")
  
  ## Average effect
  est_p <- broom::tidy(mw.dyn_p) %>% 
    select(c(event.time, estimate, std.error, conf.low, conf.high)) %>% 
    mutate(est = 'Matched Sample')
  
  # Combine results
  est <- bind_rows(est_p, est_np)
  
  # Set the title based on the variable name
  title <- ifelse(grepl('urban_size', x), "A) Effect of Landslides on Urban Size",
                  ifelse(grepl('sprawl', x), "B) Effect of Landslides on Sprawl Index",
                         ifelse(grepl('water_size', x), "A) Effect of Landslides on Water Surface",
                                ifelse(grepl('forest_size', x), "B) Effect of Landslides on Forest Formation",
                                       ifelse(grepl('natural_size', x), "C) Effect of Landslides on Other Natural Land Uses", NA)))))
  
  # Z-values for different confidence levels
  z_99 <- 2.576
  z_95 <- 1.96
  z_90 <- 1.645
  
  # Check significance for the broader sample
  IC_99 <- mw.dyn_np$overall.att + c(-z_99 * mw.dyn_np$overall.se, z_99 * mw.dyn_np$overall.se)
  IC_95 <- mw.dyn_np$overall.att + c(-z_95 * mw.dyn_np$overall.se, z_95 * mw.dyn_np$overall.se)
  IC_90 <- mw.dyn_np$overall.att + c(-z_90 * mw.dyn_np$overall.se, z_90 * mw.dyn_np$overall.se)
  
  ATT_significance_np <- ifelse(all(IC_99 < 0) | all(IC_99 > 0), paste0(round(mw.dyn_np$overall.att, 4), "***"),
                                ifelse(all(IC_95 < 0) | all(IC_95 > 0), paste0(round(mw.dyn_np$overall.att, 4), "**"),
                                       ifelse(all(IC_90 < 0) | all(IC_90 > 0), paste0(round(mw.dyn_np$overall.att, 4), "*"),
                                              paste(round(mw.dyn_np$overall.att, 4)))))
  
  # Check significance for the matched sample
  IC_99 <- mw.dyn_p$overall.att + c(-z_99 * mw.dyn_p$overall.se, z_99 * mw.dyn_p$overall.se)
  IC_95 <- mw.dyn_p$overall.att + c(-z_95 * mw.dyn_p$overall.se, z_95 * mw.dyn_p$overall.se)
  IC_90 <- mw.dyn_p$overall.att + c(-z_90 * mw.dyn_p$overall.se, z_90 * mw.dyn_p$overall.se)
  
  ATT_significance_p <- ifelse(all(IC_99 < 0) | all(IC_99 > 0), paste0(round(mw.dyn_p$overall.att, 4), "***"),
                               ifelse(all(IC_95 < 0) | all(IC_95 > 0), paste0(round(mw.dyn_p$overall.att, 4), "**"),
                                      ifelse(all(IC_90 < 0) | all(IC_90 > 0), paste0(round(mw.dyn_p$overall.att, 4), "*"),
                                             paste(round(mw.dyn_p$overall.att, 4)))))
  
  # Create the table as a grob
  dados_tabela <- data.table::data.table(
    `ATT\n(Broader Sample)` = c(ATT_significance_np, paste0("(", round(mw.dyn_np$overall.se, 4), ")")),
    `ATT\n(Matched Sample)` = c(ATT_significance_p, paste0("(", round(mw.dyn_p$overall.se, 4), ")"))
  )
  
  value = c(abs(0 - summary(est$conf.low)[1]), abs(0 - summary(est$conf.high)[6]))
  sequencia <- seq(min(est$conf.low), max(est$conf.high), length.out = 1000)
  quantil_10 <- quantile(sequencia, probs = 0.18)
  quantil_75 <- quantile(sequencia, probs = 0.91)
  
  table_pos_y = ifelse(value[1] < value[2], quantil_75, quantil_10)
  lengend_pos_y = ifelse(value[1] < value[2], 0.75, 0.1)
  
  # Generate table grob
  tabela_grob <- tableGrob(dados_tabela, 
                           rows = NULL, 
                           theme = ttheme_minimal(core = list(fg_params = list(fontsize = 18)), 
                                                  colhead = list(fg_params = list(fontsize = 18, fontface = "bold")), 
                                                  rowhead = list(fg_params = list(fontsize = 18)))) 
  
  graph <- ggplot(data = est, aes(y = event.time, x = estimate, color = est, linetype = est)) +
    geom_errorbar(aes(xmax = conf.high, xmin = conf.low), linewidth = 0.5, width = 0.5, position = position_dodge(width = 0.5)) +
    geom_pointrange(aes(xmax = conf.high, xmin = conf.low), linewidth = 0, position = position_dodge(width = 0.5), linetype = 'blank') +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = -1) +
    labs(x = 'Coefficient', y = 'Period', color = "", linetype = "", title = "") +
    scale_color_manual(name = "", values = c("black", "grey20"), labels = c('Broader Sample', 'Matched Sample')) +
    scale_linetype_manual(name = "", values = c("solid", "dashed"), labels = c('Broader Sample', 'Matched Sample')) + 
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
  
  # Combine the graph and the table
  graph <- graph + annotation_custom(grob = tabela_grob,
                                     xmin = table_pos_y,
                                     xmax = table_pos_y,
                                     ymin = -12, ymax = -8)
  return(graph)
}

# Apply the function to create plots for specified variables
output <- lapply(c('employment', 'firms', 'income', 'avg_income'), ggplot_paper)

# Save the plots
employment_output_path <- paste0(path_output_git, "_graph_mechanisms_employment.jpg")
ggsave(employment_output_path, output[[1]], width = 20, height = 10, units = "in", dpi = 100)

firms_output_path <- paste0(path_output_git, "_graph_mechanisms_firms.jpg")
ggsave(firms_output_path, output[[2]], width = 20, height = 10, units = "in", dpi = 100)

income_output_path <- paste0(path_output_git, "_graph_mechanisms_income.jpg")
ggsave(income_output_path, output[[3]], width = 20, height = 10, units = "in", dpi = 100)

avg_income_output_path <- paste0(path_output_git, "_graph_mechanisms_avg_income.jpg")
ggsave(avg_income_output_path, output[[4]], width = 20, height = 10, units = "in", dpi = 100)
