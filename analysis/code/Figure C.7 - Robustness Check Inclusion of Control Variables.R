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

psm$p_urbana <- psm$urban_population / psm$population
psm$p_informal <- psm$inap_houses / psm$total_houses

psm$log_urban_size <- log(psm$urban_size)
psm$log_avg_income <- log(psm$avg_income)
psm$log_population <- log(psm$population)

#### Selecting the Treated/Control Group Using PSM ####

# Merge the main dataset with the PSM dataset using the "code" column
dados2 <- merge(dados, psm[, c("code", "weights","p_urbana","p_informal","avg_tri",
                               "log_urban_size","log_avg_income","log_population")], by = "code")

#### Create linear trend interactions for baseline controls ####
dados2 <- dados2 %>%
  mutate(trend = year - min(year),
         log_avg_income_trend = log_avg_income * trend,
         log_population_trend = log_population * trend,
         p_urbana_trend = p_urbana * trend,
         p_informal_trend = p_informal * trend)

#### Main Result - Staggered DiD with PSM ####

# Define a function to create plots
ggplot_paper <- function(x){
  print(x)
  ## Paired results
  # Estimate ATT for the matched sample using the dynamic DiD approach
  mw.dyn_p <- aggte(
    att_gt(yname = x,
           gname = "first_year_landslide",
           xformla = ~ log_avg_income_trend + log_population_trend +
             p_urbana_trend + p_informal_trend,
           idname = "code",
           bstrap = TRUE,
           clustervars = "code",
           base_period = "universal",
           tname = "year",
           data = dados2),
    type = "dynamic",
    na.rm = TRUE     
  )
  
  ## Avg effect
  est_p <- broom::tidy(mw.dyn_p) %>%
    select(c(event.time, estimate, std.error, conf.low, conf.high)) %>%
    mutate(est = 'Matched Sample')
  
  est <- est_p
  
  # Z values for different confidence levels
  z_99 <- 2.576
  z_95 <- 1.96
  z_90 <- 1.645
  
  # Confidence intervals for ATT
  IC_99 <- mw.dyn_p$overall.att + c(-z_99 * mw.dyn_p$overall.se, z_99 * mw.dyn_p$overall.se)
  IC_95 <- mw.dyn_p$overall.att + c(-z_95 * mw.dyn_p$overall.se, z_95 * mw.dyn_p$overall.se)
  IC_90 <- mw.dyn_p$overall.att + c(-z_90 * mw.dyn_p$overall.se, z_90 * mw.dyn_p$overall.se)
  
  ATT_significance_p <- ifelse(all(IC_99 < 0) | all(IC_99 > 0), paste0(round(mw.dyn_p$overall.att, 4), "***"),
                               ifelse(all(IC_95 < 0) | all(IC_95 > 0), paste0(round(mw.dyn_p$overall.att, 4), "**"),
                                      ifelse(all(IC_90 < 0) | all(IC_90 > 0), paste0(round(mw.dyn_p$overall.att, 4), "*"),
                                             paste(round(mw.dyn_p$overall.att, 4)))))
  
  dados_tabela <- data.table::data.table(
    `ATT` = c(ATT_significance_p, paste0("(", round(mw.dyn_p$overall.se, 4), ")"))
  )
  
  value = c(abs(0 - summary(est$conf.low)[1]), abs(0 - summary(est$conf.high)[6]))
  sequencia <- seq(min(est$conf.low, na.rm = T), max(est$conf.high, na.rm = T), length.out = 1000)
  quantil_10 <- quantile(sequencia, probs = 0.18)
  quantil_75 <- quantile(sequencia, probs = 0.91)
  
  table_pos_y = ifelse(value[1] < value[2], quantil_75, quantil_10)  
  lengend_pos_y = ifelse(value[1] < value[2], 0.75, 0.1)
  
  tabela_grob <- tableGrob(dados_tabela, 
                           rows = NULL, 
                           theme = ttheme_minimal(core = list(fg_params = list(fontsize = 30)), 
                                                  colhead = list(fg_params = list(fontsize = 30, fontface = "bold")), 
                                                  rowhead = list(fg_params = list(fontsize = 30)))) 
  
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
          legend.position = c(0.125, lengend_pos_y))
  
  graph <- graph + annotation_custom(grob = tabela_grob,
                                     xmin = table_pos_y,
                                     xmax = table_pos_y,
                                     ymin = -15, ymax = -11)
  return(graph)
}

# Generate plots for 'lurban_size' and 'sprawl_index'
output <- lapply(c('lurban_size', 'sprawl_index'), ggplot_paper)

#### Saving DiD plot ####

# Save the plot for Urban Size
lurban_size_output_path <- paste0(path_output_git, "_graph_robustness_inclusion_control_urban_size.jpg")
ggsave(lurban_size_output_path, output[[1]], width = 20, height = 10, units = "in", dpi = 100)

# Save the plot for Sprawl Index
sprawl_index_output_path <- paste0(path_output_git, "_graph_robustness_inclusion_control_sprawl_index.jpg")
ggsave(sprawl_index_output_path, output[[2]], width = 20, height = 10, units = "in", dpi = 100)
