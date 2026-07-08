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

#### Alternative Estimators ####

# Define a function to create plots using different estimators
ggplot_paper <- function(x, estimator){
  
  print(x)
  print(estimator)
  
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
  est <- mw.dyn_p
  
  # Calculate the absolute values for positioning the legend
  value = c(abs(0 - summary(est$conf.low)[1]), abs(0 - summary(est$conf.high)[6]))
#  value = c(abs(0 - summary(est$conf.low)[1]), abs(0 - summary(est$conf.high)[6]))
  sequencia <- seq(min(est$conf.low, na.rm = T), max(est$conf.high, na.rm = T), length.out = 1000)
  # Calculate the quantiles at 10% and 75%
  quantil_10 <- quantile(sequencia, probs = 0.18)
  quantil_75 <- quantile(sequencia, probs = 0.91)
  table_pos_y = ifelse(value[1] < value[2], quantil_75, quantil_10)  
  lengend_pos_y = ifelse(value[1] < value[2], 0.8, 0.1)
  
  z_99 <- 2.576
  z_95 <- 1.96
  z_90 <- 1.645
  
  # Post-treatment periods only (event.time > 0)
  pos_treatment <- mw.dyn_p[mw.dyn_p$event.time > 0, ]
  
  overall.att <- mean(pos_treatment$overall.att, na.rm = T)
  overall.se  <- mean(pos_treatment$overall.se, na.rm = T)
  
  
  # Check significance for the paired sample
  IC_99 <-overall.att + c(-z_99 * overall.se, z_99 * overall.se)
  IC_95 <-overall.att + c(-z_95 * overall.se, z_95 * overall.se)
  IC_90 <-overall.att + c(-z_90 * overall.se, z_90 * overall.se)
  
  ATT_significance_p <- ifelse(all(IC_99 < 0) | all(IC_99 > 0), paste0(round(overall.att, 4), "***"),
                        ifelse(all(IC_95 < 0) | all(IC_95 > 0), paste0(round(overall.att, 4), "**"),
                        ifelse(all(IC_90 < 0) | all(IC_90 > 0), paste0(round(overall.att, 4), "*"),
                        paste(round(overall.att, 4)))))
  
  # Create the table as a grob (graphical object)
  dados_tabela <- data.table::data.table(
    `ATT` = c(ATT_significance_p, paste0("(", round(overall.se, 4), ")"))
  )
  
  # Generate the table grob
  tabela_grob <- tableGrob(dados_tabela, 
                           rows = NULL, 
                           theme = ttheme_minimal(core = list(fg_params = list(fontsize = 30)), 
                                                  colhead = list(fg_params = list(fontsize = 30, fontface = "bold")), 
                                                  rowhead = list(fg_params = list(fontsize = 30)))) 
  
  # Create the plot
  graph <- ggplot(data = est, aes(y = event.time, x = overall.att)) +
    geom_pointrange(
      aes(xmax = conf.high, xmin = conf.low),
      linewidth = 0.5, position = position_dodge(width = 0.5)
    ) +
    geom_errorbar(
      aes(xmax = conf.high, xmin = conf.low),
      linewidth = 0.5, width = 0.5, position = position_dodge(width = 0.5)
    ) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = -.5) +
    labs(x = 'Coefficient', y = 'Period',
         color = "", linetype = "",
         title = "") +
    # scale_color_manual(name = "", values = c("black", "grey20"),
    #                    labels = c('Matched Never-Treated Control Group', 'Never-Treated Control Group')) +
    # scale_linetype_manual(name = "", values = c("solid", "dashed"),
    #                       labels = c('Matched Never-Treated Control Group', 'Never-Treated Control Group')) + 
    scale_y_continuous(breaks = seq(-16, 16, by = 2)) +
    coord_flip() +
    theme_minimal() + 
    theme(text = element_text(size = 20),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = c(0.15, lengend_pos_y))
  
  # Combine the plot and the table
  graph <- graph + annotation_custom(grob = tabela_grob,
                                     xmin = table_pos_y,
                                     xmax = table_pos_y,
                                     ymin = -15, ymax = -11)
  
  return(graph)
  
}

# Generate only the alternative estimators used in the paper: Gardner (did2s) and Borusyak (impute)
# Note: 'did' duplicates the main C&S result; 'TWFE' is the biased baseline — neither is shown in the paper
output_lurban_size <- Map(x        = c('lurban_size', 'lurban_size'),
                          estimator = c('did2s', 'impute'),
                          ggplot_paper)

# Save the plots for Gardner (2021) estimator
lurban_size_output_path <- paste0(path_output_git, "_graph_alternative_urban_size_gardner.jpg")
ggsave(lurban_size_output_path, output_lurban_size[[1]], width = 20, height = 10, units = "in", dpi = 300)

# Save the plots for Borusyak, Jaravel, Spiess (2021) estimator
lurban_size_output_path <- paste0(path_output_git, "_graph_alternative_urban_size_borusyak.jpg")
ggsave(lurban_size_output_path, output_lurban_size[[2]], width = 20, height = 10, units = "in", dpi = 300)
