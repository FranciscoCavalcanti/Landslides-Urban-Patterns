# Load required libraries
library(dplyr)       # For data manipulation
library(fixest)      # For fixed effects estimation
library(did)         # For difference-in-differences estimation
library(remotes)     # For installing R packages from remote repositories
library(did2s)       # For alternative DiD estimators
library(staggered)   # For staggered DiD designs
library(ggplot2)     # For creating plots
library(gridExtra)   # For arranging multiple plots
library(tidyr)      # Data reshaping
library(broom)

# Set Seed
set.seed(123)

# Set paths for input and output files
path_input <- paste0(DROPBOX_PATH, "/build/input/")
path_output <- paste0(DROPBOX_PATH, "/build/output/") 
path_output_git <- paste0(GITHUB_PATH, "/analysis/output/") 

#### Open Databases ####

# Load the two periods database
dados <- readRDS(paste0(path_output, "database_three_periods.rds"))


#### Including Variables ####

# Create post-treatment and interaction variables for landslides
dados <- dados %>%
  group_by(code) %>%
  mutate(landslide = ifelse(is.na(landslide),0,landslide),
         first_year_landslide = ifelse(is.na(first_year_landslide),0,first_year_landslide),
         landslide = ifelse(landslide > 0, 1, 0),
         total_landslide = sum(landslide, na.rm = TRUE), .groups = "drop",
         post = ifelse(year >= first_year_landslide, 1, 0),
         postland = landslide * post,
         postland2 = total_landslide * post)



# Subset data for landslides occurring before 2011
#dados <- dados %>% subset(first_year_landslide < 2011)

#### Utilizing PSM ####

# Load the PSM database
psm <- readRDS(paste0(path_output, "restricted_PSM_database.rds"))

# Merge PSM data with the main dataset
dados2 <- merge(dados, psm[, c("code", "weights", "region")], by = "code")
#dados2 <- subset(dados2, first_year_landslide < 2011)


## Urban area



dados2 <- dados2 %>% mutate(
  first_year_landslide = ifelse(between(first_year_landslide,2003,2010),2,
                                ifelse(between(first_year_landslide,2011,2022),3,
                                       first_year_landslide)),
  year = ifelse(year == 2000,1,
                ifelse(year == 2010,2,
                       3)),
)

dados2$lurban_population <- log(dados2$urban_population)
dados2$lurban_households <- log(dados2$urban_households)
dados2$density <- dados2$urban_population/dados2$urban_size
dados2$ldensity <- log(dados2$density)
dados2$lurban_size <- log(dados2$urban_size)  



# Define a function to create plots
ggplot_paper <- function(x){
  
  
  ## Paired results
  # Estimate ATT for the matched sample using the dynamic DiD approach
  mw.dyn_p <- aggte(
    att_gt(yname = x,
           gname = "first_year_landslide",
           idname = "code",
           bstrap = TRUE,
           base_period="universal",
           tname = "year",
           clustervars = 'code',
           data = dados2), type = "dynamic")
  
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
      linewidth = 0.5, width = 0.1, position = position_dodge(width = 0.5)
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
    scale_y_continuous(breaks = seq(-16, 16, by = 1)) +
    coord_flip() +
    theme_minimal() + 
    theme(text = element_text(size = 25),
          legend.text = element_text(size = 25),
          legend.title = element_text(size = 25),
          legend.key.width = unit(1.5, "cm"),
          legend.key.height = unit(1, "cm"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  # Combine the plot and the table
  graph <- graph + annotation_custom(grob = tabela_grob,
                                     xmin = table_pos_y,
                                     xmax = table_pos_y,
                                     ymin = -2, ymax = -2)
  

  return(graph)
}

### Generate plots for population, households and density using the defined function ###

output <- lapply(c('lurban_population',
                   'lurban_households',
                   'ldensity', 'lurban_size'), ggplot_paper)


#### Saving DiD plot ####

# Save plot
ggsave(paste0(path_output_git, "_graph_lurban_population_census.jpg"),
       output[[1]], width = 20, height = 10, units = "in", dpi = 100)

ggsave(paste0(path_output_git, "_graph_lurban_households_census.jpg"),
       output[[2]], width = 20, height = 10, units = "in", dpi = 100)

ggsave(paste0(path_output_git, "_graph_density_census.jpg"),
       output[[3]], width = 20, height = 10, units = "in", dpi = 100)
