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

dados2 <- merge(dados, psm[, c("code", "weights", "state")], by = "code")

#### Selecting the Treated/Control Group Using PSM ####

# Build a safe copy with explicit FE variables
# - state: character -> factor
# - year:  -> integer
# - state_year: factor with all state×year dummies

dados2_fe <- within(dados2, {
  state      <- as.factor(state)
  year       <- as.integer(year)
  state_year <- interaction(state, year, drop = TRUE)
})
dados2_fe <- droplevels(dados2_fe)
# (Optional) enforce default treatment contrasts
options(contrasts = c("contr.treatment", "contr.poly"))

# Define a function to create plots
ggplot_paper <- function(x){
  
  ## Event-study ATT with state-by-year FE as covariates via precomputed factor
  mw.dyn_p <- aggte(
    att_gt(
      yname        = x,
      gname        = "first_year_landslide",
      idname       = "code",
      bstrap       = TRUE,
      clustervars  = "code",
      base_period  = "universal",
      tname        = "year",
      data         = dados2_fe,
      xformla      = ~ state_year    
    ),
    type  = "dynamic",
    na.rm = TRUE
  )
  
  ## Avg effect
  est_p <- broom::tidy(mw.dyn_p) %>% 
    dplyr::select(event.time, estimate, std.error, conf.low, conf.high) %>% 
    dplyr::mutate(est = 'Matched Sample')
  est <- est_p
  
  ## Significance stars for the average ATT
  z_99 <- 2.576; z_95 <- 1.96; z_90 <- 1.645
  IC_99 <- mw.dyn_p$overall.att + c(-z_99 * mw.dyn_p$overall.se,  z_99 * mw.dyn_p$overall.se)
  IC_95 <- mw.dyn_p$overall.att + c(-z_95 * mw.dyn_p$overall.se,  z_95 * mw.dyn_p$overall.se)
  IC_90 <- mw.dyn_p$overall.att + c(-z_90 * mw.dyn_p$overall.se,  z_90 * mw.dyn_p$overall.se)
  ATT_significance_p <- ifelse(all(IC_99 < 0) | all(IC_99 > 0), paste0(round(mw.dyn_p$overall.att, 4), "***"),
                               ifelse(all(IC_95 < 0) | all(IC_95 > 0), paste0(round(mw.dyn_p$overall.att, 4), "**"),
                                      ifelse(all(IC_90 < 0) | all(IC_90 > 0), paste0(round(mw.dyn_p$overall.att, 4), "*"),
                                             paste(round(mw.dyn_p$overall.att, 4)))))
  
  # ATT table
  dados_tabela <- data.table::data.table(
    `ATT` = c(ATT_significance_p, paste0("(", round(mw.dyn_p$overall.se, 4), ")"))
  )
  
  value <- c(abs(0 - summary(est$conf.low)[1]), abs(0 - summary(est$conf.high)[6]))
  sequencia <- seq(
    min(est$conf.low[is.finite(est$conf.low)]),
    max(est$conf.high[is.finite(est$conf.high)]),
    length.out = 1000
  )
  quantil_10 <- quantile(sequencia, probs = 0.18)
  quantil_75 <- quantile(sequencia, probs = 0.91)
  table_pos_y   <- ifelse(value[1] < value[2], quantil_75, quantil_10)
  lengend_pos_y <- ifelse(value[1] < value[2], 0.75, 0.1)
  
  tabela_grob <- tableGrob(
    dados_tabela,
    rows = NULL,
    theme = ttheme_minimal(
      core    = list(fg_params = list(fontsize = 30)), 
      colhead = list(fg_params = list(fontsize = 30, fontface = "bold")), 
      rowhead = list(fg_params = list(fontsize = 30))
    )
  )
  
  # Plot (same style)
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
    labs(x = 'Coefficient', y = 'Period', color = "", linetype = "", title = "") +
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
          legend.position = c(0.125, lengend_pos_y)) +
    annotation_custom(grob = tabela_grob,
                      xmin = table_pos_y, xmax = table_pos_y,
                      ymin = -15, ymax = -11)
  
  return(graph)
}

# Run for both outcomes
output <- lapply(c('lurban_size', 'sprawl_index'), ggplot_paper)

#### Saving DiD plot ####
lurban_size_output_path <- paste0(path_output_git, "_graph_robustness_urban_size_stateyear.jpg")
ggsave(lurban_size_output_path, output[[1]], width = 20, height = 10, units = "in", dpi = 100)

sprawl_index_output_path <- paste0(path_output_git, "_graph_robusteness_sprawl_index_stateyear.jpg")
ggsave(sprawl_index_output_path, output[[2]], width = 20, height = 10, units = "in", dpi = 100)
