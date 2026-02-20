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

#### Robustness — High-Risk (CEMADEN) with never-treated controls ####

# 1) Load CEMADEN and build the high-risk sample
cemaden <- read.csv(paste0(path_input, "monitored_cemaden.csv"),
                    sep = ";", header = TRUE)

# Merge with the main dataset (adjust the key name if needed)
dados_high_risk <- merge(dados, cemaden, by = "code")

# 2) Plot function (control group = never-treated; do not drop never-treated units)
ggplot_paper <- function(x){
  
  # Dynamic ATT using never-treated controls (default)
  mw.dyn_p <- aggte(
    att_gt(
      yname        = x,
      gname        = "first_year_landslide",
      idname       = "code",
      bstrap       = TRUE,
      clustervars  = "code",
      base_period  = "universal",
      # control_group = "nevertreated", # optional (default)
      tname        = "year",
      data         = dados_high_risk
    ),
    type  = "dynamic",
    na.rm = TRUE   # remove event-times com NA no agregado
  )
  
  # Resultados tidy
  est_p <- broom::tidy(mw.dyn_p) %>%
    dplyr::select(event.time, estimate, std.error, conf.low, conf.high) %>%
    dplyr::mutate(est = "Matched Sample")
  est <- est_p
  
  # Significance (overall ATT)
  z_99 <- 2.576; z_95 <- 1.96; z_90 <- 1.645
  IC_99 <- mw.dyn_p$overall.att + c(-z_99 * mw.dyn_p$overall.se,  z_99 * mw.dyn_p$overall.se)
  IC_95 <- mw.dyn_p$overall.att + c(-z_95 * mw.dyn_p$overall.se,  z_95 * mw.dyn_p$overall.se)
  IC_90 <- mw.dyn_p$overall.att + c(-z_90 * mw.dyn_p$overall.se,  z_90 * mw.dyn_p$overall.se)
  
  ATT_significance_p <- ifelse(all(IC_99 < 0) | all(IC_99 > 0), paste0(round(mw.dyn_p$overall.att, 4), "***"),
                               ifelse(all(IC_95 < 0) | all(IC_95 > 0), paste0(round(mw.dyn_p$overall.att, 4), "**"),
                                      ifelse(all(IC_90 < 0) | all(IC_90 > 0), paste0(round(mw.dyn_p$overall.att, 4), "*"),
                                             paste(round(mw.dyn_p$overall.att, 4)))))
  
  # ATT table (grob)
  dados_tabela <- data.table::data.table(
    `ATT` = c(ATT_significance_p, paste0("(", round(mw.dyn_p$overall.se, 4), ")"))
  )
  
  value <- c(abs(0 - summary(est$conf.low)[1]), abs(0 - summary(est$conf.high)[6]))
  sequencia <- seq(
    min(est$conf.low[is.finite(est$conf.low)]),
    max(est$conf.high[is.finite(est$conf.high)]),
    length.out = 1000
  )
  quantil_10 <- stats::quantile(sequencia, probs = 0.18)
  quantil_75 <- stats::quantile(sequencia, probs = 0.91)
  
  table_pos_y   <- ifelse(value[1] < value[2], quantil_75, quantil_10)
  lengend_pos_y <- ifelse(value[1] < value[2], 0.75, 0.1)
  
  tabela_grob <- gridExtra::tableGrob(
    dados_tabela, rows = NULL,
    theme = gridExtra::ttheme_minimal(
      core    = list(fg_params = list(fontsize = 30)),
      colhead = list(fg_params = list(fontsize = 30, fontface = "bold")),
      rowhead = list(fg_params = list(fontsize = 30))
    )
  )
  
  # 3) Plot (same style as the main figures)
  graph <- ggplot2::ggplot(data = est, aes(y = event.time, x = estimate)) +
    geom_pointrange(aes(xmax = conf.high, xmin = conf.low),
                    linewidth = 0.5,
                    position = position_dodge(width = 0.5),
                    linetype  = "blank") +
    geom_errorbar(aes(xmax = conf.high, xmin = conf.low),
                  linewidth = 0.5, width = 0.5,
                  position = position_dodge(width = 0.5)) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = -1) +
    labs(x = "Coefficient", y = "Period", color = "", linetype = "", title = "") +
    scale_y_continuous(breaks = seq(-16, 16, by = 2)) +
    coord_flip() +
    theme_minimal() +
    theme(
      text = element_text(size = 25),
      legend.text  = element_text(size = 25),
      legend.title = element_text(size = 25),
      legend.key.width  = unit(1.5, "cm"),
      legend.key.height = unit(1, "cm"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = c(0.125, lengend_pos_y)
    ) +
    annotation_custom(grob = tabela_grob,
                      xmin = table_pos_y, xmax = table_pos_y,
                      ymin = -15, ymax = -11)
  
  return(graph)
}

# 4) Run for both outcomes
output <- lapply(c("lurban_size", "sprawl_index"), ggplot_paper)

# 5) Save using the original high-risk (CEMADEN) filenames
lurban_size_output_path <- paste0(path_output_git, "_graph_robustness_checks_urban_size_high_risk.jpg")
ggsave(lurban_size_output_path, output[[1]], width = 20, height = 10, units = "in", dpi = 100)

sprawl_index_output_path <- paste0(path_output_git, "_graph_robustness_checks_sprawl_index_high_risk.jpg")
ggsave(sprawl_index_output_path, output[[2]], width = 20, height = 10, units = "in", dpi = 100)
