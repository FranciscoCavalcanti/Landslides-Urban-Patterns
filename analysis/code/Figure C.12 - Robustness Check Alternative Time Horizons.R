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

#### Staggered DiD with Different Time-Frames ###

fmt_dec <- function(x, k = 4) sprintf(paste0("%.", k, "f"), x)

# Plot function: event.time on X, estimate on Y, vertical error bars
ggplot_paper <- function(x, data_in = dados2, min_e = NULL, max_e = NULL){
  # 1) Estimate event-study ATT on (matched) sample
  mw.dyn_p <- aggte(
    att_gt(
      yname = x,
      gname = "first_year_landslide",
      idname = "code",
      bstrap = TRUE,
      clustervars = "code",
      control_group = "nevertreated",
      base_period = "universal",
      tname = "year",
      data = data_in
    ),
    type  = "dynamic",
    na.rm = TRUE,
    min_e = if (is.null(min_e)) -Inf else min_e,
    max_e = if (is.null(max_e))  Inf else max_e
  )
  
  # 2) Tidy results
  est <- broom::tidy(mw.dyn_p) %>%
    dplyr::select(event.time, estimate, std.error, conf.low, conf.high) %>%
    dplyr::mutate(est = "Matched Sample")
  
  # 3) ATT (overall) with stars — fixed-point strings
  z_99 <- 2.576; z_95 <- 1.96; z_90 <- 1.645
  IC_99 <- mw.dyn_p$overall.att + c(-z_99 * mw.dyn_p$overall.se,  z_99 * mw.dyn_p$overall.se)
  IC_95 <- mw.dyn_p$overall.att + c(-z_95 * mw.dyn_p$overall.se,  z_95 * mw.dyn_p$overall.se)
  IC_90 <- mw.dyn_p$overall.att + c(-z_90 * mw.dyn_p$overall.se,  z_90 * mw.dyn_p$overall.se)
  
  att_str <- fmt_dec(mw.dyn_p$overall.att, 4)
  se_str  <- fmt_dec(mw.dyn_p$overall.se,  4)
  
  ATT_significance_p <-
    ifelse(all(IC_99 < 0) | all(IC_99 > 0), paste0(att_str, "***"),
           ifelse(all(IC_95 < 0) | all(IC_95 > 0), paste0(att_str, "**"),
                  ifelse(all(IC_90 < 0) | all(IC_90 > 0), paste0(att_str, "*"), att_str)))
  
  # 4) Small table grob (ATT and SE)
  dados_tabela <- data.table::data.table(`ATT` = c(ATT_significance_p, paste0("(", se_str, ")")))
  tabela_grob <- gridExtra::tableGrob(
    dados_tabela, rows = NULL,
    theme = gridExtra::ttheme_minimal(
      core    = list(fg_params = list(fontsize = 30)),
      colhead = list(fg_params = list(fontsize = 30, fontface = "bold")),
      rowhead = list(fg_params = list(fontsize = 30))
    )
  )
  
  # 5) Safe placement values (Y = estimate space; X = event-time space)
  lo <- suppressWarnings(min(est$conf.low[is.finite(est$conf.low)],  na.rm = TRUE)); if(!is.finite(lo)) lo <- -0.5
  hi <- suppressWarnings(max(est$conf.high[is.finite(est$conf.high)], na.rm = TRUE)); if(!is.finite(hi)) hi <-  0.5
  seq_est <- seq(lo, hi, length.out = 1000)
  table_pos_est <- stats::quantile(seq_est, probs = ifelse(abs(0 - lo) < abs(0 - hi), 0.91, 0.09))
  
  x_vals <- est$event.time
  pre_vals <- x_vals[is.finite(x_vals) & x_vals < 0]
  x_min <- if (length(pre_vals) >= 1) min(pre_vals) else min(x_vals, na.rm = TRUE)
  # ~3 periods width for the table band
  x_grob_xmin <- x_min + 0.5
  x_grob_xmax <- x_min + 3.5
  
  # 6) X-axis: add a small padding so CI caps are not clipped at the ends
  x_min_obs <- min(est$event.time, na.rm = TRUE)
  x_max_obs <- max(est$event.time, na.rm = TRUE)
  x_min_limit <- x_min_obs - 0.5
  x_max_limit <- x_max_obs + 0.5
  x_breaks    <- seq(x_min_obs, x_max_obs, by = 2)
  
  # 7) Build figure (VERTICAL error bars with visible caps)
  ggplot(est, aes(x = event.time, y = estimate)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  linewidth = 0.6, width = 0.6) +  # width controls the cap length
    geom_point(size = 2) +
    geom_hline(yintercept = 0) +     # zero on coefficient axis (Y)
    geom_vline(xintercept = -1) +    # reference at t = -1 (X)
    labs(x = "Period", y = "Coefficient", title = "") +
    scale_x_continuous(limits = c(x_min_limit, x_max_limit),
                       breaks = x_breaks,
                       expand = expansion(mult = c(0,0))) +
    coord_cartesian(clip = "off") +  # ensure caps and table aren't cut off
    theme_minimal() +
    theme(
      text = element_text(size = 25),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    # Table anchored inside the panel
    annotation_custom(grob = tabela_grob,
                      xmin = x_grob_xmin, xmax = x_grob_xmax,
                      ymin = table_pos_est, ymax = table_pos_est)
}

# Outcomes
vars_to_plot <- c("lurban_size", "sprawl_index")

# Robustness A: event-time window [-10, +10] on dados2
output_clip <- lapply(vars_to_plot, function(v)
  ggplot_paper(x = v, data_in = dados2, min_e = -10, max_e = 10)
)

# Robustness B: timeframe restricted to 2006–2020
dados2_window <- subset(dados2, year >= 2006 & year <= 2020)
output_window <- lapply(vars_to_plot, function(v)
  ggplot_paper(x = v, data_in = dados2_window)
)

# Save with your exact filenames
lurban_size_clip_path   <- paste0(path_output_git, "_graph_robustness_urban_size_time_frame10.jpg")
sprawl_index_clip_path  <- paste0(path_output_git, "_graph_robustness_sprawl_index_time_frame10.jpg.jpg")
ggsave(lurban_size_clip_path,  output_clip[[1]],  width = 20, height = 10, units = "in", dpi = 100)
ggsave(sprawl_index_clip_path, output_clip[[2]],  width = 20, height = 10, units = "in", dpi = 100)

lurban_size_window_path  <- paste0(path_output_git, "_graph_robustness_urban_size_timeframe2006.jpg")
sprawl_index_window_path <- paste0(path_output_git, "_graph_robustness_sprawl_timeframe2006.jpg.jpg")
ggsave(lurban_size_window_path,  output_window[[1]], width = 20, height = 10, units = "in", dpi = 100)
ggsave(sprawl_index_window_path, output_window[[2]], width = 20, height = 10, units = "in", dpi = 100)