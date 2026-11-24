# Figure D.11: robustness, alternative time horizons: event window [-10, +10] and period 2006-2020 (matched sample).
# Output: analysis/output/_graph_robustness_urban_size_{time_frame10,timeframe2006}.jpg

library(dplyr)
library(MatchIt)
library(fixest)
library(did)
library(remotes)
library(did2s)
library(staggered)
library(ggplot2)
library(gridExtra)
library(broom)

set.seed(123)

path_input <- paste0(DROPBOX_PATH, "/build/input/")
path_output <- paste0(DROPBOX_PATH, "/build/output/")
path_output_git <- paste0(GITHUB_PATH, "/analysis/output/")


#### Data ####

dados <- readRDS(paste0(path_output, "database_panel.rds"))
#### Matching ####
dados_psm2 <- readRDS(paste0(path_output, "database_two_periods.rds")) %>% filter(year == 2000)
dados_psm2$region <- factor(dados_psm2$region, levels = c("Midwest","North","Northeast","South","Southeast"))
psm_covariates <- c("prec_verao", "prec_outono", "prec_inverno", "prec_primav", "altitude",
                    "temp_verao", "temp_inverno", "temp_primav", "temp_outono", "avg_tri",
                    "avg_slope", "region")
dados_psm4 <- dados_psm2[complete.cases(dados_psm2[, psm_covariates]), ]
PSM <- matchit(landslide ~ prec_verao + prec_outono + prec_inverno + prec_primav +
                 altitude + temp_verao + temp_inverno + temp_primav + temp_outono +
                 avg_tri + avg_slope + region,
               data = dados_psm4, method = "nearest")
psm <- match.data(PSM)

#### Matched sample ####


dados2 <- merge(dados, psm[, c("code", "weights")], by = "code")


fmt_dec <- function(x, k = 4) sprintf(paste0("%.", k, "f"), x)

ggplot_paper <- function(x, data_in = dados2, min_e = NULL, max_e = NULL){
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
  
  est <- broom::tidy(mw.dyn_p) %>%
    dplyr::select(event.time, estimate, std.error, conf.low, conf.high) %>%
    dplyr::mutate(est = "Matched Sample")
  
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
  
  dados_tabela <- data.table::data.table(`ATT` = c(ATT_significance_p, paste0("(", se_str, ")")))
  tabela_grob <- gridExtra::tableGrob(
    dados_tabela, rows = NULL,
    theme = gridExtra::ttheme_minimal(
      core    = list(fg_params = list(fontsize = 30)),
      colhead = list(fg_params = list(fontsize = 30, fontface = "bold")),
      rowhead = list(fg_params = list(fontsize = 30))
    )
  )
  
  lo <- suppressWarnings(min(est$conf.low[is.finite(est$conf.low)],  na.rm = TRUE)); if(!is.finite(lo)) lo <- -0.5
  hi <- suppressWarnings(max(est$conf.high[is.finite(est$conf.high)], na.rm = TRUE)); if(!is.finite(hi)) hi <-  0.5
  seq_est <- seq(lo, hi, length.out = 1000)
  table_pos_est <- stats::quantile(seq_est, probs = ifelse(abs(0 - lo) < abs(0 - hi), 0.91, 0.09))
  
  x_vals <- est$event.time
  pre_vals <- x_vals[is.finite(x_vals) & x_vals < 0]
  x_min <- if (length(pre_vals) >= 1) min(pre_vals) else min(x_vals, na.rm = TRUE)
  x_grob_xmin <- x_min + 0.5
  x_grob_xmax <- x_min + 3.5
  
  x_min_obs <- min(est$event.time, na.rm = TRUE)
  x_max_obs <- max(est$event.time, na.rm = TRUE)
  x_min_limit <- x_min_obs - 0.5
  x_max_limit <- x_max_obs + 0.5
  x_breaks    <- seq(x_min_obs, x_max_obs, by = 2)
  
  ggplot(est, aes(x = event.time, y = estimate)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  linewidth = 0.6, width = 0.6) +
    geom_point(size = 2) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = -1) +
    labs(x = "Period", y = "Coefficient", title = "") +
    scale_x_continuous(limits = c(x_min_limit, x_max_limit),
                       breaks = x_breaks,
                       expand = expansion(mult = c(0,0))) +
    coord_cartesian(clip = "off") +
    theme_minimal() +
    theme(
      text = element_text(size = 30),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank()
    ) +
    annotation_custom(grob = tabela_grob,
                      xmin = x_grob_xmin, xmax = x_grob_xmax,
                      ymin = table_pos_est, ymax = table_pos_est)
}

vars_to_plot <- c("lurban_size")

output_clip <- lapply(vars_to_plot, function(v)
  ggplot_paper(x = v, data_in = dados2, min_e = -10, max_e = 10)
)

dados2_window <- subset(dados2, year >= 2006 & year <= 2020)
output_window <- lapply(vars_to_plot, function(v)
  ggplot_paper(x = v, data_in = dados2_window)
)

lurban_size_clip_path   <- paste0(path_output_git, "_graph_robustness_urban_size_time_frame10.jpg")
ggsave(lurban_size_clip_path,  output_clip[[1]],  width = 20, height = 10, units = "in", dpi = 100)

lurban_size_window_path  <- paste0(path_output_git, "_graph_robustness_urban_size_timeframe2006.jpg")
ggsave(lurban_size_window_path,  output_window[[1]], width = 20, height = 10, units = "in", dpi = 100)