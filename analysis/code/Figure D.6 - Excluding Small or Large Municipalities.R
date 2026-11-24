# Figure D.6: robustness, excluding municipalities with 2000 population below 5,000 or above 500,000 (matched sample).
# Output: analysis/output/_graph_robustness_urban_size_{5000p,500000p}.jpg

library(dplyr)
library(MatchIt)
library(did)
library(ggplot2)
library(gridExtra)
library(grid)
library(broom)
library(data.table)
library(broom)

set.seed(123)

path_input <- paste0(DROPBOX_PATH, "/build/input/")
path_output <- paste0(DROPBOX_PATH, "/build/output/")
path_output_git <- paste0(GITHUB_PATH, "/analysis/output/")


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

dados2 <- merge(dados, psm[, c("code", "weights")], by = "code")

# Population in 2000 (census), as described in the text: exclusions are municipality-level
pop2000 <- dados_psm2 %>% dplyr::select(code, pop2000 = population)
dados2 <- dplyr::left_join(dados2, pop2000, by = "code")


ggplot_paper_data <- function(data, x){
  mw.dyn_p <- aggte(
    att_gt(
      yname       = x,
      gname       = "first_year_landslide",
      idname      = "code",
      bstrap      = TRUE,
      clustervars = "code",
      base_period = "universal",
      tname       = "year",
      data        = data
    ),
    type = "dynamic"
  )
  
  est <- broom::tidy(mw.dyn_p) |>
    dplyr::select(event.time, estimate, std.error, conf.low, conf.high) |>
    dplyr::mutate(est = "Matched Sample") |>
    dplyr::filter(is.finite(estimate) | is.finite(conf.low) | is.finite(conf.high))
  
  if (nrow(est) == 0) {
    stop(paste0("No finite points to plot for '", x, "'. Check aggte/att_gt output."))
  }
  
  xr <- range(c(est$estimate, est$conf.low, est$conf.high), na.rm = TRUE)
  if (!is.finite(xr[1]) || !is.finite(xr[2]) || xr[1] == xr[2]) xr <- c(-0.1, 0.1)
  
  z_99 <- 2.576; z_95 <- 1.96; z_90 <- 1.645
  att  <- mw.dyn_p$overall.att
  se   <- mw.dyn_p$overall.se
  
  ATT_significance_p <- {
    if (is.finite(att) && is.finite(se) && se > 0) {
      IC_99 <- att + c(-z_99*se, z_99*se)
      IC_95 <- att + c(-z_95*se, z_95*se)
      IC_90 <- att + c(-z_90*se, z_90*se)
      if (all(IC_99 < 0) || all(IC_99 > 0)) paste0(round(att, 4), "***")
      else if (all(IC_95 < 0) || all(IC_95 > 0)) paste0(round(att, 4), "**")
      else if (all(IC_90 < 0) || all(IC_90 > 0)) paste0(round(att, 4), "*")
      else paste(round(att, 4))
    } else {
      "NA"
    }
  }
  
  dados_tabela <- data.table::data.table(
    `ATT` = c(ATT_significance_p,
              paste0("(", ifelse(is.finite(se), round(se, 4), "NA"), ")"))
  )
  
  tabela_grob <- gridExtra::tableGrob(
    dados_tabela,
    rows  = NULL,
    theme = gridExtra::ttheme_minimal(
      core    = list(fg_params = list(fontsize = 30)),
      colhead = list(fg_params = list(fontsize = 30, fontface = "bold")),
      rowhead = list(fg_params = list(fontsize = 30))
    )
  )
  
  lower_min <- suppressWarnings(min(est$conf.low,  na.rm = TRUE))
  upper_max <- suppressWarnings(max(est$conf.high, na.rm = TRUE))
  if (!is.finite(lower_min)) lower_min <- xr[1]
  if (!is.finite(upper_max)) upper_max <- xr[2]
  
  value_left  <- abs(0 - lower_min)
  value_right <- abs(0 - upper_max)
  
  seq_safe  <- seq(from = xr[1], to = xr[2], length.out = 1000)
  q18       <- stats::quantile(seq_safe, probs = 0.18)
  q91       <- stats::quantile(seq_safe, probs = 0.91)
  table_pos_x  <- ifelse(value_left < value_right, q91, q18)
  legend_pos_y <- ifelse(value_left < value_right, 0.75, 0.10)
  
  g <- ggplot2::ggplot(est, aes(y = event.time, x = estimate)) +
    ggplot2::geom_pointrange(
      aes(xmin = conf.low, xmax = conf.high),
      linewidth = 0.5,
      position  = ggplot2::position_dodge(width = 0.5),
      linetype  = "blank",
      na.rm     = TRUE
    ) +
    ggplot2::geom_errorbar(
      aes(xmin = conf.low, xmax = conf.high),
      linewidth = 0.5, width = 0.5,
      position  = ggplot2::position_dodge(width = 0.5),
      na.rm     = TRUE
    ) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::geom_hline(yintercept = -1) +
    ggplot2::labs(x = "Coefficient", y = "Period", color = "", linetype = "", title = "") +
    ggplot2::scale_y_continuous(breaks = seq(-16, 16, by = 2)) +
    ggplot2::coord_flip(xlim = xr) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text               = ggplot2::element_text(size = 25),
      legend.text        = ggplot2::element_text(size = 25),
      legend.title       = ggplot2::element_text(size = 25),
      legend.key.width   = grid::unit(1.5, "cm"),
      legend.key.height  = grid::unit(1,   "cm"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      legend.position    = c(0.125, legend_pos_y)
    )
  
  g <- g + ggplot2::annotation_custom(
    grob = tabela_grob,
    xmin = table_pos_x, xmax = table_pos_x,
    ymin = -15, ymax = -11
  )
  
  return(g)
}

# ROBUSTNESS A: Exclude population in 2000 < 5,000 (keep NAs)
dados2_5000p <- subset(dados2, is.na(pop2000) | pop2000 >= 5000)

plots_5000p <- lapply(
  c("lurban_size"),
  function(v) ggplot_paper_data(dados2_5000p, v)
)

ggsave(
  filename = paste0(path_output_git, "_graph_robustness_urban_size_5000p.jpg"),
  plot     = plots_5000p[[1]],
  width    = 20, height = 10, units = "in", dpi = 100
)

# ROBUSTNESS B: Exclude population in 2000 > 500,000 (keep NAs)
dados2_500000p <- subset(dados2, is.na(pop2000) | pop2000 <= 500000)

plots_500000p <- lapply(
  c("lurban_size"),
  function(v) ggplot_paper_data(dados2_500000p, v)
)

ggsave(
  filename = paste0(path_output_git, "_graph_robustness_urban_size_500000p.jpg"),
  plot     = plots_500000p[[1]],
  width    = 20, height = 10, units = "in", dpi = 100
)

length(setdiff(unique(dados2$code), unique(dados2$code[is.na(dados2$pop2000) | dados2$pop2000 >= 5000])))

length(setdiff(unique(dados2$code), unique(dados2$code[is.na(dados2$pop2000) | dados2$pop2000 <= 500000])))
                                      