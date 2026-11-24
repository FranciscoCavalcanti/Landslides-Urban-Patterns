# Figure D.18: alternative treatment, extreme rainfall shocks in mountainous municipalities (intention-to-treat; matched sample).
# Output: analysis/output/_graph_robustness_extreme_rainfall_mountainous_urban_size.jpg

library(dplyr)
library(MatchIt)
library(fixest)
library(did)
library(remotes)
library(did2s)
library(staggered)
library(ggplot2)
library(gridExtra)
library(data.table)


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

dados1 <- merge(dados, psm[, c("code", "weights")], by = "code")


#### Creating a Interaction of Heavy Rain and Moutain Area #####

setDT(dados1); setkey(dados1, code, year)

dados1[, `:=`(
  int_hmount_x_rain1d = Instersection_Moutain * chuva_extrema_1d_100mm
)]

vari <- c("int_hmount_x_rain1d", "landslide_dum")
new <- paste0(vari, "_ff")

dados1$landslide_dum <- ifelse(dados1$landslide_event %in% 1,1,0)
dados1[, (new) := lapply(.SD, cummax), by = code, .SDcols = vari]


setDT(dados1)

first_int <- dados1[, .(
  first_int_1d = if (any(int_hmount_x_rain1d == 1, na.rm = TRUE))
    as.integer(min(year[int_hmount_x_rain1d == 1], na.rm = TRUE)) else 0L
), by = code]

dados2 <- left_join(dados1, first_int, by = "code")


ggplot_paper <- function(x){
  
  mw.dyn_p <- aggte(
    att_gt(
      yname         = x,
      gname         = "first_int_1d",
      idname        = "code",
      tname         = "year",
      control_group = "nevertreated",
      base_period   = "universal",
      bstrap        = TRUE,
      clustervars   = "code",
      data          = dados2
    ),
    type  = "dynamic",
    na.rm = TRUE
  )
  
  est <- broom::tidy(mw.dyn_p) |>
    select(event.time, estimate, std.error, conf.low, conf.high) |>
    mutate(est = "ITT")
  
  z_99 <- 2.576; z_95 <- 1.96; z_90 <- 1.645
  
  IC_99 <- mw.dyn_p$overall.att + c(-z_99 * mw.dyn_p$overall.se,  z_99 * mw.dyn_p$overall.se)
  IC_95 <- mw.dyn_p$overall.att + c(-z_95 * mw.dyn_p$overall.se,  z_95 * mw.dyn_p$overall.se)
  IC_90 <- mw.dyn_p$overall.att + c(-z_90 * mw.dyn_p$overall.se,  z_90 * mw.dyn_p$overall.se)
  
  ATT_significance_p <- ifelse(all(IC_99 < 0) | all(IC_99 > 0), paste0(round(mw.dyn_p$overall.att, 4), "***"),
                               ifelse(all(IC_95 < 0) | all(IC_95 > 0), paste0(round(mw.dyn_p$overall.att, 4), "**"),
                                      ifelse(all(IC_90 < 0) | all(IC_90 > 0), paste0(round(mw.dyn_p$overall.att, 4), "*"),
                                             paste(round(mw.dyn_p$overall.att, 4)))))
  
  dados_tabela <- data.table::data.table(
    `ATT` = c(ATT_significance_p, paste0("(", round(mw.dyn_p$overall.se, 4), ")"))
  )
  tabela_grob <- tableGrob(
    dados_tabela,
    rows = NULL,
    theme = ttheme_minimal(
      core    = list(fg_params = list(fontsize = 30)),
      colhead = list(fg_params = list(fontsize = 30, fontface = "bold")),
      rowhead = list(fg_params = list(fontsize = 30))
    )
  )
  
  value <- c(abs(0 - min(est$conf.low,  na.rm = TRUE)),
             abs(0 - max(est$conf.high, na.rm = TRUE)))
  sequencia   <- seq(min(est$conf.low[is.finite(est$conf.low)],  na.rm = TRUE),
                     max(est$conf.high[is.finite(est$conf.high)], na.rm = TRUE),
                     length.out = 1000)
  quantil_10  <- quantile(sequencia, probs = 0.18, names = FALSE)
  quantil_75  <- quantile(sequencia, probs = 0.91, names = FALSE)
  table_pos_y <- ifelse(value[1] < value[2], quantil_75, quantil_10)
  lengend_pos_y <- ifelse(value[1] < value[2], 0.75, 0.10)
  
  graph <- ggplot(data = est, aes(y = event.time, x = estimate)) +
    geom_pointrange(aes(xmax = conf.high, xmin = conf.low),
                    linewidth = 0.5, position = position_dodge(width = 0.5),
                    linetype = "blank") +
    geom_errorbar(aes(xmax = conf.high, xmin = conf.low),
                  linewidth = 0.5, width = 0.5, position = position_dodge(width = 0.5)) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = -1) +
    labs(x = "Coefficient", y = "Period", color = "", linetype = "", title = "") +
    scale_y_continuous(breaks = seq(-16, 16, by = 2)) +
    coord_flip() +
    theme_minimal() +
    theme(
      text = element_text(size = 30),
      legend.text = element_text(size = 25),
      legend.title = element_text(size = 25),
      legend.key.width = unit(1.5, "cm"),
      legend.key.height = unit(1, "cm"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = c(0.125, lengend_pos_y)
    )
  
  graph + annotation_custom(
    grob = tabela_grob,
    xmin = table_pos_y, xmax = table_pos_y,
    ymin = -15, ymax = -11
  )
}

outcomes <- c("lurban_size")
output   <- lapply(outcomes, ggplot_paper)

ggsave(paste0(path_output_git, "_graph_robustness_extreme_rainfall_mountainous_urban_size.jpg"),
       output[[1]], width = 20, height = 10, units = "in", dpi = 100)

corr <- cor(
  dados2$int_hmount_x_rain1d_ff,
  dados2$landslide_dum_ff,
  use = "complete.obs"
)
print(corr)
