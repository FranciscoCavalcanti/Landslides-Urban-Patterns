# Figure D.8: robustness, alternative DiD estimators: Gardner (2022) and Borusyak, Jaravel and Spiess (2021) (matched sample).
# Output: analysis/output/_graph_alternative_urban_size_{gardner,borusyak}.jpg

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

#### Alternative Estimators ####

ggplot_paper <- function(x, estimator){
  
  print(x)
  print(estimator)
  
  mw.dyn_p  <- event_study(
    data = dados2,
    yname = x,
    idname = "code",
    gname = "first_year_landslide",
    tname = "year",
    weights = NULL,
    estimator = estimator)
  
  mw.dyn_p <-  mw.dyn_p %>% 
    rename(overall.att = estimate,
           overall.se = std.error,
           event.time = term) %>% 
    mutate(conf.low  = overall.att - (1.96 * overall.se),
           conf.high = overall.att + (1.96 * overall.se)) %>% 
    select(c(event.time, overall.att, overall.se, conf.low, conf.high)) %>% 
    mutate(est = 'Matched Never-Treated Control Group')
  
  est <- mw.dyn_p
  
  value = c(abs(0 - summary(est$conf.low)[1]), abs(0 - summary(est$conf.high)[6]))
  sequencia <- seq(min(est$conf.low, na.rm = T), max(est$conf.high, na.rm = T), length.out = 1000)
  quantil_10 <- quantile(sequencia, probs = 0.18)
  quantil_75 <- quantile(sequencia, probs = 0.91)
  table_pos_y = ifelse(value[1] < value[2], quantil_75, quantil_10)  
  lengend_pos_y = ifelse(value[1] < value[2], 0.8, 0.1)
  
  z_99 <- 2.576
  z_95 <- 1.96
  z_90 <- 1.645
  
  pos_treatment <- mw.dyn_p[mw.dyn_p$event.time > 0, ]
  
  overall.att <- mean(pos_treatment$overall.att, na.rm = T)
  overall.se  <- mean(pos_treatment$overall.se, na.rm = T)
  
  
  IC_99 <-overall.att + c(-z_99 * overall.se, z_99 * overall.se)
  IC_95 <-overall.att + c(-z_95 * overall.se, z_95 * overall.se)
  IC_90 <-overall.att + c(-z_90 * overall.se, z_90 * overall.se)
  
  ATT_significance_p <- ifelse(all(IC_99 < 0) | all(IC_99 > 0), paste0(round(overall.att, 4), "***"),
                        ifelse(all(IC_95 < 0) | all(IC_95 > 0), paste0(round(overall.att, 4), "**"),
                        ifelse(all(IC_90 < 0) | all(IC_90 > 0), paste0(round(overall.att, 4), "*"),
                        paste(round(overall.att, 4)))))
  
  dados_tabela <- data.table::data.table(
    `ATT` = c(ATT_significance_p, paste0("(", round(overall.se, 4), ")"))
  )
  
  tabela_grob <- tableGrob(dados_tabela, 
                           rows = NULL, 
                           theme = ttheme_minimal(core = list(fg_params = list(fontsize = 30)), 
                                                  colhead = list(fg_params = list(fontsize = 30, fontface = "bold")), 
                                                  rowhead = list(fg_params = list(fontsize = 30)))) 
  
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
    scale_y_continuous(breaks = seq(-16, 16, by = 2)) +
    coord_flip() +
    theme_minimal() + 
    theme(text = element_text(size = 20),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = c(0.15, lengend_pos_y))
  
  graph <- graph + annotation_custom(grob = tabela_grob,
                                     xmin = table_pos_y,
                                     xmax = table_pos_y,
                                     ymin = -15, ymax = -11)
  
  return(graph)
  
}

output_lurban_size <- Map(x        = c('lurban_size', 'lurban_size'),
                          estimator = c('did2s', 'impute'),
                          ggplot_paper)

lurban_size_output_path <- paste0(path_output_git, "_graph_alternative_urban_size_gardner.jpg")
ggsave(lurban_size_output_path, output_lurban_size[[1]], width = 20, height = 10, units = "in", dpi = 300)

lurban_size_output_path <- paste0(path_output_git, "_graph_alternative_urban_size_borusyak.jpg")
ggsave(lurban_size_output_path, output_lurban_size[[2]], width = 20, height = 10, units = "in", dpi = 300)
