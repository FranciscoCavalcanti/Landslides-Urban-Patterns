# Figure D.1: robustness, full sample of municipalities (no matching).
# Output: analysis/output/_graph_robustness_broader_sample_urban_size.jpg

library(dplyr)
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

#### Matched sample ####


#### Estimation ####

ggplot_paper <- function(x){
  
  mw.dyn_np <- aggte(
    att_gt(yname = x,
           gname = "first_year_landslide",
           idname = "code",
           bstrap = TRUE,
           base_period = "universal",
           tname = "year",
           clustervars = 'code',
           data = dados), type = "dynamic")
  
  est_np <- broom::tidy(mw.dyn_np) %>% select(c(event.time, estimate, std.error, conf.low, conf.high)) %>% 
    mutate(est = 'Broader Sample')

  est <- est_np
  

  z_99 <- 2.576
  z_95 <- 1.96
  z_90 <- 1.645
  
  IC_99 <- mw.dyn_np$overall.att + c(-z_99 * mw.dyn_np$overall.se, z_99 * mw.dyn_np$overall.se)
  IC_95 <- mw.dyn_np$overall.att + c(-z_95 * mw.dyn_np$overall.se, z_95 * mw.dyn_np$overall.se)
  IC_90 <- mw.dyn_np$overall.att + c(-z_90 * mw.dyn_np$overall.se, z_90 * mw.dyn_np$overall.se)
  
  ATT_significance_np <- ifelse(all(IC_99 < 0) | all(IC_99 > 0), paste0(round(mw.dyn_np$overall.att, 4), "***"),
                         ifelse(all(IC_95 < 0) | all(IC_95 > 0), paste0(round(mw.dyn_np$overall.att, 4), "**"),
                         ifelse(all(IC_90 < 0) | all(IC_90 > 0), paste0(round(mw.dyn_np$overall.att, 4), "*"),
                         paste(round(mw.dyn_np$overall.att, 4)))))
  
  dados_tabela <- data.table::data.table(
    `ATT` = c(ATT_significance_np, paste0("(", round(mw.dyn_np$overall.se, 4), ")"))
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
    theme(text = element_text(size = 30),
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

output <- lapply(c('lurban_size'), ggplot_paper)

#### Save ####

lurban_size_output_path <- paste0(path_output_git, "_graph_robustness_broader_sample_urban_size.jpg")
ggsave(lurban_size_output_path, output[[1]], width = 20, height = 10, units = "in", dpi = 100)
