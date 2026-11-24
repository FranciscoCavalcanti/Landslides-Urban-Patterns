# Figure A.3: group-specific ATTs on log urban area (Callaway and Sant'Anna, matched sample).
# Output: analysis/output/_graph_appendix_group_hetero_urban_size.jpg

library(dplyr)
library(MatchIt)
library(fixest)
library(did)
library(remotes)
library(did2s)
library(staggered)
library(ggplot2)
library(gridExtra)

set.seed(123)

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

#### Estimation ####

ggplot_paper <- function(x){
  
  mw.dyn_p <- att_gt(yname = x,
                     gname = "first_year_landslide",
                     idname = "code",
                     bstrap = TRUE,
                     base_period="universal",
                     tname = "year",
                     clustervars = 'code',
                     data = dados2)
  
  est_p <- broom::tidy(mw.dyn_p) %>% select(c(event.time = time, event.group = group,
                                              estimate = estimate, std.error, conf.low, conf.high)) %>% 
    mutate(est = 'Matched Sample')
  
  est <- est_p
  est$event.point <- est$event.group
  est$event.group <- paste0("Group ", est$event.group)
  
  plot <- function(x){
    
    df <- est %>% 
      filter(event.group == x)
    
    event.point <- unique(df$event.point)
    
    ggplot(data = df, aes(y = event.time, x = estimate, color = est, linetype = est)) +
      geom_pointrange(
        aes(xmax = conf.high, xmin = conf.low),
        linewidth = 0.5, position = position_dodge(width = 0.5), linetype = 'blank'
      ) +
      geom_errorbar(
        aes(xmax = conf.high, xmin = conf.low),
        linewidth = 0.5, width = 0.5, position = position_dodge(width = 0.5)
      ) +
      geom_hline(yintercept = event.point-1) +
      geom_vline(xintercept = 0) +
      labs(x = 'Coefficient', y = 'Period',
           color = "", linetype = "",
           subtitle = x) +
      scale_color_manual(name = "", values = c("black", "grey20"),
                         labels = c('Broader Sample', 'Matched Sample')) +
      scale_linetype_manual(name = "", values = c("solid", "dashed"),
                            labels = c('Broader Sample', 'Matched Sample')) + 
      scale_y_continuous(breaks = seq(2004, 2020, by = 2)) +
      coord_flip() +
      theme_minimal() + 
      theme(legend.key.width = unit(1.5, "cm"),
            legend.key.height = unit(1, "cm"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            legend.position = 'bottom')
  }
  
  graph <- lapply(unique(est$event.group), plot) %>% 
    ggpubr::ggarrange(plotlist = ., nrow = 5, ncol = 4,
                      common.legend = T, legend = "bottom")
  
  return(graph)
}

output <- lapply(c('lurban_size'), ggplot_paper)

#### Save ####

lurban_size_output_path <- paste0(path_output_git, "_graph_appendix_group_hetero_urban_size.jpg")
ggsave(lurban_size_output_path, output[[1]], width = 20, height = 10, units = "in", dpi = 100)

