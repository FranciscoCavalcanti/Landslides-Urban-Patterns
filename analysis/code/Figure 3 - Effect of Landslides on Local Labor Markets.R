library(dplyr)
library(fixest)
library(did)
library(remotes)
library(did2s)
library(staggered)
library(ggplot2)
library(gridExtra)
library(broom)

##### Set Seed #######

set.seed(123)

path_input <- paste0(DROPBOX_PATH, "/build/input/")
path_output <- paste0(DROPBOX_PATH, "/build/output/") 
path_output_git <- paste0(GITHUB_PATH, "/analysis/output/") 

#### Open Databases ####
dados <- readRDS(paste0(path_output,"database_panel.rds"))

#### Creating Variables #####

dados$firms      <- log(dados$firms)
dados$wage_total     <- log(dados$income)
dados$wage_per_employee     <- log(dados$income/dados$employment)
dados$employment <- log(dados$employment)

psm <- readRDS(paste0(path_output,"restricted_PSM_database.rds"))

#### Selecting the Treated/Control Group Using PSM ####

dados2 <- merge(dados, psm[,c("code","weights")], by="code")

#### Mechanism: Labor Market - Staggered DiD with PSM ####

ggplot_paper <- function(x){
  
  print(x)
  
  ## Paired results
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
  est_p <- broom::tidy(mw.dyn_p) %>% select(c(event.time, estimate,std.error,conf.low,conf.high)) %>% 
    mutate(est = 'Matched Sample')
  
  
  # Graph
  est <- est_p
  
  title <- ifelse(grepl('urban_size',x),"A) Effect of Landslides on Urban Size",
           ifelse(grepl('sprawl',x),"B) Effect of Landslides on Sprawl Index",
           ifelse(grepl('water_size',x),"A) Effect of Landslides on Water Surface",
           ifelse(grepl('forest_size',x),"B) Effect of Landslides on Forest Formation",
           ifelse(grepl('natural_size',x),"C) Effect of Landslides on Other Natural Land Uses",NA)))))
  
  
  z_99 <- 2.576
  z_95 <- 1.96
  z_90 <- 1.645
  
  
  IC_99 <- mw.dyn_p$overall.att + c(-z_99 * mw.dyn_p$overall.se, z_99 * mw.dyn_p$overall.se)
  IC_95 <- mw.dyn_p$overall.att + c(-z_95 * mw.dyn_p$overall.se, z_95 * mw.dyn_p$overall.se)
  IC_90 <- mw.dyn_p$overall.att + c(-z_90 * mw.dyn_p$overall.se, z_90 * mw.dyn_p$overall.se)
  
  ATT_significance_p <- ifelse(all(IC_99 < 0) | all(IC_99 > 0), paste0(round(mw.dyn_p$overall.att,4), "***"),
                        ifelse(all(IC_95 < 0) | all(IC_95 > 0), paste0(round(mw.dyn_p$overall.att,4), "**"),
                        ifelse(all(IC_90 < 0) | all(IC_90 > 0), paste0(round(mw.dyn_p$overall.att,4), "*"),
                        paste(round(mw.dyn_p$overall.att,4)))))
  
  
  dados_tabela <- data.table::data.table(
    `ATT` = c(ATT_significance_p, paste0("(",round(mw.dyn_p$overall.se,4),")"))
  )
  
  value = c(abs(0 - summary(est$conf.low)[1]),abs(0 - summary(est$conf.high)[6]))
  sequencia <- seq(min(est$conf.low, na.rm = T), max(est$conf.high, na.rm = T), length.out = 1000)
  quantil_10 <- quantile(sequencia, probs = 0.18)
  quantil_75 <- quantile(sequencia, probs = 0.91)
  
  
  table_pos_y = ifelse(value[1] < value[2],quantil_75,quantil_10)  
  lengend_pos_y = ifelse(value[1] < value[2],0.75,0.1)
  
  
  tabela_grob <- tableGrob(dados_tabela, 
                           rows = NULL, 
                           theme = ttheme_minimal(core = list(fg_params = list(fontsize = 30)), # Font size for the table body
                                                  colhead = list(fg_params = list(fontsize = 30, fontface = "bold")), # Font size and bold for the header
                                                  rowhead = list(fg_params = list(fontsize = 30)))) # Font size for row headers, if any
  
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
    theme(text = element_text(size = 25),
          legend.text = element_text(size = 25),
          legend.title = element_text(size = 25),
          legend.key.width = unit(1.5, "cm"),
          legend.key.height = unit(1, "cm"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          legend.position = c(0.125, lengend_pos_y))
  
  
  graph <- graph + annotation_custom(grob=tabela_grob,
                                     xmin=table_pos_y,
                                     xmax=table_pos_y,
                                     ymin=-12, ymax=-8)
  return(graph)
  
}


# Using loop
output <- lapply(c('employment','firms','wage_total','wage_per_employee'),
                 ggplot_paper)


#### Saving Results ####

employment_output_path <- paste0(path_output_git, "_graph_mechanisms_employment.jpg")
ggsave(employment_output_path, output[[1]], width = 20, height = 10, units = "in", dpi = 100)

firms_output_path <- paste0(path_output_git, "_graph_mechanisms_firms.jpg")
ggsave(firms_output_path, output[[2]], width = 20, height = 10, units = "in", dpi = 100)

wage_total_output_path <- paste0(path_output_git, "_graph_mechanisms_wage_total.jpg")
ggsave(wage_total_output_path, output[[3]], width = 20, height = 10, units = "in", dpi = 100)

wage_per_employee_output_path <- paste0(path_output_git, "_graph_mechanisms_wage_per_employee.jpg")
ggsave(wage_per_employee_output_path, output[[4]], width = 20, height = 10, units = "in", dpi = 100)
