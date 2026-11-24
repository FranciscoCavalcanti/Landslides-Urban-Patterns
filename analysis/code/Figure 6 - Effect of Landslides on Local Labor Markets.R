# Figure 6: effect of landslides on local labor markets (Callaway and Sant'Anna, matched sample).
# Per-capita outcomes use the census population log-linearly interpolated to 2003-2020.
# Output: analysis/output/_graph_mechanisms_{firms_per_capita,employment_per_capita,income_per_capita,wage}.jpg

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

path_output <- paste0(DROPBOX_PATH, "/build/output/")
path_output_git <- paste0(GITHUB_PATH, "/analysis/output/")


#### Data ####
dados <- readRDS(paste0(path_output,"database_panel.rds"))

# Population denominator: census total population (2000, 2010, 2022), log-linearly interpolated to 2003-2020
cens <- readRDS(paste0(path_output,"database_three_periods.rds")) %>% select(code, year, population)
interp_pop <- function(d) {
  d <- d[order(d$year), ]
  exp(approx(d$year, log(d$population), xout = 2003:2020, rule = 2)$y)
}
ip <- do.call(rbind, lapply(split(cens, cens$code), function(d)
  data.frame(code = d$code[1], year = 2003:2020, pop_interp = interp_pop(d))))
dados <- merge(dados %>% select(-population), ip, by = c("code", "year"), all.x = TRUE)
dados$population <- dados$pop_interp

#### Variables ####
# Denominator of the per-capita outcomes: interpolated census population
dados$firms_per_capita      <- log(dados$firms/dados$population)       # establishments / population
dados$employment_per_capita <- log(dados$employment/dados$population)  # employment / population
dados$income_per_capita     <- log(dados$income/dados$population)      # earnings / population
dados$wage                  <- log(dados$income/dados$employment)      # earnings / employment (average wage)

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

dados2 <- merge(dados, psm[,c("code","weights")], by="code")

#### Estimation ####

ggplot_paper <- function(x){
  
  print(x)
  
  mw.dyn_p <- aggte(
    att_gt(yname = x,
           gname = "first_year_landslide",
           idname = "code",
           bstrap = TRUE,
           base_period="universal",
           tname = "year",
           clustervars = 'code',
           data = dados2), type = "dynamic")
  
  est_p <- broom::tidy(mw.dyn_p) %>% select(c(event.time, estimate,std.error,conf.low,conf.high)) %>% 
    mutate(est = 'Matched Sample')
  
  
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
  
  digits <- ifelse(grepl("employment|firms", x), 3, 4)

  ATT_significance_p <- ifelse(all(IC_99 < 0) | all(IC_99 > 0), paste0(formatC(mw.dyn_p$overall.att, format = "f", digits = digits), "***"),
                        ifelse(all(IC_95 < 0) | all(IC_95 > 0), paste0(formatC(mw.dyn_p$overall.att, format = "f", digits = digits), "**"),
                        ifelse(all(IC_90 < 0) | all(IC_90 > 0), paste0(formatC(mw.dyn_p$overall.att, format = "f", digits = digits), "*"),
                        paste(formatC(mw.dyn_p$overall.att, format = "f", digits = digits)))))
  
  
  dados_tabela <- data.table::data.table(
    `ATT` = c(ATT_significance_p, paste0("(",formatC(mw.dyn_p$overall.se, format = "f", digits = digits),")"))
  )
  
  value = c(abs(0 - summary(est$conf.low)[1]),abs(0 - summary(est$conf.high)[6]))
  sequencia <- seq(min(est$conf.low, na.rm = T), max(est$conf.high, na.rm = T), length.out = 1000)
  quantil_10 <- quantile(sequencia, probs = 0.18)
  quantil_75 <- quantile(sequencia, probs = 0.91)
  
  
  table_pos_y = ifelse(value[1] < value[2],quantil_75,quantil_10)  
  lengend_pos_y = ifelse(value[1] < value[2],0.75,0.1)
  
  
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


output <- lapply(c('firms_per_capita','employment_per_capita','income_per_capita','wage'), ggplot_paper)


#### Save ####

ggsave(paste0(path_output_git, "_graph_mechanisms_firms_per_capita.jpg"), output[[1]], width = 20, height = 10, units = "in", dpi = 100)
ggsave(paste0(path_output_git, "_graph_mechanisms_employment_per_capita.jpg"), output[[2]], width = 20, height = 10, units = "in", dpi = 100)
ggsave(paste0(path_output_git, "_graph_mechanisms_income_per_capita.jpg"), output[[3]], width = 20, height = 10, units = "in", dpi = 100)
ggsave(paste0(path_output_git, "_graph_mechanisms_wage.jpg"), output[[4]], width = 20, height = 10, units = "in", dpi = 100)
