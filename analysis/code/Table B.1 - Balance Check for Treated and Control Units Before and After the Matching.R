# Load required libraries
library(dplyr)       # Data manipulation
library(fixest)      # Fixed effects estimation
library(did)         # Difference-in-differences estimation
library(remotes)     # Installing packages from remote repositories
library(did2s)       # Alternative DiD estimators
library(MatchIt)     # Propensity score matching
library(pscl)        # Additional model utilities


path_input <- paste0(DROPBOX_PATH, "/build/input/")
path_output <- paste0(DROPBOX_PATH, "/build/output/") 
path_output_git <- paste0(GITHUB_PATH, "/analysis/output/") 

#### Open Database for Pre-Processing PSM ####

dados_psm <- readRDS(paste0(path_output,"database_two_periods.rds"))

### Restricting the Sample to the pre-treatment period ###

dados_psm2 <- dados_psm %>% filter(year == 2000)

dados_psm4 <- dados_psm2[complete.cases(dados_psm2$avg_income, 
                                        dados_psm2$urban_size, 
                                        dados_psm2$sprawl_index), ]

dados_psm4$region <- factor(dados_psm4$region, levels = c("Midwest","North","Northeast","South","Southeast"))
dados_psm4 <- cbind(dados_psm4, as.data.frame(model.matrix(~ region - 1, dados_psm4)))


#### Estimating the PSM #### 

attach(dados_psm4)

PSM <- matchit(landslide ~ prec_verao + prec_outono + prec_inverno + prec_primav +
                 altitude + temp_verao + temp_inverno + temp_primav + temp_outono +
                 avg_tri + region + prop_urban_size_high_risk_max_30, data = dados_psm4, method = "nearest")
summary(PSM)

m.data <- match.data(PSM)


#### Balance Table Function ####

balance_table_simple <- function(dados_all, dados_matched, treat, vars, weight_col = NULL, digits = 3) {
  stars <- function(p) if (is.na(p)) "" else if (p < .001) "***" else if (p < .01) "**" else if (p < .05) "*" else ""
  w <- if (!is.null(weight_col) && weight_col %in% names(dados_matched)) dados_matched[[weight_col]] else rep(1, nrow(dados_matched))
  
  res <- lapply(vars, function(v) {
    xa_all <- dados_all[[v]][dados_all[[treat]] == 1]
    xb_all <- dados_all[[v]][dados_all[[treat]] == 0]
    mt_all <- mean(xa_all, na.rm = TRUE)
    mc_all <- mean(xb_all, na.rm = TRUE)
    d_all  <- mt_all - mc_all
    p_all  <- tryCatch(t.test(xa_all, xb_all)$p.value, error = function(e) NA_real_)
    
    t_m <- dados_matched[[treat]]
    y_m <- dados_matched[[v]]
    mt_m <- weighted.mean(y_m[t_m == 1], w[t_m == 1], na.rm = TRUE)
    mc_m <- weighted.mean(y_m[t_m == 0], w[t_m == 0], na.rm = TRUE)
    d_m  <- mt_m - mc_m
    p_m  <- tryCatch(summary(lm(y_m ~ t_m, weights = w))$coef["t_m", "Pr(>|t|)"], error = function(e) NA_real_)
    
    data.frame(
      Variable                       = v,
      Treated_Mean                   = round(mt_m, digits),
      Control_Mean_Overall           = round(mc_all, digits),
      Control_Mean_Matched           = round(mc_m, digits),
      Diff_T_minus_Control_Overall   = paste0(round(d_all, digits),  stars(p_all)),
      Diff_T_minus_Control_Matched   = paste0(round(d_m,  digits),  stars(p_m)),
      check.names = FALSE
    )
  })
  do.call(rbind, res)
}




######## Balance Table #########

vars <- c("prec_verao","prec_outono","prec_inverno","prec_primav","altitude",
           "temp_verao","temp_inverno","temp_primav","temp_outono","avg_tri",
           "prop_urban_size_high_risk_max_30","regionMidwest","regionNorth",
           "regionNortheast","regionSouth","regionSoutheast")

tab <- balance_table_simple(dados_psm4, m.data, treat = "landslide", vars = vars, weight_col = "weights")

writexl::write_xlsx(tab, file.path(path_output_git, "balance_table.xlsx"))



