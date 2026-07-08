# Load required libraries
library(dplyr)       # Data manipulation
library(fixest)      # Fixed effects estimation
library(did)         # Difference-in-differences estimation
library(remotes)     # Installing packages from remote repositories
library(did2s)       # Alternative DiD estimators
library(MatchIt)     # Propensity score matching
if (requireNamespace("pscl", quietly = TRUE)) library(pscl) # Additional model utilities


path_input <- paste0(DROPBOX_PATH, "/build/input/")
path_output <- paste0(DROPBOX_PATH, "/build/output/") 
path_output_git <- paste0(GITHUB_PATH, "/analysis/output/") 

#### Open Database for Pre-Processing PSM ####

dados_psm <- readRDS(paste0(path_output,"database_two_periods.rds"))

### Restricting the Sample to the pre-treatment period ###

dados_psm2 <- dados_psm %>%
  filter(year == 2000)

dados_psm2$region <- factor(dados_psm2$region, levels = c("Midwest","North","Northeast","South","Southeast"))
dados_psm2 <- cbind(dados_psm2, as.data.frame(model.matrix(~ region - 1, dados_psm2)))

psm_covariates <- c(
  "prec_verao", "prec_outono", "prec_inverno", "prec_primav",
  "altitude", "temp_verao", "temp_inverno", "temp_primav", "temp_outono",
  "avg_tri", "region"
)

dados_psm4 <- dados_psm2[complete.cases(dados_psm2[, psm_covariates]), ]


#### Estimating the PSM #### 

attach(dados_psm4)

PSM <- matchit(landslide ~ prec_verao + prec_outono + prec_inverno + prec_primav +
                 altitude + temp_verao + temp_inverno + temp_primav + temp_outono +
                 avg_tri + region,
               data = dados_psm4, method = "nearest")
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
           "regionMidwest","regionNorth",
           "regionNortheast","regionSouth","regionSoutheast")

tab <- balance_table_simple(dados_psm2, m.data, treat = "landslide", vars = vars, weight_col = "weights")

#### Save LaTeX Table ####

variable_labels <- c(
  prec_verao = "Avg. Precipitation (Summer)",
  prec_outono = "Avg. Precipitation (Fall)",
  prec_inverno = "Avg. Precipitation (Winter)",
  prec_primav = "Avg. Precipitation (Spring)",
  temp_verao = "Avg. Temperature (Summer)",
  temp_inverno = "Avg. Temperature (Winter)",
  temp_primav = "Avg. Temperature (Spring)",
  temp_outono = "Avg. Temperature (Fall)",
  altitude = "Avg. Altitude (meters)",
  avg_tri = "Terrain Ruggedness Index",
  regionMidwest = "Region: Midwest (\\%)",
  regionNorth = "Region: North (\\%)",
  regionNortheast = "Region: Northeast (\\%)",
  regionSouth = "Region: South (\\%)",
  regionSoutheast = "Region: Southeast (\\%)"
)

tex_order <- c(
  "prec_verao", "prec_outono", "prec_inverno", "prec_primav",
  "temp_verao", "temp_inverno", "temp_primav", "temp_outono",
  "altitude", "avg_tri",
  "regionMidwest", "regionNorth", "regionNortheast", "regionSouth", "regionSoutheast"
)

tab_tex <- tab[match(tex_order, tab$Variable), ]

fmt_num <- function(x) sprintf("%.3f", as.numeric(x))
tex_row <- function(label, treated, full_control, matched_control, diff_full, diff_matched) {
  paste0(
    label, " & ", treated, " & & ", full_control, " & & ", matched_control,
    " & & ", diff_full, " & & ", diff_matched, " \\\\"
  )
}

table_rows <- vapply(seq_len(nrow(tab_tex)), function(i) {
  tex_row(
    variable_labels[[tab_tex$Variable[i]]],
    fmt_num(tab_tex$Treated_Mean[i]),
    fmt_num(tab_tex$Control_Mean_Overall[i]),
    fmt_num(tab_tex$Control_Mean_Matched[i]),
    tab_tex$Diff_T_minus_Control_Overall[i],
    tab_tex$Diff_T_minus_Control_Matched[i]
  )
}, character(1))

n_treated <- sum(dados_psm4$landslide == 1, na.rm = TRUE)
n_control <- sum(dados_psm4$landslide == 0, na.rm = TRUE)
n_matched_control <- sum(m.data$landslide == 0, na.rm = TRUE)

tex <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\scalebox{0.75}{",
  "\\begin{threeparttable}",
  "\\caption{Balance Check for Treated and Control Units Before and After the Matching}",
  "\\label{A:_table_balance_test}",
  "\\begin{tabular}{lccccccccc}",
  "\\toprule",
  "  & (1) &       &  (2)  &       &  (3) &       &  (4) &        &  (5) \\\\",
  "\\cmidrule{2-2}\\cmidrule{4-4}\\cmidrule{6-6}\\cmidrule{8-8}\\cmidrule{10-10} ",
  " & Treated & & Full Control & & Matched Control & & Diff (1-2) & & Diff (1-3) \\\\",
  "\\midrule",
  table_rows,
  "\\midrule",
  tex_row("Number of Municipalities", n_treated, n_control, n_matched_control, "", ""),
  "\\bottomrule",
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{minipage}{\\linewidth}",
  "\\small ",
  "Note: This table shows the mean of the covariates for the group of municipalities affected by the landslides (column (1)), for the group of unaffected municipalities from all over Brazil (column (2)), and for the group of unaffected municipalities selected by the nearest neighbor algorithm (column (3)). Column (4) presents the difference in means between the treated and unmatched controls, and column (5) presents the difference in means between the treated and matched controls municipalities. ",
  "\\end{minipage}",
  "\\end{threeparttable}",
  "}",
  "\\end{table}"
)

writeLines(tex, file.path(path_output_git, "_table_balance_test.tex"))



