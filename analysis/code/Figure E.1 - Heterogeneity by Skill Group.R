# Figure E.1: heterogeneity by skill group, overall ATT on the log average wage of high- and low-skill workers and on the share of low-skill workers in employment (matched sample).
# The gap test (high-skill minus low-skill wage effect) is printed to the console.
# Output: analysis/output/_graph_heterogeneity_skill_{wage,composition}.jpg

library(dplyr)
library(MatchIt)
library(did)
library(ggplot2)

set.seed(123)
path_output <- paste0(DROPBOX_PATH, "/build/output/")
path_output_git <- paste0(GITHUB_PATH, "/analysis/output/")
dir.create(path_output_git, showWarnings = FALSE, recursive = TRUE)

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
dados <- readRDS(paste0(path_output, "database_panel.rds"))
dados2 <- merge(dados, psm[, c("code", "weights")], by = "code")

#### Outcomes ####
lw <- function(wage_mass, emp) ifelse(emp > 0 & wage_mass > 0, log(wage_mass / emp), NA)      # log average wage of the group (undefined without employment or without wages)
dados2$lwage_high <- lw(dados2$wage_mass_high_skill, dados2$employment_high_skill)
dados2$lwage_low  <- lw(dados2$wage_mass_low_skill,  dados2$employment_low_skill)
emp_tot <- dados2$employment_high_skill + dados2$employment_low_skill
dados2$share_emp_low <- ifelse(emp_tot > 0, 100 * dados2$employment_low_skill / emp_tot, NA)  # percentage points

#### Estimation (Callaway and Sant'Anna, dynamic aggregation; overall ATT) ####
estimate <- function(y, data = dados2) {
  set.seed(123)   # same bootstrap draws in every estimation
  d <- data[!is.na(data[[y]]), ]
  keep <- names(which(table(d$code) == max(table(d$code))))                  # municipalities observed in all years (balanced panel)
  d <- d[d$code %in% keep, ]
  mw <- aggte(att_gt(yname = y, gname = "first_year_landslide", idname = "code", bstrap = TRUE, base_period = "universal",
                     tname = "year", clustervars = "code", data = d), type = "dynamic")
  c(att = mw$overall.att, se = mw$overall.se, n_mun = length(keep), n_treated = n_distinct(d$code[d$first_year_landslide > 0]))
}
defs <- data.frame(y = c("lwage_high", "lwage_low", "share_emp_low"), figure = c("wage", "wage", "composition"),
                   label = c("High-skill workers", "Low-skill workers", "Low-skill workers"), stringsAsFactors = FALSE)
res <- bind_cols(defs, as.data.frame(t(sapply(defs$y, estimate)))) %>% mutate(conf.low = att - 1.96 * se, conf.high = att + 1.96 * se)
print(res %>% mutate(across(where(is.numeric), ~ round(.x, 4))), row.names = FALSE)

#### Figures (paper style: black markers, caps, light grid; circle = high-skill, triangle = low-skill) ####
stars <- function(a, s) { z <- abs(a / s); ifelse(z > 2.576, "***", ifelse(z > 1.96, "**", ifelse(z > 1.645, "*", ""))) }
res$lab <- paste0(ifelse(res$att < 0, "−", ""), sprintf("%.3f", abs(res$att)), stars(res$att, res$se))   # ATT with significance stars
plot_att <- function(fig, ylab, width, legend = "top") {
  pl <- res %>% filter(figure == fig) %>% mutate(skill = factor(label, levels = c("High-skill workers", "Low-skill workers")))
  g <- ggplot(pl, aes(x = skill, y = att, shape = skill)) +
    geom_hline(yintercept = 0) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), linewidth = 0.5, width = 0.35) +
    geom_point(size = 3) +
    geom_text(aes(y = conf.high, label = lab), vjust = -0.7, size = 4.6) +
    scale_shape_manual(values = c("High-skill workers" = 16, "Low-skill workers" = 17)) +
    scale_y_continuous(labels = function(x) sprintf("%.2f", x), expand = expansion(mult = c(0.05, 0.14))) +
    labs(x = NULL, y = ylab, shape = NULL) +
    theme_minimal() +
    theme(text = element_text(size = 19), legend.position = legend, panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank())
  list(plot = g, width = width)
}
f1 <- plot_att("wage", "Overall ATT (log average wage)", 8)
f2 <- plot_att("composition", "Overall ATT (share of employment)", 8)      # same size as the wage figure
ggsave(paste0(path_output_git, "_graph_heterogeneity_skill_wage.jpg"), f1$plot, width = f1$width, height = 8, units = "in", dpi = 100, bg = "white")
ggsave(paste0(path_output_git, "_graph_heterogeneity_skill_composition.jpg"), f2$plot, width = f2$width, height = 8, units = "in", dpi = 100, bg = "white")

#### Test of the difference between high-skill and low-skill wages ####
# Gap test: ATT on log(high-skill wage) - log(low-skill wage), common sample; H0: equal effects on both wages.
dados2$lwage_gap <- dados2$lwage_high - dados2$lwage_low
ok <- !is.na(dados2$lwage_gap)
common <- names(which(table(dados2$code[ok]) == max(table(dados2$code[ok]))))
dc <- dados2[dados2$code %in% common, ]
gap <- data.frame(outcome = c("High-skill log wage", "Low-skill log wage", "Gap (high - low)"), t(sapply(c("lwage_high", "lwage_low", "lwage_gap"), function(y) estimate(y, dc)[c("att", "se")])))
gap$att <- as.numeric(gap$att); gap$se <- as.numeric(gap$se)
gap$z <- gap$att / gap$se; gap$p_value <- 2 * pnorm(-abs(gap$z)); gap$n_mun <- length(common); gap$n_treated <- n_distinct(dc$code[dc$first_year_landslide > 0])
print(gap %>% mutate(across(where(is.numeric), ~ round(.x, 4))), row.names = FALSE)
