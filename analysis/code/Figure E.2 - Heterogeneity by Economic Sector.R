# Figure E.2: heterogeneity by economic sector, overall ATT on firms and employment per capita in transportation, manufacturing, wholesale and retail, services and construction (matched sample).
# Per-capita denominator: census population log-linearly interpolated to 2003-2020. Optional environment variables: SCALE (default 1), PLUS1 (default 0).
# Output: analysis/output/_graph_heterogeneity_sectors_{firms,employment}.jpg

library(dplyr)
library(MatchIt)
library(did)
library(ggplot2)

set.seed(123)
path_output <- paste0(DROPBOX_PATH, "/build/output/")
path_output_git <- paste0(GITHUB_PATH, "/analysis/output/")
dir.create(path_output_git, showWarnings = FALSE, recursive = TRUE)
SCALE <- as.numeric(Sys.getenv("SCALE", "1"))
PLUS1 <- Sys.getenv("PLUS1", "0") != "0"; suf <- paste0(if (PLUS1) "_plus1" else "", if (SCALE != 1) paste0("_scale", SCALE) else "")
unit <- if (SCALE == 1) "per capita" else paste("per", format(SCALE, big.mark = ","), "inhabitants")

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

#### Data: panel with the sector variables and the interpolated population ####
dados <- readRDS(paste0(path_output, "database_panel.rds"))
cens <- readRDS(paste0(path_output, "database_three_periods.rds")) %>% select(code, year, population)
interp_pop <- function(d) { d <- d[order(d$year), ]; exp(approx(d$year, log(d$population), xout = 2003:2020, rule = 2)$y) }
ip <- do.call(rbind, lapply(split(cens, cens$code), function(d) data.frame(code = d$code[1], year = 2003:2020, pop_interp = interp_pop(d))))
dados <- merge(dados %>% select(-population), ip, by = c("code", "year"), all.x = TRUE)
dados2 <- merge(dados, psm[, c("code", "weights")], by = "code")      # matched sample: 547 treated + 547 control municipalities

sectors <- c(transportation = "Transportation", manufacturing = "Manufacturing", wholesale_and_retail = "Wholesale and retail", services = "Services", construction = "Construction")
for (s in names(sectors)) {
  f <- SCALE * dados2[[paste0("firms_", s)]] / dados2$pop_interp; e <- SCALE * dados2[[paste0("employment_", s)]] / dados2$pop_interp
  dados2[[paste0("lfirms_", s)]] <- if (PLUS1) log(1 + f) else ifelse(f > 0, log(f), NA)
  dados2[[paste0("lemp_", s)]]   <- if (PLUS1) log(1 + e) else ifelse(e > 0, log(e), NA)
}

#### Estimation (Callaway and Sant'Anna, dynamic aggregation; overall ATT) ####
estimate <- function(y) {
  set.seed(123)   # same bootstrap draws in every estimation
  d <- dados2[!is.na(dados2[[y]]), ]
  keep <- names(which(table(d$code) == max(table(d$code))))                  # municipalities observed in all years (balanced panel)
  d <- d[d$code %in% keep, ]
  mw <- aggte(att_gt(yname = y, gname = "first_year_landslide", idname = "code", bstrap = TRUE, base_period = "universal",
                     tname = "year", clustervars = "code", data = d), type = "dynamic")
  c(att = mw$overall.att, se = mw$overall.se, n_mun = length(keep), n_treated = n_distinct(d$code[d$first_year_landslide > 0]))
}
defs <- expand.grid(sector = names(sectors), outcome = c("firms", "employment"), stringsAsFactors = FALSE)
defs$y <- paste0(c(firms = "lfirms_", employment = "lemp_")[defs$outcome], defs$sector)
defs$label <- sectors[defs$sector]
res <- bind_cols(defs, as.data.frame(t(sapply(defs$y, estimate)))) %>% mutate(conf.low = att - 1.96 * se, conf.high = att + 1.96 * se)
print(res %>% mutate(across(where(is.numeric), ~ round(.x, 4))), row.names = FALSE)

#### Figures (paper style: black markers, caps, light grid) ####
stars <- function(a, s) { z <- abs(a / s); ifelse(z > 2.576, "***", ifelse(z > 1.96, "**", ifelse(z > 1.645, "*", ""))) }
plot_att <- function(out, ylab) {
  pl <- res %>% filter(outcome == out) %>% mutate(label = factor(label, levels = unname(sectors)))
  d <- if (max(abs(c(pl$conf.low, pl$conf.high))) < 0.02) 4 else 2                   # more decimals when the ATTs are very small (per capita without the 1,000 scale)
  pl$lab <- paste0(ifelse(pl$att < 0, "\u2212", ""), formatC(abs(pl$att), format = "f", digits = d + 1), stars(pl$att, pl$se))   # ATT with significance stars
  ggplot(pl, aes(x = label, y = att)) +
    geom_hline(yintercept = 0) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), linewidth = 0.5, width = 0.3) +
    geom_point(size = 3.5) +
    geom_text(aes(y = conf.high, label = lab), vjust = -0.7, size = 5.5) +
    scale_y_continuous(labels = function(x) formatC(x, format = "f", digits = d), expand = expansion(mult = c(0.05, 0.14))) +
    labs(x = NULL, y = ylab) +
    theme_minimal() +
    theme(text = element_text(size = 20), panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(size = 17))
}
ggsave(paste0(path_output_git, "_graph_heterogeneity_sectors_firms", suf, ".jpg"), plot_att("firms", paste0("Overall ATT (log firms ", unit, if (PLUS1) " + 1)" else ")")), width = 12, height = 8, units = "in", dpi = 100, bg = "white")
ggsave(paste0(path_output_git, "_graph_heterogeneity_sectors_employment", suf, ".jpg"), plot_att("employment", paste0("Overall ATT (log employment ", unit, if (PLUS1) " + 1)" else ")")), width = 12, height = 8, units = "in", dpi = 100, bg = "white")
