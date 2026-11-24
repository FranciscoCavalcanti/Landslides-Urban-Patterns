# Figure 8: overall ATT on the urban area of high-risk and low-risk regions under alternative definitions of risky locations (Callaway and Sant'Anna, matched sample).
# Output: analysis/output/coefplot_all_criteria.jpg, analysis/output/overall_att_by_definition.csv

library(dplyr)
library(MatchIt)
library(did)
library(ggplot2)

set.seed(123)
path_output <- paste0(DROPBOX_PATH, "/build/output/")
path_output_git <- paste0(GITHUB_PATH, "/analysis/output/")

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
dados2 <- merge(dados, psm[, c("code", "weights")], by = "code")       # matched sample: 547 treated + 547 control municipalities
dados2 <- dados2[!is.na(dados2$lurban_size_high_risk_near_30), ]       # years 2003-2020 with the risk outcomes

#### Outcomes: (label, high-risk variable, low-risk variable, criterion, cutoff, sample) ####
defs <- data.frame(
  criterion = c("near", "prox", "pix", "cellmax", "qnt", "sgb"),
  cutoff    = c(20, 30, 30, 30, 30, NA),
  high = c("lurban_size_high_risk_near_20", "lurban_size_high_risk_prox_30", "lurban_size_high_risk_pix_30", "lurban_size_high_risk_cellmax_30",
           "lurban_size_high_risk_qnt_30", "lurban_size_high_risk_sgb_100m"),
  low  = c("lurban_size_low_risk_near_20", "lurban_size_low_risk_prox_30", "lurban_size_low_risk_pix_30", "lurban_size_low_risk_cellmax_30",
           "lurban_size_low_risk_qnt_30", "lurban_size_low_risk_sgb_100m"),
  stringsAsFactors = FALSE)
panel_lab <- c(near = "Proximity to steep terrain, 100 m (slope above 20%)", prox = "Proximity to steep terrain, 250 m (slope above 30%)",
               pix = "Slope of the urban pixel (slope above 30%)", cellmax = "Maximum slope of the cell (slope above 30%)",
               qnt = "90th percentile of slope of the cell (slope above 30%)", sgb = "SGB risk sectors (within 100 m of a sector)")

#### Estimation (Callaway and Sant'Anna, dynamic aggregation; overall ATT) ####
estimate <- function(y, data) {
  set.seed(123)   # same bootstrap draws in every estimation
  mw <- aggte(att_gt(yname = y, gname = "first_year_landslide", idname = "code", bstrap = TRUE, base_period = "universal",
                     tname = "year", clustervars = "code", data = data), type = "dynamic")
  c(att = mw$overall.att, se = mw$overall.se)
}
rows <- list()
for (i in seq_len(nrow(defs))) {
  d <- if (defs$criterion[i] == "sgb") dados2[!is.na(dados2[[defs$high[i]]]), ] else dados2   # SGB: only municipalities with at least one sector
  for (g in c("high", "low")) {
    e <- estimate(defs[[g]][i], d)
    rows[[length(rows) + 1]] <- data.frame(criterion = defs$criterion[i], cutoff = defs$cutoff[i], region_type = g, att = e[["att"]], se = e[["se"]])
  }
}
res <- bind_rows(rows) %>% mutate(conf.low = att - 1.96 * se, conf.high = att + 1.96 * se)
write.csv(res %>% mutate(across(where(is.numeric), ~ round(.x, 4))), paste0(path_output_git, "overall_att_by_definition.csv"), row.names = FALSE)
print(res %>% mutate(across(where(is.numeric), ~ round(.x, 4))), row.names = FALSE)

#### Figure (paper style: black markers, caps, light grid; circle = high-risk, triangle = low-risk) ####
lev <- unname(panel_lab[defs$criterion])
pl <- res %>% mutate(panel = factor(panel_lab[criterion], levels = lev),
                     region = factor(region_type, levels = c("high", "low"), labels = c("High-risk regions", "Low-risk regions")))
stars <- function(a, s) { z <- abs(a / s); ifelse(z > 2.576, "***", ifelse(z > 1.96, "**", ifelse(z > 1.645, "*", ""))) }
pl$lab <- paste0(ifelse(pl$att < 0, "\u2212", ""), sprintf("%.3f", abs(pl$att)), stars(pl$att, pl$se))   # ATT with significance stars
levels(pl$panel) <- paste0("(", letters[seq_along(levels(pl$panel))], ") ", levels(pl$panel))             # (a), (b), ... to identify the panels
g <- ggplot(pl, aes(x = region, y = att, shape = region)) +
  geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), linewidth = 0.5, width = 0.35, position = position_dodge(width = 0.6)) +
  geom_point(size = 3, position = position_dodge(width = 0.6)) +
  geom_text(aes(y = conf.high, label = lab), position = position_dodge(width = 0.6), vjust = -0.7, size = 4.6) +
  facet_grid(. ~ panel, scales = "free_x", space = "free_x", labeller = labeller(panel = label_wrap_gen(18, multi_line = TRUE))) +
  scale_shape_manual(values = c("High-risk regions" = 16, "Low-risk regions" = 17)) +
  scale_y_continuous(labels = function(x) sprintf("%.2f", x), expand = expansion(mult = c(0.05, 0.14))) +
  labs(x = NULL, y = "Overall ATT", shape = NULL) +
  theme_minimal() +
  theme(text = element_text(size = 19), legend.position = "top", panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        strip.text = element_text(size = 16, face = "bold"), axis.text.x = element_blank(), panel.spacing.x = unit(0.9, "lines"))
ggsave(paste0(path_output_git, "coefplot_all_criteria.jpg"), g, width = 20, height = 8, units = "in", dpi = 100, bg = "white")
