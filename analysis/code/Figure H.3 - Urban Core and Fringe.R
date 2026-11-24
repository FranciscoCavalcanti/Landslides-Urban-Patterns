# Figure H.3: effect of landslides on the urban area of the core and the fringe (core: within the distance to the municipal seat that contains one third of the 2003 urban area), matched sample.
# Output: analysis/output/eventstudy_urban_{core,fringe}.jpg, heterogeneity_core_vs_fringe*.csv

library(dplyr); library(MatchIt); library(did); library(ggplot2); library(broom); library(gridExtra); library(data.table)
set.seed(123)
path_output <- paste0(DROPBOX_PATH, "/build/output/"); path_output_git <- paste0(GITHUB_PATH, "/analysis/output/")
dir.create(path_output_git, showWarnings = FALSE, recursive = TRUE)

#### Matching ####
d2 <- readRDS(paste0(path_output, "database_two_periods.rds")) %>% filter(year == 2000)
d2$region <- factor(d2$region, levels = c("Midwest","North","Northeast","South","Southeast"))
v <- c("prec_verao","prec_outono","prec_inverno","prec_primav","altitude","temp_verao","temp_inverno","temp_primav","temp_outono","avg_tri","avg_slope","region")
psm <- match.data(matchit(landslide ~ prec_verao + prec_outono + prec_inverno + prec_primav + altitude + temp_verao + temp_inverno + temp_primav + temp_outono +
                            avg_tri + avg_slope + region, data = d2[complete.cases(d2[, v]), ], method = "nearest"))
dados <- readRDS(paste0(path_output, "database_panel.rds"))
dados2 <- merge(dados, psm[, c("code", "weights")], by = "code")      # matched sample: 547 treated + 547 control municipalities
dados2 <- dados2[!is.na(dados2$lurban_size_high_risk_near_30), ]      # years 2003-2020 with the risk outcomes

dados2$y_core <- dados2$lurban_size_core; dados2$y_fringe <- dados2$lurban_size_fringe
x20 <- dados2[dados2$year == 2020, ]
share <- c(core = sum(expm1(x20$y_core)), fringe = sum(expm1(x20$y_fringe))) / sum(x20$urban_area_c10_ha) * 100
print(round(share, 1))
estimate <- function(y) { set.seed(123); mw <- aggte(att_gt(yname = y, gname = "first_year_landslide", idname = "code", bstrap = TRUE, base_period = "universal", tname = "year", clustervars = "code", data = dados2), type = "dynamic")
  list(att = mw$overall.att, se = mw$overall.se, est = broom::tidy(mw) %>% select(event.time, estimate, std.error, conf.low, conf.high)) }
jobs <- c("y_core", "y_fringe"); zones <- c(core = 1, fringe = 1)
set.seed(123)
res <- lapply(jobs, function(y) tryCatch(estimate(y), error = function(e) NULL)); names(res) <- jobs
cat("estimations:", length(res), "| failed:", sum(sapply(res, is.null)), "\n")
sm <- bind_rows(lapply(jobs, function(y) data.frame(zone = sub("^y_", "", y), att = res[[y]]$att, se = res[[y]]$se))) %>%
  mutate(conf.low = att - 1.96 * se, conf.high = att + 1.96 * se, share_urban_area = share[zone])
write.csv(sm %>% mutate(across(where(is.numeric), ~ round(.x, 4))), paste0(path_output_git, "heterogeneity_core_vs_fringe.csv"), row.names = FALSE)
print(sm %>% mutate(across(where(is.numeric), ~ round(.x, 3))), row.names = FALSE)
write.csv(bind_rows(lapply(jobs, function(y) cbind(zone = sub("^y_", "", y), res[[y]]$est))) %>% mutate(across(where(is.numeric), ~ round(.x, 4))), paste0(path_output_git, "heterogeneity_core_vs_fringe_eventstudy.csv"), row.names = FALSE)

#### Event studies (same format as Figure 7) ####
plot_es <- function(r) {
  est <- r$est; ic <- lapply(c(2.576, 1.96, 1.645), function(zz) r$att + c(-zz, zz) * r$se)
  stars <- ifelse(all(ic[[1]] < 0) | all(ic[[1]] > 0), "***", ifelse(all(ic[[2]] < 0) | all(ic[[2]] > 0), "**", ifelse(all(ic[[3]] < 0) | all(ic[[3]] > 0), "*", "")))
  dt <- data.table::data.table(ATT = c(paste0(round(r$att, 4), stars), paste0("(", round(r$se, 4), ")")))
  value <- c(abs(0 - summary(est$conf.low)[1]), abs(0 - summary(est$conf.high)[6])); seqs <- seq(min(est$conf.low, na.rm = T), max(est$conf.high, na.rm = T), length.out = 1000)
  pos <- ifelse(value[1] < value[2], quantile(seqs, probs = 0.91), quantile(seqs, probs = 0.18))
  tg <- tableGrob(dt, rows = NULL, theme = ttheme_minimal(core = list(fg_params = list(fontsize = 30)), colhead = list(fg_params = list(fontsize = 30, fontface = "bold"))))
  ggplot(est, aes(y = event.time, x = estimate)) + geom_errorbar(aes(xmax = conf.high, xmin = conf.low), linewidth = 0.5, width = 0.5) + geom_point(size = 1.5) +
    geom_vline(xintercept = 0) + geom_hline(yintercept = -1) + labs(x = "Coefficient", y = "Period") + scale_y_continuous(breaks = seq(-16, 16, by = 2)) + coord_flip() +
    theme_minimal() + theme(text = element_text(size = 25), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) + annotation_custom(grob = tg, xmin = pos, xmax = pos, ymin = -15, ymax = -11)
}
for (z in c("core", "fringe")) ggsave(paste0(path_output_git, "eventstudy_urban_", z, ".jpg"), plot_es(res[[paste0("y_", z)]]), width = 20, height = 10, units = "in", dpi = 100)
