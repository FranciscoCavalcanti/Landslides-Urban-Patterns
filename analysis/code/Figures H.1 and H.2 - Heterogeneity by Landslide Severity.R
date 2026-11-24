# Figures H.1 and H.2: distribution of people affected and heterogeneity above/below the median, matched sample.
# Output: analysis/output/severity_distribution_total_affected.jpg, eventstudy_severity_median_*.jpg, heterogeneity_severity_att.csv

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
dados <- merge(readRDS(paste0(path_output, "database_panel.rds")), psm[, c("code", "weights")], by = "code")   # matched sample
dados <- dados[!is.na(dados$lurban_size_high_risk_near_30), ]                                              # years 2003-2020 of the matched municipalities

#### Severity of the first landslide of each treated municipality ####
sev <- dados %>% filter(first_year_landslide > 0) %>% group_by(code) %>%
  summarise(fy = max(first_year_landslide), total = first(landslide_people_affected_first), .groups = "drop")
stopifnot(!anyNA(sev$total))
cat("treated municipalities:", nrow(sev), "| median of people affected:", median(sev$total), "\n")
sev$g_med <- factor(ifelse(sev$total <= median(sev$total), "Low", "High"), levels = c("Low", "High"))

#### Histogram of the people affected (first landslide of the 547 treated municipalities) ####
lv <- c("0", "1-5", "6-10", "11-30", "31-100", "101-300", "301-1,000", "> 1,000")
d <- as.data.frame(table(cut(sev$total, c(-1, 0, 5, 10, 30, 100, 300, 1000, Inf), labels = lv))); names(d) <- c("bin", "n")
gh <- ggplot(d, aes(bin, n)) + geom_col(fill = "grey45", width = .8) + geom_text(aes(label = n), vjust = -.4, size = 4.2) +
  scale_y_continuous(expand = expansion(mult = c(0, .1))) + labs(x = "People affected by the first landslide", y = "Number of municipalities") +
  theme_minimal(base_size = 14) + theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
ggsave(paste0(path_output_git, "severity_distribution_total_affected.jpg"), gh, width = 10, height = 5.2, dpi = 150, bg = "white")

#### Estimation: overall ATT and event study by severity group ####
estimate <- function(codes_treated) {
  set.seed(123)   # same bootstrap draws in every estimation
  dd <- dados %>% filter(first_year_landslide == 0 | code %in% codes_treated)
  mw <- aggte(att_gt(yname = "lurban_size", gname = "first_year_landslide", idname = "code", bstrap = TRUE, base_period = "universal", tname = "year", clustervars = "code", data = dd), type = "dynamic")
  list(att = mw$overall.att, se = mw$overall.se, n_treated = sum(sev$fy[sev$code %in% codes_treated] > min(dados$year)),
       est = broom::tidy(mw) %>% select(event.time, estimate, std.error, conf.low, conf.high)) }
specs <- setNames(lapply(levels(sev$g_med), function(l) sev$code[sev$g_med == l]), paste0("g_med:", levels(sev$g_med)))
res <- lapply(specs, estimate)
sm <- bind_rows(lapply(names(res), function(k) data.frame(measure = "People affected (median split)", group = sub(".*:", "", k), n_treated = res[[k]]$n_treated, att = res[[k]]$att, se = res[[k]]$se))) %>%
  mutate(conf.low = att - 1.96 * se, conf.high = att + 1.96 * se)
write.csv(sm %>% mutate(across(where(is.numeric), ~ round(.x, 4))), paste0(path_output_git, "heterogeneity_severity_att.csv"), row.names = FALSE)
print(sm %>% mutate(across(where(is.numeric), ~ round(.x, 3))), row.names = FALSE)

#### Event studies (same format as Figure 7), people affected ####
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
for (l in levels(sev$g_med)) ggsave(paste0(path_output_git, "eventstudy_severity_median_", tolower(l), ".jpg"), plot_es(res[[paste0("g_med:", l)]]), width = 20, height = 10, units = "in", dpi = 100)
