# Figure D.9b: robustness, standard errors clustered by matched pair (Abadie and Spiess, 2022); same estimation as Figure 2.
# Output: analysis/output/_graph_robustness_urban_size_cluster_matched_pair.jpg, _robustness_urban_size_cluster_matched_pair.csv

library(dplyr); library(MatchIt); library(did); library(ggplot2); library(gridExtra); library(broom)
set.seed(123)
path_output <- paste0(DROPBOX_PATH, "/build/output/"); path_output_git <- paste0(GITHUB_PATH, "/analysis/output/")
dir.create(path_output_git, showWarnings = FALSE)

dados <- readRDS(paste0(path_output, "database_panel.rds"))
d2 <- readRDS(paste0(path_output, "database_two_periods.rds")) %>% filter(year == 2000)
d2$region <- factor(d2$region, levels = c("Midwest", "North", "Northeast", "South", "Southeast"))
v <- c("prec_verao", "prec_outono", "prec_inverno", "prec_primav", "altitude", "temp_verao", "temp_inverno", "temp_primav", "temp_outono", "avg_tri", "avg_slope", "region")
psm <- match.data(matchit(landslide ~ prec_verao + prec_outono + prec_inverno + prec_primav + altitude + temp_verao + temp_inverno + temp_primav + temp_outono + avg_tri + avg_slope + region,
                          data = d2[complete.cases(d2[, v]), ], method = "nearest"))
dados2 <- merge(dados, psm[, c("code", "weights", "subclass")], by = "code")
dados2$pair <- as.integer(dados2$subclass)
cat("units:", n_distinct(dados2$code), "| pairs:", n_distinct(dados2$pair), "| units per pair:", unique(table(unique(dados2[, c("code", "pair")])$pair)), "\n")

est <- function(cl) {
  set.seed(123)
  aggte(att_gt(yname = "lurban_size", gname = "first_year_landslide", idname = "code", tname = "year", data = dados2, base_period = "universal",
               bstrap = TRUE, clustervars = cl), type = "dynamic")
}
m_code <- est("code")
m_pair <- est("pair")
cmp <- data.frame(event.time = m_pair$egt, estimate = m_pair$att.egt, se_municipality = m_code$se.egt, se_pair = m_pair$se.egt,
                  conf.low = m_pair$att.egt - m_pair$crit.val.egt * m_pair$se.egt, conf.high = m_pair$att.egt + m_pair$crit.val.egt * m_pair$se.egt)
write.csv(cmp %>% mutate(across(where(is.numeric), ~ round(.x, 5))), paste0(path_output_git, "_robustness_urban_size_cluster_matched_pair.csv"), row.names = FALSE)
cmp[cmp$event.time == -1, c("conf.low", "conf.high")] <- 0   # reference period
cat(sprintf("\nOverall ATT = %.4f | SE cluster municipality = %.4f | SE cluster pair = %.4f | uniform critical value: municipality %.2f, pair %.2f\n",
            m_pair$overall.att, m_code$overall.se, m_pair$overall.se, m_code$crit.val.egt, m_pair$crit.val.egt))
print(cmp %>% mutate(across(where(is.numeric), ~ round(.x, 4))), row.names = FALSE)

est <- broom::tidy(m_pair) %>% select(c(event.time, estimate, std.error, conf.low, conf.high))
IC_99 <- m_pair$overall.att + c(-2.576, 2.576) * m_pair$overall.se
IC_95 <- m_pair$overall.att + c(-1.96, 1.96) * m_pair$overall.se
IC_90 <- m_pair$overall.att + c(-1.645, 1.645) * m_pair$overall.se
ATT_significance <- ifelse(all(IC_99 < 0) | all(IC_99 > 0), paste0(round(m_pair$overall.att, 4), "***"),
                    ifelse(all(IC_95 < 0) | all(IC_95 > 0), paste0(round(m_pair$overall.att, 4), "**"),
                    ifelse(all(IC_90 < 0) | all(IC_90 > 0), paste0(round(m_pair$overall.att, 4), "*"), paste(round(m_pair$overall.att, 4)))))
dados_tabela <- data.table::data.table(`ATT` = c(ATT_significance, paste0("(", round(m_pair$overall.se, 4), ")")))
value <- c(abs(0 - summary(est$conf.low)[1]), abs(0 - summary(est$conf.high)[6]))
sequencia <- seq(min(est$conf.low[is.finite(est$conf.low)]), max(est$conf.high[is.finite(est$conf.high)]), length.out = 1000)
table_pos_y <- ifelse(value[1] < value[2], quantile(sequencia, probs = 0.91), quantile(sequencia, probs = 0.18))
tabela_grob <- tableGrob(dados_tabela, rows = NULL,
                         theme = ttheme_minimal(core = list(fg_params = list(fontsize = 30)), colhead = list(fg_params = list(fontsize = 30, fontface = "bold")),
                                                rowhead = list(fg_params = list(fontsize = 30))))
g <- ggplot(data = est, aes(y = event.time, x = estimate)) +
  geom_pointrange(aes(xmax = conf.high, xmin = conf.low), linewidth = 0.5, position = position_dodge(width = 0.5), linetype = 'blank') +
  geom_errorbar(aes(xmax = conf.high, xmin = conf.low), linewidth = 0.5, width = 0.5, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = -1) +
  labs(x = 'Coefficient', y = 'Period', color = "", linetype = "", title = "") +
  scale_y_continuous(breaks = seq(-16, 16, by = 2)) +
  coord_flip() +
  theme_minimal() +
  theme(text = element_text(size = 25), legend.text = element_text(size = 25), legend.title = element_text(size = 25),
        legend.key.width = unit(1.5, "cm"), legend.key.height = unit(1, "cm"),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
  annotation_custom(grob = tabela_grob, xmin = table_pos_y, xmax = table_pos_y, ymin = -15, ymax = -11)
ggsave(paste0(path_output_git, "_graph_robustness_urban_size_cluster_matched_pair.jpg"), g, width = 20, height = 10, units = "in", dpi = 100)
