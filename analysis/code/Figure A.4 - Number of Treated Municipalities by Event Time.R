# Figure A.4: number of treated municipalities identifying each event-time coefficient of Figure 2.
# Output: analysis/output/sample_support_histogram.jpg, sample_support_by_eventtime.csv

library(dplyr)
library(MatchIt)
library(did)
library(ggplot2)

set.seed(123)

path_output <- paste0(DROPBOX_PATH, "/build/output/")
path_output_git <- paste0(GITHUB_PATH, "/analysis/output/")

#### Data ####
dados <- readRDS(paste0(path_output, "database_panel.rds"))

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

dados2 <- merge(dados, psm[, c("code", "weights")], by = "code")

cs <- att_gt(yname = "lurban_size", gname = "first_year_landslide", idname = "code",
             base_period = "universal", tname = "year", data = dados2)

cohort_n <- psm %>% filter(landslide == 1) %>% distinct(code, first_year_landslide) %>%
  count(first_year_landslide, name = "n_treated")

contrib <- data.frame(group = cs$group, t = cs$t) %>%
  mutate(event_time = t - group) %>%
  left_join(cohort_n, by = c("group" = "first_year_landslide")) %>%
  group_by(event_time) %>% summarise(n_treated = sum(n_treated), .groups = "drop") %>%
  arrange(event_time)

write.csv(contrib, paste0(path_output_git, "sample_support_by_eventtime.csv"), row.names = FALSE)

p <- ggplot(contrib, aes(event_time, n_treated)) +
  geom_col(fill = "grey30") +
  geom_text(aes(label = n_treated), vjust = -0.4, size = 3) +
  geom_vline(xintercept = -0.5) +
  scale_x_continuous(breaks = seq(-17, 16, 1)) +
  labs(x = "Period", y = "N treated municipalities") +
  theme_minimal() +
  theme(text = element_text(size = 18), axis.text = element_text(color = "black"),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

ggsave(paste0(path_output_git, "sample_support_histogram.jpg"), p, width = 12, height = 5, dpi = 300)
