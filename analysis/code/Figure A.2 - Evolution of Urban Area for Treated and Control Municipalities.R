# Figure A.2: average urban area of treated and control municipalities (matched sample), normalized to 100 in 2003.
# Output: analysis/output/_graph_urban_area_treated_control_index.jpg

library(dplyr); library(MatchIt); library(ggplot2)
set.seed(123)
path_output <- paste0(DROPBOX_PATH, "/build/output/"); path_output_git <- paste0(GITHUB_PATH, "/analysis/output/")
dir.create(path_output_git, showWarnings = FALSE)

d2 <- readRDS(paste0(path_output, "database_two_periods.rds")) %>% filter(year == 2000)
d2$region <- factor(d2$region, levels = c("Midwest", "North", "Northeast", "South", "Southeast"))
v <- c("prec_verao", "prec_outono", "prec_inverno", "prec_primav", "altitude", "temp_verao", "temp_inverno", "temp_primav", "temp_outono", "avg_tri", "avg_slope", "region")
psm <- match.data(matchit(landslide ~ prec_verao + prec_outono + prec_inverno + prec_primav + altitude + temp_verao + temp_inverno + temp_primav + temp_outono + avg_tri + avg_slope + region,
                          data = d2[complete.cases(d2[, v]), ], method = "nearest"))
panel <- readRDS(paste0(path_output, "database_panel.rds")) %>% filter(code %in% psm$code, !is.na(urban_size)) %>%
  mutate(group = ifelse(first_year_landslide > 0, "Treated", "Control"))
cat("treated:", n_distinct(panel$code[panel$group == "Treated"]), "| control:", n_distinct(panel$code[panel$group == "Control"]), "\n")

ts <- panel %>% group_by(group, year) %>% summarise(mean = mean(urban_size), .groups = "drop") %>% group_by(group) %>% mutate(index = 100 * mean / mean[year == min(year)]) %>% ungroup() %>%
  mutate(group = factor(group, levels = c("Treated", "Control")))
g <- ggplot(ts, aes(year, index, linetype = group, shape = group)) +
  geom_line(linewidth = 0.8, colour = "black") + geom_point(size = 2.6, colour = "black") +
  scale_linetype_manual(values = c(Treated = "solid", Control = "dashed")) + scale_shape_manual(values = c(Treated = 16, Control = 17)) +
  scale_x_continuous(breaks = seq(2003, 2020, 2)) +
  labs(x = "Year", y = "Urban area (2003 = 100)", linetype = NULL, shape = NULL) +
  theme_minimal(base_size = 20) + theme(legend.position = "top", panel.grid.minor = element_blank())
ggsave(paste0(path_output_git, "_graph_urban_area_treated_control_index.jpg"), g, width = 12, height = 7, dpi = 100, bg = "white")
print(ts %>% filter(year %in% c(2003, 2010, 2020)) %>% mutate(across(c(mean, index), ~ round(.x, 1))), n = 20)
