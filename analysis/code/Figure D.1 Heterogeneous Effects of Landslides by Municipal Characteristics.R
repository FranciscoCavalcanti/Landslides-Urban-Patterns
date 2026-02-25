# Heterogeneity plot (overall ATT by Above/Below median groups)

# --- Libraries ----------------------------------------------------------------
library(dplyr)
library(did)
library(ggplot2)
library(tibble)
library(broom)

# --- Reproducibility ----------------------------------------------------------
set.seed(123)

# --- Paths (assumes DROPBOX_PATH and GITHUB_PATH exist in your environment) ---
path_input     <- paste0(DROPBOX_PATH, "/build/input/")
path_output    <- paste0(DROPBOX_PATH, "/build/output/")
path_output_git <- paste0(GITHUB_PATH, "/analysis/output/")

# --- Load data ----------------------------------------------------------------
dados <- readRDS(paste0(path_output, "database_panel.rds"))
psm   <- readRDS(paste0(path_output, "restricted_PSM_database.rds"))

# --- Merge main dataset with PSM covariates -----------------------------------
dados2 <- merge(
  dados,
  psm[, c(
    "code",
    "weights",
    "p_informal", "p_urbana",
    "avg_income", "population",
    "prop_ativo_forca_trabalho",
    "prop_manufatura", "prop_desemprego"
  )],
  by = "code"
)

# --- Settings -----------------------------------------------------------------
vars_het <- c(
  "p_informal",
  "avg_income",
  "population.y",
  "prop_ativo_forca_trabalho",
  "prop_manufatura",
  "prop_desemprego"
)

outcome <- "lurban_size"
alpha   <- 0.05  # 95% CI
z       <- qnorm(1 - alpha / 2)

# --- Helper: overall ATT estimator --------------------------------------------
estimate_overall_att <- function(data, y) {
  
  dyn <- aggte(
    att_gt(
      yname = y,
      gname = "first_year_landslide",
      idname = "code",
      tname = "year",
      data = data,
      control_group = "nevertreated",
      base_period = "universal",
      bstrap = TRUE,
      clustervars = "code"
    ),
    type = "dynamic"
  )
  
  tibble(att = dyn$overall.att, se = dyn$overall.se)
}

# --- Build Above/Below median groups at municipality level --------------------
# Note: we only join the group indicators back to avoid .x/.y issues everywhere
cov_muni <- dados2 %>%
  group_by(code) %>%
  summarise(
    across(all_of(vars_het), ~ .x[which.max(!is.na(.x))]),
    .groups = "drop"
  )

cov_groups <- cov_muni %>%
  mutate(
    across(
      all_of(vars_het),
      ~ ifelse(.x >= median(.x, na.rm = TRUE), "Above Median", "Below Median"),
      .names = "{.col}_grp"
    )
  ) %>%
  select(code, ends_with("_grp"))

dados2_het <- dados2 %>%
  left_join(cov_groups, by = "code")

# --- Estimate overall ATT by group for each heterogeneity variable ------------
res_het <- lapply(vars_het, function(v) {
  
  gcol <- paste0(v, "_grp")
  
  dados_low  <- dados2_het %>% filter(.data[[gcol]] == "Below Median")
  dados_high <- dados2_het %>% filter(.data[[gcol]] == "Above Median")
  
  est_low  <- estimate_overall_att(dados_low,  outcome)
  est_high <- estimate_overall_att(dados_high, outcome)
  
  bind_rows(
    est_low  %>% mutate(var = v, group = "Below Median"),
    est_high %>% mutate(var = v, group = "Above Median")
  )
}) %>%
  bind_rows() %>%
  mutate(
    conf.low  = att - z * se,
    conf.high = att + z * se
  )

# --- Pretty labels -------------------------------------------------------------
pretty_labels <- c(
  "p_informal"                = "Informality\nShare",
  "avg_income"                = "Average\nIncome",
  "population.y"              = "Population",
  "prop_ativo_forca_trabalho" = "Labor Force\nParticipation",
  "prop_manufatura"           = "Manufacturing\nShare",
  "prop_desemprego"           = "Unemployment\nRate"
)

res_het <- res_het %>%
  mutate(
    var_label = ifelse(var %in% names(pretty_labels), pretty_labels[var], var),
    var_label = factor(var_label, levels = unname(pretty_labels[vars_het])),
    group     = factor(group, levels = c("Below Median", "Above Median"))
  )

# --- Build coefficient labels under the CI ------------------------------------
range_y <- diff(range(c(res_het$conf.low, res_het$conf.high), na.rm = TRUE))
gap_y   <- 0.08 * range_y  # tweak if you want: 0.06 (closer) to 0.12 (farther)

res_het_plot <- res_het %>%
  mutate(
    coef_lbl = sprintf("%.3f", att),
    y_lbl    = conf.low - gap_y
  )

# --- Plot -----------------------------------------------------------
dodge_w <- 0.55

coef_plot <- ggplot(res_het_plot, aes(x = var_label, y = att, color = group, shape = group)) +
  # zero line
  geom_hline(yintercept = 0, linewidth = 0.7, color = "grey40") +
  
  # CI + points
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high),
    width = 0.14,
    linewidth = 1.35,
    position = position_dodge(width = dodge_w)
  ) +
  geom_point(
    size = 5.0,
    stroke = 0.25,
    position = position_dodge(width = dodge_w)
  ) +
  
  # coefficient label under the CI (IMPORTANT: remove from legend to avoid the "a")
  geom_text(
    aes(y = y_lbl, label = coef_lbl),
    position = position_dodge(width = dodge_w),
    size = 7.2,
    show.legend = FALSE
  ) +
  
  # black/grey palette + shapes for grayscale readability
  scale_color_manual(values = c("Below Median" = "grey55", "Above Median" = "black")) +
  scale_shape_manual(values = c("Below Median" = 16, "Above Median" = 17)) +
  
  labs(x = "", y = "Overall ATT", color = "", shape = "") +
  
  # extra space at the bottom for the coefficient labels
  scale_y_continuous(expand = expansion(mult = c(0.20, 0.05))) +
  coord_cartesian(clip = "off") +
  
  theme_minimal(base_size = 26) +
  theme(
    axis.text.x  = element_text(size = 24),
    axis.text.y  = element_text(size = 24),
    axis.title.y = element_text(size = 28),
    legend.text  = element_text(size = 24),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    plot.margin = margin(10, 10, 25, 10)
  ) +
  guides(
    color = guide_legend(override.aes = list(size = 5.0, linewidth = 1.35)),
    shape = guide_legend(override.aes = list(size = 5.0))
  )

print(coef_plot)

# --- Save ----------------------------------------------------------------------
ggsave(
  filename = paste0(path_output_git, "_heterogeneities_plot.jpg"),
  plot     = coef_plot,
  width    = 20,
  height   = 10,
  units    = "in",
  dpi      = 100
)
