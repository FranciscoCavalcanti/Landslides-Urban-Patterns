# Figure D.10: robustness, aggregation at the metropolitan-region level (arranjo populacional, agglo_id); matching re-done at that level.
# Output: analysis/output/graph_robustness_aggregation_urban_size.jpg

library(dplyr)
library(MatchIt)
library(fixest)
library(did)
library(remotes)
library(did2s)
library(staggered)
library(ggplot2)
library(gridExtra)
library(broom)

set.seed(123)

path_input <- paste0(DROPBOX_PATH, "/build/input/")
path_output <- paste0(DROPBOX_PATH, "/build/output/")
path_output_git <- paste0(GITHUB_PATH, "/analysis/output/")


## 1) Municipal inputs. `agglo_id` (in the two-periods database) is the arranjo populacional of each
##    municipality; municipalities outside any arranjo are their own unit ("ISO_<code>")
dados4     <- readRDS(paste0(path_output,"database_panel.rds"))                         # full municipal panel
dados_psm  <- readRDS(paste0(path_output,"database_two_periods.rds"))           # covariates (use year == 2000)
dados_psm2 <- dplyr::filter(dados_psm, year == 2000)
dados_psm2$region <- factor(dados_psm2$region, levels = c("Midwest","North","Northeast","South","Southeast"))

## 2) Propensity score matching at the arranjo level:
##    municipal covariates aggregated to the arranjo (area-weighted means); an arranjo is treated if any of its
##    municipalities is hit by a landslide; region = region of the arranjo's largest municipality
covs <- c("prec_verao", "prec_outono", "prec_inverno", "prec_primav", "altitude",
          "temp_verao", "temp_inverno", "temp_primav", "temp_outono", "avg_tri", "avg_slope")

agglo_cov <- dados_psm2 %>%
  group_by(agglo_id) %>%
  summarise(across(all_of(covs), ~ weighted.mean(.x, area_geographic, na.rm = TRUE)),
            landslide = as.integer(any(landslide == 1, na.rm = TRUE)),
            region    = region[which.max(area_geographic)],
            .groups = "drop") %>%
  as.data.frame()
agglo_cov <- agglo_cov[complete.cases(agglo_cov[, c(covs, "region")]), ]

PSM_agglo <- matchit(as.formula(paste("landslide ~", paste(c(covs, "region"), collapse = " + "))),
                     data = agglo_cov, method = "nearest")
m.data_agglomeration <- match.data(PSM_agglo)
cat("Matched arranjos:", sum(m.data_agglomeration$landslide == 1), "treated,",
    sum(m.data_agglomeration$landslide == 0), "controls\n")
agglo_keep <- unique(m.data_agglomeration$agglo_id)

weights_area <- dados_psm2 %>% distinct(code, agglo_id, area_geographic)

dados5 <- dados4 %>%
  mutate(landslide_dum = ifelse(landslide == 0 | is.na(landslide),0,1)) %>% 
  dplyr::select(code, year, urban_size, sprawl_index, landslide_dum)

dados_rm5 <- dados5 %>%
  left_join(weights_area, by = "code") %>%
  group_by(agglo_id, year) %>%
  summarise(
    urban_size     = sum(urban_size, na.rm = TRUE),
    sprawl_index = weighted.mean(sprawl_index, area_geographic, na.rm = TRUE),
    landslide_dum  = as.integer(any(landslide_dum == 1, na.rm = TRUE)),
    .groups = "drop"
  )

dados_rm6 <- dados_rm5 %>%
  filter(agglo_id %in% agglo_keep)

firsts <- dados_rm6 %>%
  arrange(agglo_id, year) %>%
  group_by(agglo_id) %>%
  summarise(
    first_year_landslide = {
      ix <- which(landslide_dum == 1)
      if (length(ix) > 0) year[min(ix)] else 0L
    },
    .groups = "drop"
  )

dados_rm7 <- dados_rm6 %>%
  left_join(firsts, by = "agglo_id") %>%
  mutate(
    lurban_size          = log(urban_size),
    lurban_size          = ifelse(is.finite(lurban_size), lurban_size, NA_real_),
    agglo_id_num         = as.integer(factor(agglo_id)),
    year                 = as.integer(year),
    first_year_landslide = as.integer(first_year_landslide)
  )

ggplot_paper <- function(yname, data = dados_rm7) {
  mw <- aggte(
    att_gt(
      yname       = yname,
      gname       = "first_year_landslide",
      idname      = "agglo_id_num",
      tname       = "year",
      data        = data,
      bstrap      = TRUE,
      clustervars = "agglo_id_num",
      base_period = "universal"
    ),
    type = "dynamic"
  )
  
  est <- broom::tidy(mw) %>%
    dplyr::select(event.time, estimate, std.error, conf.low, conf.high) %>%
    mutate(est = "Unweighted")
  
  z_99 <- 2.576; z_95 <- 1.96; z_90 <- 1.645
  IC_99 <- mw$overall.att + c(-z_99 * mw$overall.se, z_99 * mw$overall.se)
  IC_95 <- mw$overall.att + c(-z_95 * mw$overall.se,  z_95 * mw$overall.se)
  IC_90 <- mw$overall.att + c(-z_90 * mw$overall.se,  z_90 * mw$overall.se)
  ATT_txt <- ifelse(
    all(IC_99 < 0) | all(IC_99 > 0), paste0(round(mw$overall.att, 4), "***"),
    ifelse(
      all(IC_95 < 0) | all(IC_95 > 0), paste0(round(mw$overall.att, 4), "**"),
      ifelse(
        all(IC_90 < 0) | all(IC_90 > 0), paste0(round(mw$overall.att, 4), "*"),
        paste(round(mw$overall.att, 4))
      )
    )
  )
  
  value <- c(abs(0 - summary(est$conf.low)[1]), abs(0 - summary(est$conf.high)[6]))
  sequencia <- seq(min(est$conf.low[is.finite(est$conf.low)], na.rm = TRUE),
                   max(est$conf.high[is.finite(est$conf.high)], na.rm = TRUE),
                   length.out = 1000)
  quantil_10  <- quantile(sequencia, probs = 0.18)
  quantil_75  <- quantile(sequencia, probs = 0.91)
  table_pos_y <- ifelse(value[1] < value[2], quantil_75, quantil_10)
  lengend_pos_y <- ifelse(value[1] < value[2], 0.75, 0.10)
  
  tab_dt <- data.table::data.table(`ATT` = c(ATT_txt, paste0("(", round(mw$overall.se, 4), ")")))
  tab_grob <- tableGrob(
    tab_dt, rows = NULL,
    theme = ttheme_minimal(
      core    = list(fg_params = list(fontsize = 30)),
      colhead = list(fg_params = list(fontsize = 30, fontface = "bold")),
      rowhead = list(fg_params = list(fontsize = 30))
    )
  )
  
  ggplot(est, aes(y = event.time, x = estimate)) +
    geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                    linewidth = 0.5, position = position_dodge(width = 0.5),
                    linetype = "blank") +
    geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                  linewidth = 0.5, width = 0.5, position = position_dodge(width = 0.5)) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = -1) +
    labs(x = "Coefficient", y = "Period", title = "") +
    scale_y_continuous(breaks = seq(-16, 16, by = 2)) +
    coord_flip() +
    theme_minimal() +
    theme(
      text = element_text(size = 30),
      legend.text = element_text(size = 25),
      legend.title = element_text(size = 25),
      legend.key.width = unit(1.5, "cm"),
      legend.key.height = unit(1, "cm"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = c(0.125, lengend_pos_y)
    ) +
    annotation_custom(grob = tab_grob, xmin = table_pos_y, xmax = table_pos_y, ymin = -15, ymax = -11)
}

p1 <- ggplot_paper("lurban_size")

ggsave(paste0(path_output_git,"graph_robustness_aggregation_urban_size.jpg"), p1, width = 20, height = 10, units = "in", dpi = 100)
