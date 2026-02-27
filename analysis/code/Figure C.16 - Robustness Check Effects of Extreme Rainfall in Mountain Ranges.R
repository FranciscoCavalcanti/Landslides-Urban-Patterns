# Load required libraries
library(dplyr)       # For data manipulation
library(fixest)      # For fixed effects estimation
library(did)         # For difference-in-differences estimation
library(remotes)     # For installing R packages from remote repositories
library(did2s)       # For alternative DiD estimators
library(staggered)   # For staggered DiD designs
library(ggplot2)     # For creating plots
library(gridExtra)   # For arranging multiple plots
library(data.table)


# Set Seed
set.seed(123)

# Set the paths for input and output files
path_input <- paste0(DROPBOX_PATH, "/build/input/")
path_output <- paste0(DROPBOX_PATH, "/build/output/") 
path_output_git <- paste0(GITHUB_PATH, "/analysis/output/") 

##### Open Data ######
# Load datasets from the specified file paths
dados <- readRDS(paste0(path_output, "database_panel.rds"))
psm <- readRDS(paste0(path_output, "restricted_PSM_database.rds"))

#### Selecting the Treated/Control Group Using PSM ####

# Merge the main dataset with the PSM dataset using the "code" column
dados1 <- merge(dados, psm[, c("code", "weights")], by = "code")


#### Creating a Interaction of Heavy Rain and Moutain Area #####

setDT(dados1); setkey(dados1, code, year)

dados1[, `:=`(
  int_hmount_x_rain1d = Instersection_Moutain * chuva_extrema_1d_100mm
)]

vari <- c("int_hmount_x_rain1d", "landslide_dum")
new <- paste0(vari, "_ff")

dados1$landslide_dum <- ifelse(dados1$landslide_event %in% 1,1,0)
dados1[, (new) := lapply(.SD, cummax), by = code, .SDcols = vari]


# =========================================================
# First Extreme Rainfall Year (for Callaway & Sant’Anna)
# =========================================================


setDT(dados1)

first_int <- dados1[, .(
  first_int_1d = if (any(int_hmount_x_rain1d == 1, na.rm = TRUE))
    as.integer(min(year[int_hmount_x_rain1d == 1], na.rm = TRUE)) else 0L
), by = code]

dados2 <- left_join(dados1, first_int, by = "code")


# --------- Function with the same visual style, but for ITT (dados2 / first_int_1d) ---------
ggplot_paper <- function(x){
  
  # Dynamic estimation (ITT)
  mw.dyn_p <- aggte(
    att_gt(
      yname         = x,
      gname         = "first_int_1d",
      idname        = "code",
      tname         = "year",
      control_group = "nevertreated",
      base_period   = "universal",
      bstrap        = TRUE,
      clustervars   = "code",
      data          = dados2
    ),
    type  = "dynamic",
    na.rm = TRUE
  )
  
  # Tidy table
  est <- broom::tidy(mw.dyn_p) |>
    select(event.time, estimate, std.error, conf.low, conf.high) |>
    mutate(est = "ITT")
  
  # Zs
  z_99 <- 2.576; z_95 <- 1.96; z_90 <- 1.645
  
  # Overall ATT significance (stars)
  IC_99 <- mw.dyn_p$overall.att + c(-z_99 * mw.dyn_p$overall.se,  z_99 * mw.dyn_p$overall.se)
  IC_95 <- mw.dyn_p$overall.att + c(-z_95 * mw.dyn_p$overall.se,  z_95 * mw.dyn_p$overall.se)
  IC_90 <- mw.dyn_p$overall.att + c(-z_90 * mw.dyn_p$overall.se,  z_90 * mw.dyn_p$overall.se)
  
  ATT_significance_p <- ifelse(all(IC_99 < 0) | all(IC_99 > 0), paste0(round(mw.dyn_p$overall.att, 4), "***"),
                               ifelse(all(IC_95 < 0) | all(IC_95 > 0), paste0(round(mw.dyn_p$overall.att, 4), "**"),
                                      ifelse(all(IC_90 < 0) | all(IC_90 > 0), paste0(round(mw.dyn_p$overall.att, 4), "*"),
                                             paste(round(mw.dyn_p$overall.att, 4)))))
  
  # Table as a grob
  dados_tabela <- data.table::data.table(
    `ATT` = c(ATT_significance_p, paste0("(", round(mw.dyn_p$overall.se, 4), ")"))
  )
  tabela_grob <- tableGrob(
    dados_tabela,
    rows = NULL,
    theme = ttheme_minimal(
      core    = list(fg_params = list(fontsize = 30)),
      colhead = list(fg_params = list(fontsize = 30, fontface = "bold")),
      rowhead = list(fg_params = list(fontsize = 30))
    )
  )
  
  # Legend/table position (keeps the baseline logic; robust to different ranges)
  value <- c(abs(0 - min(est$conf.low,  na.rm = TRUE)),
             abs(0 - max(est$conf.high, na.rm = TRUE)))
  sequencia   <- seq(min(est$conf.low[is.finite(est$conf.low)],  na.rm = TRUE),
                     max(est$conf.high[is.finite(est$conf.high)], na.rm = TRUE),
                     length.out = 1000)
  quantil_10  <- quantile(sequencia, probs = 0.18, names = FALSE)
  quantil_75  <- quantile(sequencia, probs = 0.91, names = FALSE)
  table_pos_y <- ifelse(value[1] < value[2], quantil_75, quantil_10)
  lengend_pos_y <- ifelse(value[1] < value[2], 0.75, 0.10)
  
  # Breaks on the event-time axis (keeps the -16 to 16 style; step = 2 when possible)
  y_breaks <- tryCatch({
    rng <- range(est$event.time, na.rm = TRUE)
    by2 <- seq(floor(rng[1]), ceiling(rng[2]), by = 2)
    if (length(by2) >= 3) by2 else sort(unique(est$event.time))
  }, error = function(e) sort(unique(est$event.time)))
  
  # Plot (same visual style)
  graph <- ggplot(data = est, aes(y = event.time, x = estimate)) +
    geom_pointrange(aes(xmax = conf.high, xmin = conf.low),
                    linewidth = 0.5, position = position_dodge(width = 0.5),
                    linetype = "blank") +
    geom_errorbar(aes(xmax = conf.high, xmin = conf.low),
                  linewidth = 0.5, width = 0.5, position = position_dodge(width = 0.5)) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = -1) +
    labs(x = "Coefficient", y = "Period", color = "", linetype = "", title = "") +
    scale_y_continuous(breaks = y_breaks) +
    coord_flip() +
    theme_minimal() +
    theme(
      text = element_text(size = 25),
      legend.text = element_text(size = 25),
      legend.title = element_text(size = 25),
      legend.key.width = unit(1.5, "cm"),
      legend.key.height = unit(1, "cm"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = c(0.125, lengend_pos_y)
    )
  
  # Combine with the table (keeps your vertical positioning rule)
  graph + annotation_custom(
    grob = tabela_grob,
    xmin = table_pos_y, xmax = table_pos_y,
    ymin = -15, ymax = -11
  )
}

# --------- Generate plots for the two outcomes ---------
outcomes <- c("lurban_size", "sprawl_index", "landslide_dum")
output   <- lapply(outcomes, ggplot_paper)


ggsave(paste0(path_output_git, "_graph_robustness_extreme_rainfall_mountainous_urban_size.jpg"),
       output[[1]], width = 20, height = 10, units = "in", dpi = 100)

ggsave(paste0(path_output_git, "_graph_robustness_extreme_rainfall_mountainous_sprawl_index.jpg"),
       output[[2]], width = 20, height = 10, units = "in", dpi = 100)

# --------- Correlation (no attach) ---------
corr <- cor(
  dados2$int_hmount_x_rain1d_ff,
  dados2$landslide_dum_ff,
  use = "complete.obs"
)
print(corr)
