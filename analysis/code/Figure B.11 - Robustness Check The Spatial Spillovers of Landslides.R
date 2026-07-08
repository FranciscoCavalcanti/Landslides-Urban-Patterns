# Load required libraries
library(dplyr)       # For data manipulation
library(fixest)      # For fixed effects estimation
library(did)         # For difference-in-differences estimation
library(remotes)     # For installing R packages from remote repositories
library(ggplot2)     # For creating plots
library(gridExtra)   # For arranging multiple plots
library(broom)
library(sf)          # Spatial operations
library(arrow)       # Read parquet (cached municipality geometries; replaces broken geobr 2.0.0)
library(data.table)  # For data.table used in the table
library(grid)        # For unit()

# Set Seed
set.seed(123)

# Define paths for input and output files

# Set Seed
set.seed(123)

# Set the paths for input and output files
path_input <- paste0(DROPBOX_PATH, "/build/input/")
path_output <- paste0(DROPBOX_PATH, "/build/output/") 
path_output_git <- paste0(GITHUB_PATH, "/analysis/output/") 

#### Open Databases ####

# Load datasets from the specified file paths
dados <- readRDS(paste0(path_output, "database_panel.rds"))
psm <- readRDS(paste0(path_output, "restricted_PSM_database.rds"))


#### Selecting the Treated/Control Group Using PSM ####
dados2 <- merge(dados, psm[, c("code", "weights")], by = "code")

#### Robustness 1 - Remove NEVER-treated municipalities that share an IMMEDIATE border with treated municipalities ####

# (Optional but recommended) Work only with municipalities that exist in your sample (dados2)
codes_sample <- sort(unique(dados2$code))

# Read municipality geometries from cached parquet — geobr 2.0.0 download consistently fails.
# Parquet source: IPEA, cached at analysis/output/mun2020_simplified.parquet.
# WKB conversion: arrow_binary must be cast to raw + class "WKB" before sf can read it.
parquet_path <- paste0(GITHUB_PATH, "/analysis/output/mun2020_simplified.parquet")
pq_raw    <- arrow::read_parquet(parquet_path)
wkb_list  <- lapply(pq_raw[["geometry"]], function(x) structure(as.raw(x), class = "WKB"))
mun_geom  <- sf::st_sf(data.frame(code_muni = pq_raw[["code_muni"]]),
                        geometry = sf::st_as_sfc(wkb_list, crs = 4674)) %>%
  dplyr::select(code = code_muni) %>%
  dplyr::filter(code %in% codes_sample) %>%
  dplyr::left_join(dados %>% dplyr::distinct(code, first_year_landslide), by = "code")

# If you face geometry validity issues uncomment the next line (can be slower)
# mun_geom <- sf::st_make_valid(mun_geom)

# Adjacency list: neighbors share a boundary
matriz_adj <- sf::st_touches(mun_geom)

# Fast lookup vector for first_year_landslide by code
fy <- dados %>%
  dplyr::distinct(code, first_year_landslide) %>%
  { setNames(.$first_year_landslide, .$code) }

# Vectorized construction of all (city, neighbor) pairs
from_idx <- rep(seq_along(matriz_adj), lengths(matriz_adj))
to_idx   <- unlist(matriz_adj)

pairs <- data.frame(
  code_city         = mun_geom$code[from_idx],
  code_neighborhood = mun_geom$code[to_idx]
)

pairs$first_year_landslide_city         <- fy[as.character(pairs$code_city)]
pairs$first_year_landslide_neighborhood <- fy[as.character(pairs$code_neighborhood)]

# Identify NEVER-treated neighbors that border any treated municipality
# Assumption: never-treated is coded as 0 in first_year_landslide
border_never_treated <- pairs %>%
  dplyr::filter(first_year_landslide_city != 0,
                first_year_landslide_neighborhood == 0) %>%
  dplyr::distinct(code_neighborhood) %>%
  dplyr::pull(code_neighborhood)

# Remove those NEVER-treated municipalities from the analysis sample
dados2_robustness <- dados2 %>%
  dplyr::filter(!(code %in% border_never_treated))

#### Plot function for robustness checks ####
ggplot_paper <- function(x){
  
  mw.dyn_p <- aggte(
    att_gt(
      yname = x,
      gname = "first_year_landslide",
      idname = "code",
      bstrap = TRUE,
      clustervars = "code",
      base_period = "universal",
      tname = "year",
      data = dados2_robustness
      # If you want faster runs while testing, uncomment:
      # , biters = 200
    ),
    type = "dynamic"
  )
  
  est <- broom::tidy(mw.dyn_p) %>%
    dplyr::select(event.time, estimate, std.error, conf.low, conf.high) %>%
    dplyr::mutate(est = "Matched Sample")
  
  # Z values for different confidence levels
  z_99 <- 2.576
  z_95 <- 1.96
  z_90 <- 1.645
  
  # Check significance for the overall ATT
  IC_99 <- mw.dyn_p$overall.att + c(-z_99 * mw.dyn_p$overall.se, z_99 * mw.dyn_p$overall.se)
  IC_95 <- mw.dyn_p$overall.att + c(-z_95 * mw.dyn_p$overall.se, z_95 * mw.dyn_p$overall.se)
  IC_90 <- mw.dyn_p$overall.att + c(-z_90 * mw.dyn_p$overall.se, z_90 * mw.dyn_p$overall.se)
  
  ATT_significance_p <- ifelse(all(IC_99 < 0) | all(IC_99 > 0), paste0(round(mw.dyn_p$overall.att, 4), "***"),
                               ifelse(all(IC_95 < 0) | all(IC_95 > 0), paste0(round(mw.dyn_p$overall.att, 4), "**"),
                                      ifelse(all(IC_90 < 0) | all(IC_90 > 0), paste0(round(mw.dyn_p$overall.att, 4), "*"),
                                             paste0(round(mw.dyn_p$overall.att, 4)) )))
  
  # Table (grob)
  dados_tabela <- data.table::data.table(
    ATT = c(ATT_significance_p, paste0("(", round(mw.dyn_p$overall.se, 4), ")"))
  )
  
  value <- c(abs(0 - summary(est$conf.low)[1]), abs(0 - summary(est$conf.high)[6]))
  sequencia <- seq(min(est$conf.low, na.rm = TRUE),
                   max(est$conf.high, na.rm = TRUE),
                   length.out = 1000)
  
  quantil_10 <- quantile(sequencia, probs = 0.18)
  quantil_75 <- quantile(sequencia, probs = 0.91)
  
  table_pos_y   <- ifelse(value[1] < value[2], quantil_75, quantil_10)
  lengend_pos_y <- ifelse(value[1] < value[2], 0.75, 0.10)
  
  tabela_grob <- gridExtra::tableGrob(
    dados_tabela,
    rows = NULL,
    theme = gridExtra::ttheme_minimal(
      core    = list(fg_params = list(fontsize = 30)),
      colhead = list(fg_params = list(fontsize = 30, fontface = "bold")),
      rowhead = list(fg_params = list(fontsize = 30))
    )
  )
  
  graph <- ggplot(data = est, aes(y = event.time, x = estimate)) +
    geom_pointrange(
      aes(xmax = conf.high, xmin = conf.low),
      linewidth = 0.5,
      position = position_dodge(width = 0.5),
      linetype = "blank"
    ) +
    geom_errorbar(
      aes(xmax = conf.high, xmin = conf.low),
      linewidth = 0.5,
      width = 0.5,
      position = position_dodge(width = 0.5)
    ) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = -1) +
    labs(x = "Coefficient", y = "Period", color = "", linetype = "", title = "") +
    scale_y_continuous(breaks = seq(-16, 16, by = 2)) +
    coord_flip() +
    theme_minimal() +
    theme(
      text = element_text(size = 30),
      legend.text = element_text(size = 25),
      legend.title = element_text(size = 25),
      legend.key.width = grid::unit(1.5, "cm"),
      legend.key.height = grid::unit(1, "cm"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = c(0.15, lengend_pos_y)
    )
  
  graph <- graph + annotation_custom(
    grob = tabela_grob,
    xmin = table_pos_y,
    xmax = table_pos_y,
    ymin = -15,
    ymax = -11
  )
  
  return(graph)
}

# Generate plots for lurban_size
output_robustness <- lapply(c("lurban_size"), ggplot_paper)

## Saving DiD plots

output_path <- paste0(path_output_git, "_graph_robustness_checks_urban_size_neighbour.jpg")
ggsave(output_path, output_robustness[[1]], width = 20, height = 10, units = "in", dpi = 100)

