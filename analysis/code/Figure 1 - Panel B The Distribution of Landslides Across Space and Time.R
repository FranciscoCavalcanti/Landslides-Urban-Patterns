library(tidyverse)
library(ggplot2)
library(geobr)
library(sf)
library(ggspatial)
library(tmap)

# Set the path for the output file
path_input <- paste0(DROPBOX_PATH, "/build/input/")
path_output <- paste0(DROPBOX_PATH, "/build/output/") 
path_output_github <- paste0(GITHUB_PATH, "/analysis/output/") 

#### Open Databases ####
dados <- readRDS(paste0(path_output,"database_panel.rds"))
psm   <- readRDS(paste0(path_output,"restricted_PSM_database.rds"))
psm   <- merge(dados, psm[,c("code","weights")], by="code")
mun   <- geobr::read_municipality(year = 2020, simplified = F) %>% select(c(code = code_muni))
uf    <- geobr::read_state()

# Figure ?: Map of affected municipalities. Ball will affect the intensity
df <- dados %>% select(c(code,year,first_year_landslide))


df <- df %>% 
  #  filter(substr(code,1,1) %in% c('3')) %>% 
  group_by(code) %>% 
  dplyr::summarise(first_year_landslide = max(first_year_landslide)) %>% 
  left_join(mun) %>% 
  st_as_sf()
df <- cbind(df,st_coordinates(st_centroid(df$geom)))
df$interval <- ifelse(between(df$first_year_landslide, 2001, 2005), '2001 - 2005',
                      ifelse(between(df$first_year_landslide, 2006, 2010), '2006 - 2010',
                             ifelse(between(df$first_year_landslide, 2011, 2015), '2011 - 2015',
                                    ifelse(between(df$first_year_landslide, 2016, 2020), '2016 - 2020',''))))
df <- filter(df, interval != '')

# Graph Affected Disaster Municipalities
tmap_mode("plot")

tmap_options(check_and_fix = TRUE)

# Filtrar os dados para o bounding box
df_filtered <- df %>% filter(substr(code, 1, 1) %in% c('3'))

# Obter o bounding box dos dados filtrados
bb <- tmaptools::bb(df_filtered)

# Criar o mapa com zoom baseado no bounding box, mas incluindo todos os dados
plot <- tm_shape(df, bbox = bb) +
  tm_polygons(col = "interval", scale = 0.5, n = 5, border.col = "white", 
              palette = "Spectral", title = "Year of the Firstly Landslide Occurrence") +
  tm_shape(uf) +
  tm_borders(col = "grey40", lwd = 1) +
  tm_layout(
    inner.margins = c(0.15, 0.15, 0.15, 0.1),
    legend.stack = "horizontal",
    legend.bg.color = "white",
    legend.bg.alpha = 0.9,
    legend.outside = FALSE,                 # coloca DENTRO do mapa
    legend.position = c("right", "bottom"), # canto inferior direito
    legend.frame = FALSE                    # tira a borda preta
  ) +
  tmap_style("white")


tmap_save(plot, file = paste0(path_output_github, "_graph_affected_disaster_south_southeast_municipalities.png"), 
          dpi = 300, height = 14, width = 17, units = "cm")
