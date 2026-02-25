library(dplyr)
library(fixest)
library(did)
library(remotes)
library(did2s)
library(MatchIt)
library(pscl)
library(modelsummary)

path_input <- paste0(DROPBOX_PATH, "/build/input/")
path_output <- paste0(DROPBOX_PATH, "/build/output/") 
path_output_git <- paste0(GITHUB_PATH, "/analysis/output/") 

#### Open Database for Pre-Processing PSM ####

dados_psm <- readRDS(paste0(path_output,"database_two_periods.rds"))
psm <- readRDS(paste0(path_output, "restricted_PSM_database.rds"))

#### Selecting the Treated/Control Group Using PSM ####

dados2 <- merge(dados_psm, psm[, c("code", "weights")], by = "code")

#### Creating new Variables ####

dados2$post <- ifelse(dados2$year == 2010, 1L, 0L)
dados2$prop_inadequados <- with(dados2, inap_houses / total_houses)
dados2$did <- with(dados2, landslide * post)

#### Keep only observations where the first landslide year is 2010 or earlier ####

dados3 <- subset(dados2, is.na(first_year_landslide) | first_year_landslide <= 2010)

### Estimating DiD with two Periods ###

reg1 <- feols(prop_inadequados ~ did | code + year, data = dados3, cluster = ~code)
summary(reg1)

reg2 <- feols(log(urban_size) ~ did | code + year, data = dados3, cluster = ~code)
summary(reg2)

reg3 <- feols(sprawl_index ~ did | code + year, data = dados3, cluster = ~code)
summary(reg3)

reg4 <- feols(log(urban_population) ~ did | code + year, data = dados3, cluster = ~code)
summary(reg4)


### Creating Table and Saving the Results ###

models <- list(
  "Share of Inadequate Housing" = reg1,
  "Log Urban Area Size"   = reg2,
  "Fragmentation Index"   = reg3,
  "Log Urban Population"   = reg4
)

out_xlsx <- paste0(path_output_git, "_did_table.xlsx")

msummary(
  models,
  coef_map = c("did" = "Landslide × Post"),
  gof_map  = c("nobs", "r.squared", "adj.r.squared"),
  fmt      = 4,
  output   = out_xlsx
)


##### Placebo Checking #######

### Subseting the Data ###

dados_pl <- subset(dados2, first_year_landslide == 0 | first_year_landslide > 2010)

#### Creating new Variables ####

dados_pl$placebo_treated <- ifelse(dados_pl$first_year_landslide > 2010, 1L, 0L)
dados_pl$post <- ifelse(dados_pl$year == 2010, 1L, 0L)
dados_pl$did_placebo <- dados_pl$placebo_treated * dados_pl$post




pl1 <- feols(prop_inadequados ~ did_placebo | code + year, data = dados_pl, cluster = ~code)
pl2 <- feols(prop_adequados   ~ did_placebo | code + year, data = dados_pl, cluster = ~code)
pl3 <- feols(log(urban_size)  ~ did_placebo | code + year, data = dados_pl, cluster = ~code)
pl4 <- feols(log(urban_population) ~ did_placebo | code + year, data = dados_pl, cluster = ~code)
pl5 <- feols(log(sprawl_index) ~ did_placebo | code + year, data = dados_pl, cluster = ~code)



models2 <- list(
  "Share of Inadequate Housing" = pl1,
  "Share of Adequate Housing"   = pl2,
  "Log Urban Area Size"   = pl3,
  "Log Urban Population"   = pl4,
  "Fragmentation Index"       = pl5
)

out_xlsx2 <- paste0(path_output_git, "_did_table_pacebo.xlsx")

msummary(
  models2,
  coef_map = c("did_placebo" = "Landslide × Post"),
  gof_map  = c("nobs", "r.squared", "adj.r.squared"),
  fmt      = 4,
  output   = out_xlsx2
)


