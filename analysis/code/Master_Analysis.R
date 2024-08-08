####################
# Folder Path
####################

# Initialize the paths with placeholder text
# For example:
# DROPBOX_PATH <- "C:/User/Landslides-Urban-Patterns"
# GITHUB_PATH <- "C:/User/Landslides-Urban-Patterns"

DROPBOX_PATH <- "INSERT YOUR DIRECTORY PATH HERE/Landslides-Urban-Patterns"
GITHUB_PATH <- "INSERT YOUR DIRECTORY PATH HERE/Landslides-Urban-Patterns"

# Change the working directory to the GitHub path
setwd(GITHUB_PATH)

# Source (execute) the R script files containing various analyses and graphs
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_main_staggered_DID.R"))
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_placebos_checks.R"))
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_alternative_estimators_.R"))
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_robustness_alternative_SEs_.R"))
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_robustness_other_disasters.R"))
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_spillover_DID.R"))
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_staggered_DID_Mechanisms_1.R"))
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_staggered_DID_Mechanisms_2.R"))
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_staggered_DID_Mechanisms_3.R"))

source(file = paste0(GITHUB_PATH, "/analysis/code/_table_balance_test.R"))
source(file = paste0(GITHUB_PATH, "/analysis/code/_table_descriptive_stats.R"))
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_disasters_occourence_by_year_region.R"))
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_affected_disaster_municipalities.R"))
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_affected_disaster_south_southeast_municipalities.R"))
