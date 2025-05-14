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

### Figure 1: The Distribution of Landslides Across Space and Time
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_affected_disaster_municipalities.R"))
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_affected_disaster_south_southeast_municipalities.R"))
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_disasters_occourence_by_year_region.R"))

### Figure 2: Effects of Landslides on Urbanization Patterns
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_main_staggered_DID.R"))

### Figure 3: Effect of Landslides on Local Labor Markets
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_staggered_DID_Mechanisms_3.R"))

### Figure 4: Effect of Landslides on Land-Use Regulations
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_staggered_DID_Mechanisms_2.R"))

### Figure 5: Effect of Landslides on the Urban Areas of High-Risk and Low-Risk Regions
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_mechanism_amenities.R"))

### Appendix Figure A.1: The Urban Layout of Selected Brazilian Cities
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_urban_spraw_examples.R"))

### Appendix Figure A.2: Landslides Effects on Urbanization Patterns: group-specific ATTs
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_appendix_group_hetero.R"))

### Appendix Table B.1: Balance Check for Treated and Control Units Before and After the Matching
source(file = paste0(GITHUB_PATH, "/analysis/code/_table_balance_test.R"))

### Appendix Figure C.1: Robustness Check: Full Sample
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_robustness_broader_sample.R"))

### Appendix Figure C.2: Robustness Check: Considering Only Not-Yet Treated Units
### Appendix Figure C.3: Robustness Check: Southeast Municipalities Only
### Appendix Figure C.4: Robustness Check: Disaster Monitored Municipalities
### Appendix Figure C.5: Robustness Check: Other Natural Disasters
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_robustness_checks_1.R"))

### Appendix Figure C.6: Robustness Check: Only municipalities treated once
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_robustness_primary_landslide_DID.R"))

### Appendix Figure C.7: Robustness Check: Inclusion of Control Variables
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_robustness_inclusion_control_variables.R"))

### Appendix Figure C.8: Robustness Check: Alternative Estimators
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_alternative_estimators_.R"))

### Appendix Figure C.9: Robustness Check: Standard Errors Clustered by Micro-region
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_robustness_alternative_SEs_.R"))

### Appendix Figure C.10: Robustness Check: The Spatial Spillovers of Landslides
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_robustness_spillover_DID.R"))

### Appendix Figure C.11: Effects of Landslides on Population Size
source(file = paste0(GITHUB_PATH, "/analysis/code/_graph_population_households.R"))