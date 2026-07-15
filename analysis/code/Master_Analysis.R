####################
# Folder Path
####################

# Initialize the paths with placeholder text
# DROPBOX_PATH: folder where the replication package (datasets) was extracted
# GITHUB_PATH:  folder of this repository
# For example:
# DROPBOX_PATH <- "C:/User/Landslides-Urban-Patterns"
# GITHUB_PATH <- "C:/User/Landslides-Urban-Patterns"

DROPBOX_PATH <- "INSERT YOUR DIRECTORY PATH HERE/Landslides-Urban-Patterns"
GITHUB_PATH <- "INSERT YOUR DIRECTORY PATH HERE/Landslides-Urban-Patterns"

# Change the working directory to the GitHub path
setwd(GITHUB_PATH)

# Source (execute) the R script files containing various analyses and graphs
# ---- Figure 1 ----
source(paste0(GITHUB_PATH, "/analysis/code/Figure 1 - Panel A The Distribution of Landslides Across Space and Time.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure 1 - Panel B The Distribution of Landslides Across Space and Time.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure 1 - Panel C The Distribution of Landslides Across Space and Time.R"))

# ---- Figures 2-7 ----
source(paste0(GITHUB_PATH, "/analysis/code/Figure 2 - Effect of Landslides on Urban Area.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure 3 - Effects of Landslides on Population and Households.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure 4 - Effect of Landslides on Urban Fragmentation.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure 5 - Effect of Landslides on Land-Use Regulations.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure 6 - Effect of Landslides on Local Labor Markets.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure 7 - Effect of Landslides on the Urban Areas of High-Risk and Low-Risk Regions.R"))

# ---- Tables 1-2 ----
source(paste0(GITHUB_PATH, "/analysis/code/Table 1 - Balance Check for Treated and Control Units.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Table 2 - Effects on Urban Shrinkage.R"))

# ---- Appendix A ----
# Figure A.1 requires MAPBIOMAS raster files (not included in the replication package)
# source(paste0(GITHUB_PATH, "/analysis/code/Figure A.1 - The Urban Layout of Selected Brazilian Cities.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure A.2 - Landslides Effects on Urbanization Patterns group-specific ATTs.R"))

# ---- Appendix B ----
source(paste0(GITHUB_PATH, "/analysis/code/Figure B.1 - Robustness Check Full Sample.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure B.2 - Robustness Check Considering Only Not-Yet Treated Units.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure B.3 - Robustness Check Southeast-Only and Southeast-Excluded.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure B.4 - Robustness Check Disaster Monitored Municipalities.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure B.5 - Robustness Check Other Natural Disasters.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure B.6 - Robustness Check Only municipalities treated once.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure B.7 - Robustness Check Inclusion of Control Variables.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure B.8 - Robustness Check Inclusion of State-by-Year Fixed Effects.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure B.9 - Robustness Check Alternative Estimators.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure B.10 - Robustness Check Standard Errors Clustered by Micro-region.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure B.11 - Robustness Check The Spatial Spillovers of Landslides.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure B.12 - Robustness Check Alternative Time Horizons.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure B.13 - Robustness Check Synthetic Difference-in-Differences.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure B.14 - Robustness to Aggregation at the Urban Agglomeration Level.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure B.15 - Robustness Check Excluding the Smallest and Largest Municipalities.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure B.16 - Robustness Check Effects of Extreme Rainfall in Mountain Ranges.R"))
