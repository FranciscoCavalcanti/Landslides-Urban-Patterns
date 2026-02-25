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
# ---- Figures 1.x ----
source(paste0(GITHUB_PATH, "/analysis/code/Figure 1 - Panel A The Distribution of Landslides Across Space and Time.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure 1 - Panel B The Distribution of Landslides Across Space and Time.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure 1 - Panel C The Distribution of Landslides Across Space and Time.R"))

# ---- Figures 2–5 ----
source(paste0(GITHUB_PATH, "/analysis/code/Figure 2 - Effects of Landslides on Urbanization Patterns.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure 3 - Effect of Landslides on Local Labor Markets.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure 4 - Effect of Landslides on Land-Use Regulations.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure 5 - Effect of Landslides on the Urban Areas of High-Risk and Low-Risk Regions.R"))

# ---- Appendix A ----
# source(paste0(GITHUB_PATH, "/analysis/code/Figure A.1 - The Urban Layout of Selected Brazilian Cities.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure A.2 - Landslides Effects on Urbanization Patterns group-specific ATTs.R"))

# ---- Appendix B ----
source(paste0(GITHUB_PATH, "/analysis/code/Table B.1 - Balance Check for Treated and Control Units Before and After the Matching.R"))

# ---- Appendix C ----
source(paste0(GITHUB_PATH, "/analysis/code/Figure C.1 - Robustness Check Full Sample.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure C.2 - Robustness Check Considering Only Not-Yet Treated Units.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure C.3 - Robustness Check Southeast-Only and Southeast-Excluded.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure C.4 - Robustness Check Disaster Monitored Municipalities.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure C.5 - Robustness Check Other Natural Disasters.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure C.6 - Robustness Check Only municipalities treated once.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure C.7 - Robustness Check Inclusion of Control Variables.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure C.8 - Robustness Check Accounting for State-Specific Shocks.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure C.9 - Robustness Check Alternative Estimators.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure C.10 - Robustness Check Standard Errors Clustered by Micro-region.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure C.11 - Robustness Check The Spatial Spillovers of Landslides.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure C.12 - Robustness Check Alternative Time Horizons.R"))
#source(paste0(GITHUB_PATH, "/analysis/code/Figure C.13 - Robustness Check Synthetic Difference-in-Differences.do"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure C.14 - Robustness Check Aggregation at the Metropolitan Region Level.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure C.15 - Robustness Check Excluding Smallest and Largest Municipalities.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure C.16 - Robustness Check Effects of Extreme Rainfall in Mountain Ranges.R"))

# ---- Appendix D ----
source(paste0(GITHUB_PATH, "analysis/code/Figure D.1 Heterogeneous Effects of Landslides by Municipal Characteristics.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Figure D.2 Effects of Landslides on Population and Number of Households.R"))
source(paste0(GITHUB_PATH, "/analysis/code/Table D.1  - Long-Term Differences.R"))


