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

# Run each script in a clean R session, so that packages and objects loaded by one script do not affect the others.
# To run a single script, define DROPBOX_PATH and GITHUB_PATH as above and source it.
run_script <- function(file) {
  script <- file.path(GITHUB_PATH, "analysis/code", file)
  if (!file.exists(script)) stop("Missing script: ", script)
  quoted <- function(x) encodeString(x, quote = "\"")
  launcher <- tempfile(fileext = ".R")
  on.exit(unlink(launcher), add = TRUE)
  writeLines(c(
    paste0("DROPBOX_PATH <- ", quoted(DROPBOX_PATH)),
    paste0("GITHUB_PATH <- ", quoted(GITHUB_PATH)),
    "setwd(GITHUB_PATH)",
    paste0("source(", quoted(script), ", encoding = \"UTF-8\")")
  ), launcher)
  cat("\n", file, "\n", sep = "")
  status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(launcher))
  if (!identical(status, 0L)) stop("Script failed: ", file)
}

# Run the R script files containing various analyses and graphs
# ---- Figure 1 ----
run_script("Figure 1 - Panel A The Distribution of Landslides Across Space and Time.R")
run_script("Figure 1 - Panel B The Distribution of Landslides Across Space and Time.R")
run_script("Figure 1 - Panel C The Distribution of Landslides Across Space and Time.R")

# ---- Figures 2-8 ----
run_script("Figure 2 - Effect of Landslides on Urban Area.R")
run_script("Figure 3 - Effects of Landslides on Population and Households.R")
run_script("Figure 4 - Effect of Landslides on Urban Fragmentation.R")
run_script("Figure 5 - Effect of Landslides on Land-Use Regulations.R")
run_script("Figure 6 - Effect of Landslides on Local Labor Markets.R")
run_script("Figure 7 - Effect of Landslides on the Urban Areas of High-Risk and Low-Risk Regions.R")
run_script("Figure 8 - Robustness to Alternative Definitions of Risky Locations.R")

# ---- Tables 1-2 ----
run_script("Table 1 - Balance Check for Treated and Control Units.R")
run_script("Table 2 - Effects on Urban Shrinkage.R")

# ---- Section 4.1 ----
run_script("Section 4.1 - Conditional Pre-Test (Callaway and Sant'Anna).R")

# ---- Appendix A ----
# Figure A.1 requires MAPBIOMAS raster files (not included in the replication package)
# run_script("Figure A.1 - The Urban Layout of Selected Brazilian Cities.R")
run_script("Figure A.2 - Evolution of Urban Area for Treated and Control Municipalities.R")
run_script("Figure A.3 - Group-Specific ATTs.R")
run_script("Figure A.4 - Number of Treated Municipalities by Event Time.R")

# ---- Appendix B ----
run_script("Table B.1 - Logit Model and Predictive Performance.R")

# ---- Appendix C ----
run_script("Table C.1 - Sensitivity Analysis HonestDiD.R")

# ---- Appendix D ----
run_script("Figure D.1 - Full Sample.R")
run_script("Figure D.2 - Only Not-Yet Treated Units.R")
run_script("Figure D.3 - Southeast-Only and Southeast-Excluded.R")
run_script("Figure D.4 - Disaster-Monitored Municipalities.R")
run_script("Figure D.5 - Extended Propensity Score Covariates.R")
run_script("Figure D.6 - Excluding Small or Large Municipalities.R")
run_script("Figure D.7 - Balanced Cohorts over a Fixed Window.R")
run_script("Figure D.8 - Alternative Estimators.R")
run_script("Figure D.9a - Alternative Inference Micro-region Clusters.R")
run_script("Figure D.9b - Alternative Inference Matched-Pair Clusters.R")
run_script("Figure D.10 - Aggregation at the Metropolitan Region Level.R")
run_script("Figure D.11 - Alternative Time Horizons.R")
run_script("Figure D.12 - Synthetic Difference-in-Differences.R")
run_script("Figure D.13 - Other Natural Disasters.R")
run_script("Figure D.14 - Only Municipalities Treated Once.R")
run_script("Figure D.15 - Inclusion of Control Variables.R")
run_script("Figure D.16 - State-Specific Shocks.R")
run_script("Figure D.17 - Spatial Spillovers.R")
run_script("Figure D.18 - Extreme Rainfall in Mountain Ranges.R")

# ---- Appendix E ----
run_script("Figure E.1 - Heterogeneity by Skill Group.R")
run_script("Figure E.2 - Heterogeneity by Economic Sector.R")

# ---- Appendix H ----
run_script("Figures H.1 and H.2 - Heterogeneity by Landslide Severity.R")
run_script("Figure H.3 - Urban Core and Fringe.R")
