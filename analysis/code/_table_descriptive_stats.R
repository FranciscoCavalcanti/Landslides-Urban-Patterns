# Load necessary packages
library(dplyr)
library(vtable)

# Set paths for input and output files
path_output <- paste0(DROPBOX_PATH, "/build/output/")
path_output_github <- paste0(GITHUB_PATH, "/analysis/output/")

#### Open Databases ####

# Load datasets
dados <- readRDS(paste0(path_output, "database_panel.rds"))
psm <- readRDS(paste0(path_output, "restricted_PSM_database.rds"))

#### Creating Variables #####

# Create new variables: GDP per capita, log GDP per capita, and landslide status
dados <- dados %>%
  mutate(
    gdp_pc = gdp / population,
    lgdp_pc = log(gdp_pc + 1),
    landslide_status = ifelse(first_year_landslide > 0, 1, 0)
  )

# Merge PSM dataset and create a flag for PSM inclusion
dados <- dados %>%
  left_join(psm %>% mutate(in_psm = 1) %>% select(code, in_psm), by = "code") %>%
  mutate(in_psm = ifelse(is.na(in_psm), 0, 1))

#### Separating Groups for Summary Statistics ####

# Create a new variable 'treatment_status' to separate groups for summary statistics
dados <- dados %>%
  mutate(treatment_status = if_else(landslide_status == 1 & year == 2003, "1. Treated Units (2003)",
                                    if_else(landslide_status == 0 & year == 2003 & in_psm == 1, "3. Matched Control Units (2003)",
                                            if_else(landslide_status == 0 & year == 2003, "2. Control Units (2003)",
                                                    if_else(landslide_status == 1 & year == 2020, "4. Treated Units (2020) ",
                                                            if_else(landslide_status == 0 & year == 2020 & in_psm == 1, "6. Matched Control Units (2020)",
                                                                    if_else(landslide_status == 0 & year == 2020, "5. Control Units (2020)", NA_character_)))))))

# Display counts for each treatment status group
table(dados$treatment_status)

#### Summary Statistics #####

# Select variables for summary statistics
variable_list <- dados %>% select(lurban_size, sprawl_index.x, lpopulation, lgdp_pc, lfinanc_imob,
                                  dum_financ, zoning_policy, ugb_policy, subdivision_policy,
                                  building_policy, treatment_status)

# Define labels for the variables
labs <- c('ln(Urban Area Size)',
          'Fragmentation Index',
          'ln(Population)',
          'ln(GDP per Capita)',
          'ln(Stock of Housing Loans)',
          'Housing Loans (0/1)',
          'Zoning Ordinance (0/1)',
          'Urban Growth Boundary (0/1)',
          'Land Subdivision Regulation (0/1)',
          'Building Code (0/1)')

# Generate summary statistics
sumtable_result <- sumtable(variable_list, labels = labs, summ = c('mean(x)', 'sd(x)'),
                            digits = 3, group = 'treatment_status')

# Display the summary statistics table
summary(sumtable_result)
