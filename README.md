# Do Landslides Impact Urbanization Patterns? Evidence from Brazilian Cities - Replication Package

**Authors:** Ricardo C. A. Lima, Pedro Jorge Alves, Bruno Barsanetti, and Francisco Cavalcanti

## Overview

You can download the replication package **[here](https://www.dropbox.com/scl/fo/5urmqltc3bvohn5qtwkbl/AKb4FIZIJ8iTR9KZlEx94t8?rlkey=1w1f79ksry5852665lvserdv0&st=3krxjljh&dl=0)** (about 26 MB).

The folder is subdivided into the following directories: 1) [build](./build), 2) [analysis](./analysis). Their purposes are described below:

1. **[build](./build)**: This folder contains raw data and the code to extract and clean the data. The output of this folder should be a "clean" database used for the analysis.
2. **[analysis](./analysis)**: This folder contains the implementation of the analyses and their results, such as tables and graphs.

The analysis folder is further subdivided into: a) **code**, b) **output**, described as follows:

- **code**: This folder is for storing the code used for the respective task.
- **output**: This folder is for storing the results according to the “input” and the “code”.

In the `build/output` folder, there are three important databases for replicating the work:
- `database_panel.rds`: This file contains the annual panel data of Brazilian municipalities.
- `restricted_PSM_database.rds`: This file contains the selection of municipalities through the propensity score method.
- `database_two_periods.rds`: This file contains the decadal panel data of Brazilian municipalities.

## Analysis

To replicate the results, please follow these steps:

1. Download and extract the replication package to a folder of your choice: [Replication Package](https://www.dropbox.com/scl/fo/5urmqltc3bvohn5qtwkbl/AKb4FIZIJ8iTR9KZlEx94t8?rlkey=1w1f79ksry5852665lvserdv0&st=3krxjljh&dl=0)
2. Open the code file (`Main_analysis.R`) located in the folder (`.\analysis\code`) using RStudio.
3. Set the working directory to the folder where the replication package has been extracted.
4. Change the path to the main analysis folder specified at lines 8 and 9 in the code file to the directory where the replication package is located.
5. Run the `Main_analysis.R` to generate the results. The output files (both in PNG and TeX format) will be saved automatically in the folder (`.\analysis\output`).

## Code and Corresponding Results

Here is a detailed explanation of the codes within `Main_analysis.R` and the results they generate:

### Figure 1 Descriptive Analysis of Landslides
- `.\analysis\code\_graph_affected_disaster_municipalities.R` - Spatial Distribution of Landslides
- `.\analysis\code\_graph_affected_disaster_south_southeast_municipalities.R` - Landslides in the Southeast
- `.\analysis\code\_graph_disasters_occourence_by_year_region.R` - Annual Occurrence of Landslides

### Figure 2 Landslides Effects on Urbanization Patterns
- `.\analysis\code\_graph_main_urban_size.R`

### Figure 3 Effect of Landslides on Local Labor Markets
- `.\analysis\code\_graph_staggered_DID_Mechanisms_3.R`

### Figure 4 Effect of Landslides on Land-Use Regulations
- `.\analysis\code\_graph_staggered_DID_Mechanisms_2.R`

### Figure 5 Effect of Landslides on the Urban Areas of High-Risk and Low-Risk Regions
- `.\analysis\code\_graph_mechanism_amenities.R`

### Appendix Figure B.1 The Urban Layout of Selected Brazilian Cities
- `.\analysis\code\_graph_urban_spraw_examples.R`

### Appendix Figure B.2 Landslides Effects on Urbanization Patterns: group-specific ATTs
- `.\analysis\code\_graph_appendix_group_hetero.R`

### Appendix Table C.1 Balance Check for Treated and Control Units Before and After the Matching
- `.\analysis\code\_table_balance_test.R`

### Appendix Figure D.1 Robustness Check: Full Sample
- `.\analysis\code\_graph_robustness_broader_sample.R`

### Appendix Figure D.2 Robustness Check: Considering Only Not-Yet Treated Units
### Appendix Figure D.3 Robustness Check: Southeast Municipalities Only
### Appendix Figure D.4 Robustness Check: Disaster Monitored Municipalities
### Appendix Figure D.5 Robustness Check: Other Natural Disasters
- `.\analysis\code\_graph_robustness_checks_1.R`

### Appendix Figure D.6 Robustness Check: Inclusion of Control Variables
- `.\analysis\code\_graph_robustness_inclusion_control_variables.R`

### Appendix Figure D.7 Robustness Check: Alternative Estimators
- `.\analysis\code\_graph_alternative_estimators_.R`

### Appendix Figure D.8 Robustness Check: Standard Errors Clustered by Micro-region
- `.\analysis\code\_graph_robustness_alternative_SEs_.R`

### Appendix Figure D.9 Robustness Check: The Spatial Spillovers of Landslides
- `.\analysis\code\_graph_robustness_spillover_DID.R`