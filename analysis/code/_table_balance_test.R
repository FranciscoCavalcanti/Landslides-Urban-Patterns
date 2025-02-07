library(tidyverse)

# Set the path for the output file
path_input <- paste0(DROPBOX_PATH, "/build/input/")
path_output <- paste0(DROPBOX_PATH, "/build/output/")
path_output_github <- paste0(GITHUB_PATH, "/analysis/output/")

#### Open Databases ####
# dados <- readRDS(paste0(path_output,"database_panel.rds"))
dados <- readRDS(paste0(path_output, "database_two_periods.rds"))
# colnames(dados)

dados$p_urbana <- dados$urban_population / dados$population
dados$p_informal <- dados$inap_houses / dados$total_houses

dados$log_urban_size <- log(dados$urban_size)
dados$log_avg_income <- log(dados$avg_income)
dados$log_population <- log(dados$population)

#
psm <- readRDS(paste0(path_output, "restricted_PSM_database.rds"))
psm <- merge(dados, psm[, c("code", "weights")], by = "code")

psm$log_urban_size <- log(psm$urban_size)
psm$log_avg_income <- log(psm$avg_income)
psm$log_population <- log(psm$population)

variables_of_interest <- c(
  "altitude", "avg_tri", 
  "prec_verao", "prec_outono", "prec_inverno", "prec_primav",
  "temp_verao", "temp_inverno", "temp_primav", "temp_outono", 
  "log_population", "log_avg_income", "p_urbana", "p_informal",
  "log_urban_size", "sprawl_index","prop_urban_size_high_risk_max_30"
  #"financ_imob"
)


# "region" é caractere, logo não da para fazer teste de diferença de média

# 1-2 columns Brasil: Landslisdes and No landslisdes without psm and No landslisdes with psm
columns_1_2 <- dados %>%
  filter(year == min(year, na.rm = T)) %>%
  mutate(landslide = ifelse(landslide == 1, "Affected BR", "Not affected BR")) %>%
  group_by(landslide) %>%
  dplyr::summarise(across(c(variables_of_interest), list(
    mean = ~ mean(., na.rm = T),
    sd = ~ sd(., na.rm = T)
  ))) %>%
  mutate_if(is.numeric, ~ round(., 2)) %>%
  mutate(across(c(ends_with("sd")), ~ paste0("(", ., ")")))

n_obs <- dados %>%
  filter(year == min(year, na.rm = T)) %>%
  group_by(landslide) %>%
  dplyr::summarise(n_obs = n()) %>%
  mutate(landslide = ifelse(landslide == 1, "Affected", "Not affected Paired")) %>%
  t()

columns_3 <- psm %>%
  filter(year == min(year, na.rm = T), landslide == 0) %>%
  mutate(landslide = ifelse(landslide == 1, "Affected", "Not affected Paired")) %>%
  group_by(landslide) %>%
  dplyr::summarise(across(c(variables_of_interest), list(
    mean = ~ mean(., na.rm = T),
    sd = ~ sd(., na.rm = T)
  ))) %>%
  mutate_if(is.numeric, ~ round(., 2)) %>%
  mutate(across(c(ends_with("sd")), ~ paste0("(", ., ")")))

n_obs_psm <- psm %>%
  filter(year == min(year, na.rm = T), landslide == 0) %>%
  group_by(landslide) %>%
  dplyr::summarise(n_obs = n()) %>%
  mutate(landslide = ifelse(landslide == 1, "Affected", "Not affected Paired")) %>%
  t()

columns_1_3 <- bind_rows(columns_1_2, columns_3)

diff_columns_1_2 <- columns_1_3 %>%
  select(c(landslide, ends_with("mean"))) %>%
  slice(1:2) %>%
  mutate(across(ends_with("mean"), ~ diff(.) * -1)) %>%
  slice(1) %>%
  mutate(landslide = "Diff (1-2)")

diff_columns_1_3 <- columns_1_3 %>%
  select(c(landslide, ends_with("mean"))) %>%
  slice(1, 3) %>%
  mutate(across(ends_with("mean"), ~ diff(.) * -1)) %>%
  slice(1) %>%
  mutate(landslide = "Diff (1-3)")

columns_1_3 <- bind_rows(columns_1_3, diff_columns_1_2, diff_columns_1_3) %>%
  mutate(across(ends_with("sd"), ~ ifelse(is.na(.), "", .))) %>%
  t() %>%
  as.data.frame()

# Function to generate descriptive statistics
table <- as.data.frame(columns_1_3)
table$variables <- rownames(table)
rownames(table) <- 1:nrow(table)
colnames(table) <- table[1, ]
table <- table[-1, ]
table <- table %>%
  relocate(landslide, .before = "Affected BR") %>%
  rename(Variables = landslide)


# Teste de médias
dados_filtrados <- dados %>%
  filter(year == min(year, na.rm = TRUE)) %>%
  mutate(landslide = ifelse(landslide == 1, "Affected BR", "Not affected BR"))

resultados <- list()

for (var in variables_of_interest) {
  t_test <- t.test(as.formula(paste(var, "~ landslide")), data = dados_filtrados)
  resultados[[var]] <- broom::tidy(t_test)
}

# Convertendo a lista de resultados em um dataframe
resultados_df <- bind_rows(resultados, .id = "Variable") %>%
  mutate(
    sd = round(abs(estimate - conf.low), 2),
    p.value = round(p.value, 2)
  ) %>%
  select(c(Variables = Variable, p.value, sd))

resultados_df <- bind_rows(
  resultados_df %>% mutate(Variables = paste0(Variables, "_mean")),
  resultados_df %>% mutate(Variables = paste0(Variables, "_sd"))
)

table <- left_join(table, resultados_df) %>%
  mutate(
    `Diff (1-2)` = as.numeric(`Diff (1-2)`),
    `Diff (1-2)` = ifelse(p.value <= 0.01 & !is.na(`Diff (1-2)`), paste0(format(round(`Diff (1-2)`, 2), nsmall = 2), "***"),
                          ifelse(p.value <= 0.05 & !is.na(`Diff (1-2)`), paste0(format(round(`Diff (1-2)`, 2), nsmall = 2), "**"),
                                 ifelse(p.value <= 0.10 & !is.na(`Diff (1-2)`), paste0(format(round(`Diff (1-2)`, 2), nsmall = 2), "*"),
                                        format(round(`Diff (1-2)`, 2), nsmall = 2))
                          )
    ),
    `Diff (1-2)` = ifelse(`Diff (1-2)` == "    NA",
                          paste0("(", format(round(sd, 2), nsmall = 2), ")"),
                          `Diff (1-2)`),
    `Diff (1-2)` = gsub(" ","",`Diff (1-2)`),
    .keep = "unused"
  )

dados_filtrados <- psm %>%
  filter(year == min(year, na.rm = TRUE)) %>%
  mutate(landslide = ifelse(landslide == 1, "Affected BR", "Not affected BR"))

resultados <- list()

for (var in variables_of_interest) {
  t_test <- t.test(as.formula(paste(var, "~ landslide")), data = dados_filtrados)
  resultados[[var]] <- broom::tidy(t_test)
}

# Convertendo a lista de resultados em um dataframe
resultados_df <- bind_rows(resultados, .id = "Variable") %>%
  mutate(
    sd = round(abs(estimate - conf.low), 2),
    p.value = round(p.value, 2)
  ) %>%
  select(c(Variables = Variable, p.value, sd))

resultados_df <- bind_rows(
  resultados_df %>% mutate(Variables = paste0(Variables, "_mean")),
  resultados_df %>% mutate(Variables = paste0(Variables, "_sd"))
)

table <- left_join(table, resultados_df) %>%
  mutate(
    `Diff (1-3)` = as.numeric(`Diff (1-3)`),
    `Diff (1-3)` = ifelse(p.value <= 0.01 & !is.na(`Diff (1-3)`), paste0(format(round(`Diff (1-3)`, 2), nsmall = 2), "***"),
                   ifelse(p.value <= 0.05 & !is.na(`Diff (1-3)`), paste0(format(round(`Diff (1-3)`, 2), nsmall = 2), "**"),
                   ifelse(p.value <= 0.10 & !is.na(`Diff (1-3)`), paste0(format(round(`Diff (1-3)`, 2), nsmall = 2), "*"),
                   format(round(`Diff (1-3)`, 2), nsmall = 2))
                          )
    ),
    `Diff (1-3)` = ifelse(`Diff (1-3)` == "   NA",
                          paste0("(", format(round(sd, 2), nsmall = 2), ")"),
                          `Diff (1-3)`),
    `Diff (1-3)` = gsub(" ","",`Diff (1-3)`),
    .keep = "unused"
  )



#
table <- table %>%
  mutate(Variables = stringr::str_to_title(gsub("_", " ", gsub(
    "_mean", "",
    ifelse(grepl("_sd", Variables), "", Variables)
  ))))

# relabel variables
table$Variables[table$Variables == "Prec Verao"] <- "Av. precipitation (Summer)"
table$Variables[table$Variables == "Prec Outono"] <- "Av. precipitation (Autumn)"
table$Variables[table$Variables == "Prec Inverno"] <- "Av. precipitation (Winter)"
table$Variables[table$Variables == "Prec Primav"] <- "Av. precipitation (Spring)"
table$Variables[table$Variables == "Altitude"] <- "Altitude (in meters)"
table$Variables[table$Variables == "Temp Verao"] <- "Av. temperature (Summer)"
table$Variables[table$Variables == "Temp Inverno"] <- "Av. temperature (Winter)"
table$Variables[table$Variables == "Temp Primav"] <- "Av. temperature (Spring)"
table$Variables[table$Variables == "Temp Outono"] <- "Av. temperature (Autumn)"
table$Variables[table$Variables == "Log Avg Income"] <- "Log(Average income)"
table$Variables[table$Variables == "P Urbana"] <- "Share of urban population"
table$Variables[table$Variables == "Log Population"] <- "Log(population)"
table$Variables[table$Variables == "P Informal"] <- "Share of informal houses"
table$Variables[table$Variables == "Avg Tri"] <- "Ruggedness Index (TRI) "
table$Variables[table$Variables == "Log Urban Size"] <- "Log(Urban size)"

table <- rbind(table, c("N", as.numeric(n_obs[2, 2]), as.numeric(n_obs[2, 1]), as.numeric(n_obs_psm[2, 1]), "", ""))

# Tabela no tex
output_file <- paste0(path_output_github, "_table_balance_test.tex")

# Open a connection to write LaTeX output to the file
sink(output_file)

# Start generating LaTeX code
cat("\\begin{table}[H]\n")
cat("\\centering\n")
cat("\\label{_table_balance_test}\n")
cat("\\scalebox{0.75}{\n")
cat("\\begin{threeparttable}\n")
cat("\\caption{Balance Test of Sample Selection Using PSM}\n")
cat("\\begin{tabular}{rccccccccc}\n")
cat("\\toprule\n")
cat("\  & (1)   &       & (2)   &       & (3)   &       & (4)   &       & (5) \\\\\n")
cat("\\cmidrule{2-2}\\cmidrule{4-4}\\cmidrule{6-6}\\cmidrule{8-8}\\cmidrule{10-10} \n")

# Print column names
cat(paste0(" & ", paste0(names(table[, -1]), collapse = " & & "), "  \\\\\n"))
cat("\\hline\n")

# Print descriptive statistics for each variable
for (i in 1:nrow(table)) {
  cat(table[i, 1], " & ", paste(table[i, -1], collapse = " & & "), " \\\\\n")
}

# Finish generating LaTeX code
cat("\\hline \n")
cat("\\end{tabular}\n")
cat("\\end{threeparttable}}\n")
cat("\\end{table}\n")

# Close the connection
sink()