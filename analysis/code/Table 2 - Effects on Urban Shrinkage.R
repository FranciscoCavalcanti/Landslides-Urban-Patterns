library(dplyr)
library(fixest)

path_output     <- paste0(DROPBOX_PATH, "/build/output/")
path_output_git <- paste0(GITHUB_PATH,  "/analysis/output/")

# ── Load and prepare data ────────────────────────────────────────────────────
dados_psm <- readRDS(paste0(path_output, "database_two_periods.rds"))
psm       <- readRDS(paste0(path_output, "restricted_PSM_database.rds"))

dados2 <- merge(dados_psm, psm[, c("code", "weights")], by = "code") |>
  mutate(
    in_migration        = ifelse(is.na(in_migration), 0, in_migration),
    prop_inadequados    = inap_houses / total_houses,
    share_in_migration  = in_migration / population,
    post                = ifelse(year == 2010, 1L, 0L),
    did                 = landslide * post
  )

# Main sample: landslide in 2010 or earlier, or never treated
dados3 <- subset(dados2, is.na(first_year_landslide) | first_year_landslide <= 2010)

cat("Sample:", nrow(dados3), "obs | Treated (did=1):", sum(dados3$did), "\n")

# ── Regressions ───────────────────────────────────────────────────────────────
reg1 <- feols(prop_inadequados   ~ did | code + year, data = dados3, cluster = ~code)
reg2 <- feols(share_in_migration ~ did | code + year, data = dados3, cluster = ~code)
reg3 <- feols(log(avg_income)    ~ did | code + year, data = dados3, cluster = ~code)
reg4 <- feols(log(urban_population) ~ did | code + year, data = dados3, cluster = ~code)
reg5 <- feols(log(urban_size)    ~ did | code + year, data = dados3, cluster = ~code)

models <- list(reg1, reg2, reg3, reg4, reg5)

# ── Helper functions ──────────────────────────────────────────────────────────
stars <- function(pval) {
  if (is.na(pval))  return("")
  if (pval < 0.01)  return("***")
  if (pval < 0.05)  return("**")
  if (pval < 0.10)  return("*")
  return("")
}
fmt_coef <- function(m) {
  b  <- coef(m)["did"]
  pv <- pvalue(m)["did"]
  sprintf("%.4f%s", b, stars(pv))
}
fmt_se   <- function(m) sprintf("(%.4f)", sqrt(diag(vcov(m)))["did"])
fmt_nobs <- function(m) formatC(nobs(m), format = "d", big.mark = ",")
fmt_r2   <- function(m) sprintf("%.3f", fitstat(m, "ar2")[[1]])

# ── Console summary ───────────────────────────────────────────────────────────
labels <- c("Share Inadequate Hous.", "Share In-Migration",
            "Log Per Capita Income", "Log Urban Population", "Log Urban Area")
cat("\n=== Table D.2 Results ===\n")
for (i in seq_along(models))
  cat(sprintf("%-25s  %s  %s\n", labels[i], fmt_coef(models[[i]]), fmt_se(models[[i]])))

# ── Build LaTeX table ─────────────────────────────────────────────────────────
h1 <- c("\\multicolumn{1}{c}{Share of}",
        "\\multicolumn{1}{c}{Share of}",
        "\\multicolumn{1}{c}{Log Per}",
        "\\multicolumn{1}{c}{Log Urban}",
        "\\multicolumn{1}{c}{Log Urban}")
h2 <- c("\\multicolumn{1}{c}{Inadequate Hous.}",
        "\\multicolumn{1}{c}{In-Migration}",
        "\\multicolumn{1}{c}{Capita Income}",
        "\\multicolumn{1}{c}{Population}",
        "\\multicolumn{1}{c}{Area Size}")

coef_row <- paste(sapply(models, fmt_coef), collapse = " & ")
se_row   <- paste(sapply(models, fmt_se),   collapse = " & ")
nobs_row <- paste(sapply(models, fmt_nobs), collapse = " & ")
r2_row   <- paste(sapply(models, fmt_r2),   collapse = " & ")

tex <- paste0(
'\\begin{table}[H]
\\centering
\\scalebox{0.85}{
\\begin{threeparttable}
\\caption{Effects of Landslides on Urban Shrinkage}
\\label{tab:urban_shrinkage}

\\begin{tabular}{lccccc}
\\toprule
 & (1) & (2) & (3) & (4) & (5) \\\\
\\cmidrule(lr){2-2}\\cmidrule(lr){3-3}\\cmidrule(lr){4-4}\\cmidrule(lr){5-5}\\cmidrule(lr){6-6}
 & ', paste(h1, collapse = '\n & '), ' \\\\
 & ', paste(h2, collapse = '\n & '), ' \\\\
\\midrule
$Landslide_{i}\\times Post_{t}$  & ', coef_row, ' \\\\
                                  & ', se_row,   ' \\\\
\\midrule
Municipality Fixed Effects        & Yes & Yes & Yes & Yes & Yes \\\\
Time Fixed Effects                & Yes & Yes & Yes & Yes & Yes \\\\
Number of Observations            & ', nobs_row, ' \\\\
Adjusted $R^{2}$                  & ', r2_row,   ' \\\\
\\bottomrule
\\bottomrule
\\end{tabular}

\\begin{tablenotes}[flushleft]
\\footnotesize
\\item \\textit{Notes:} This table reports two-way fixed-effects estimates of the effect of landslides on urban outcomes. The variable $Landslide_{i}\\times Post_{t}$ equals one for treated municipalities in the post-event period and zero otherwise. Share of inadequate housing is the ratio of inadequate dwellings to total dwellings. Share of in-migration is the number of in-migrants divided by total population. All specifications include municipality and year fixed effects. Standard errors clustered at the municipality level are reported in parentheses. Significance levels: *** $p<0.01$, ** $p<0.05$, * $p<0.10$.
\\end{tablenotes}

\\end{threeparttable}
}
\\end{table}
')

out_path <- paste0(path_output_git, "_table_D2_urban_shrinkage.tex")
writeLines(tex, out_path)
cat("\nSaved:", out_path, "\n")
