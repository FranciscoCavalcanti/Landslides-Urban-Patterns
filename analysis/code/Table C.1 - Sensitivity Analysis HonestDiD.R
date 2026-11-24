# Table C.1: HonestDiD (Rambachan and Roth, 2023) sensitivity of the overall ATT; relative-magnitude bounds, pre-treatment windows anchored at e = -2. Matched sample.
# Output: analysis/output/_table_C1_honestdid.tex, honestdid_table.csv

library(dplyr); library(did); library(MatchIt); library(HonestDiD); library(parallel)
set.seed(123)

data_dir <- paste0(DROPBOX_PATH, "/build/output")
dados_psm2 <- readRDS(file.path(data_dir, "database_two_periods.rds")) %>% filter(year == 2000)
dados_psm2$region <- factor(dados_psm2$region, levels = c("Midwest", "North", "Northeast", "South", "Southeast"))

psm_covariates <- c("prec_verao", "prec_outono", "prec_inverno", "prec_primav",
                     "altitude", "temp_verao", "temp_inverno", "temp_primav", "temp_outono",
                     "avg_tri", "avg_slope", "region")
dados_psm4 <- dados_psm2[complete.cases(dados_psm2[, psm_covariates]), ]

PSM <- matchit(landslide ~ prec_verao + prec_outono + prec_inverno + prec_primav +
                 altitude + temp_verao + temp_inverno + temp_primav + temp_outono +
                 avg_tri + avg_slope + region,
               data = dados_psm4, method = "nearest")
m.data <- match.data(PSM)
cat("Matched sample:", sum(m.data$landslide == 1), "treated,", sum(m.data$landslide == 0), "controls\n\n")

dados <- readRDS(file.path(data_dir, "database_panel.rds"))
dados2 <- merge(dados, m.data[, c("code", "weights")], by = "code")

es <- aggte(att_gt(yname = "lurban_size", gname = "first_year_landslide", idname = "code",
                   tname = "year", base_period = "universal", data = dados2),
            type = "dynamic", na.rm = TRUE)
k <- es$egt != -1
e <- es$egt[k]; b <- es$att.egt[k]; psi <- es$inf.function$dynamic.inf.func.e[, k]; n <- nrow(psi)

janelas <- setNames(lapply(3:17, function(fim) -2:-fim),
                     sapply(3:17, function(fim) sprintf("e = -2 to -%d", fim)))
M_values <- c(0.25, 0.5, 0.75, 1)

ic <- function(t) {
  j <- e %in% janelas[[t$janela]] | e >= 0
  V <- crossprod(psi[, j]) / n^2; pre <- sum(e[j] < 0); pos <- sum(e[j] >= 0); l <- rep(1 / pos, pos)
  r <- suppressWarnings(createSensitivityResults_relativeMagnitudes(
    betahat = b[j],
    sigma = V,
    numPrePeriods = pre,
    numPostPeriods = pos,
    l_vec = l,
    Mbarvec = M_values
  ))
  data.frame(janela = t$janela, n_leads = pre, M = M_values, lb = r$lb, ub = r$ub)
}
g <- data.frame(janela = names(janelas), stringsAsFactors = FALSE)
workers <- min(16L, parallel::detectCores())
tasks <- split(g, seq_len(nrow(g)))
if (.Platform$OS.type == "windows" && workers > 1L) {
  cluster <- parallel::makeCluster(workers)
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  parallel::clusterEvalQ(cluster, library(HonestDiD))
  parallel::clusterExport(
    cluster,
    c("ic", "e", "janelas", "M_values", "b", "psi", "n"),
    envir = environment()
  )
  results <- parallel::parLapply(cluster, tasks, function(x) ic(as.list(x)))
  parallel::stopCluster(cluster)
  on.exit(NULL, add = FALSE)
} else {
  results <- parallel::mclapply(
    tasks,
    function(x) ic(as.list(x)),
    mc.cores = workers,
    mc.preschedule = FALSE
  )
}
res <- do.call(rbind, results)

res$janela <- factor(res$janela, levels = names(janelas))
res <- res[order(res$janela, res$M), ]
write.csv(res, paste0(GITHUB_PATH, "/analysis/output/honestdid_table.csv"), row.names = FALSE)

Ms <- M_values
num <- function(x) sprintf("$%.3f$", x)
rows <- sapply(levels(res$janela), function(w) {
  fim <- sub("e = -2 to -", "", w)
  cells <- sapply(Ms, function(m) { r <- res[res$janela == w & res$M == m, ]; sprintf("[%s, %s]", num(r$lb), num(r$ub)) })
  paste0("$e = -2$ to $-", fim, "$ & ", paste(cells, collapse = " & "), " \\\\")
})
tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Sensitivity Analysis: Robustness to Violations of Parallel Trends}",
  "\\label{tab:honestdid}",
  "\\small",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  "Leads used & $\\bar{M}=0.25$ & $\\bar{M}=0.5$ & $\\bar{M}=0.75$ & $\\bar{M}=1.0$ \\\\",
  "\\midrule",
  rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\vspace{2pt}",
  "\\begin{minipage}{0.95\\textwidth}",
  "\\scriptsize",
  "Notes: Robust 95\\% confidence intervals for the average post-treatment ATT under the relative-magnitude restriction $\\Delta^{RM}(\\bar{M})$ \\cite{rambachan2023more}. Each row anchors the benchmark window at $e=-2$ and extends it one lead further back.",
  "\\end{minipage}",
  "\\end{table}")
writeLines(tex, paste0(GITHUB_PATH, "/analysis/output/_table_C1_honestdid.tex"))
res$ic <- sprintf("[%.3f, %.3f]", res$lb, res$ub)
print(reshape(res[res$M != 0, c("janela", "M", "ic")], idvar = "janela", timevar = "M", direction = "wide"), row.names = FALSE)
