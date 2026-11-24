# Section 4.1: conditional pre-test of parallel trends (Callaway and Sant'Anna, Cramer-von Mises), matched sample.
# Output: test statistic and p-value printed to the console

suppressMessages({library(dplyr); library(did); library(MatchIt)})
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
dados3 <- dados2 %>% filter(first_year_landslide == 0 | first_year_landslide > min(year))

src <- paste(deparse(did::conditional_did_pretest), collapse = "\n")
src2 <- sub("dp <- pre_process_did(yname = yname,",
            "dp <- pre_process_did(base_period = \"universal\", yname = yname,", src, fixed = TRUE)
# The package evaluates the test at every row of X. With an intercept-only
# formula all rows are identical, so retaining one distinct evaluation point is
# algebraically equivalent and avoids thousands of duplicate calculations.
src2 <- sub(
  "cat(\"Step 1 of 2: Computing test statistic....\\n\")",
  paste0(
    "n_eval <- if (ncol(X) == 1L && length(unique(X[, 1L])) == 1L) 1L else nrow(X); ",
    "cat(\"Step 1 of 2: Computing test statistic....\\n\")"
  ),
  src2,
  fixed = TRUE
)
src2 <- sub("pbapply::pblapply(1:nrow(X),", "pbapply::pblapply(seq_len(n_eval),", src2, fixed = TRUE)
cdp_universal <- eval(parse(text = src2))
environment(cdp_universal) <- asNamespace("did")

set.seed(123)
biters <- as.integer(Sys.getenv("CS_PRETEST_BITERS", unset = "50000"))
if (is.na(biters) || biters < 1L) stop("CS_PRETEST_BITERS must be a positive integer")
pretest_cores <- as.integer(Sys.getenv(
  "CS_PRETEST_CORES",
  unset = as.character(max(1L, min(12L, parallel::detectCores(logical = FALSE))))
))
if (is.na(pretest_cores) || pretest_cores < 1L) stop("CS_PRETEST_CORES must be a positive integer")
out <- suppressMessages(cdp_universal(yname = "lurban_size", tname = "year", idname = "code",
                      gname = "first_year_landslide", xformla = ~1, data = dados3, panel = TRUE,
                      control_group = "nevertreated", alp = 0.05, biters = biters,
                      clustervars = "code", est_method = "ipw", cores = pretest_cores))

cat("=== Callaway-Sant'Anna Conditional Pre-Test (CvM, base_period = universal) ===\n")
cat("Bootstrap repetitions:", biters, "\n")
cat("Parallel workers:", pretest_cores, "\n")
cat(sprintf("CvM = %.4f, Critical Value (5%%) = %.4f, p = %.5f\n", out$CvM, quantile(out$CvMb, 0.95), out$CvMpval))
cat("\nDONE\n")
