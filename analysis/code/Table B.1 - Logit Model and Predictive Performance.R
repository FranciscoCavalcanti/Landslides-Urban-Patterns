# Table B.1: logit of the propensity score (cross-section of 2000, before matching) and measures of predictive performance
# (McFadden pseudo R2, likelihood-ratio test, cross-validated AUC, Hosmer-Lemeshow, share correctly classified).
# Output: analysis/output/_table_B1_psm_logit.tex, psm_logit_quality.csv

library(dplyr); library(MatchIt)
set.seed(123)
path_output <- paste0(DROPBOX_PATH, "/build/output/"); path_output_git <- paste0(GITHUB_PATH, "/analysis/output/")

d <- readRDS(paste0(path_output, "database_two_periods.rds")) %>% filter(year == 2000)
n_municipalities <- nrow(d)
d$region <- factor(d$region, levels = c("Northeast", "Midwest", "North", "South", "Southeast"))   # reference category: Northeast (the fitted probabilities do not depend on this choice)
v <- c("prec_verao", "prec_outono", "prec_inverno", "prec_primav", "altitude", "temp_verao", "temp_inverno", "temp_primav", "temp_outono", "avg_tri", "avg_slope", "region")
d <- d[complete.cases(d[, c(v, "landslide", "state")]), ]
fml <- landslide ~ prec_verao + prec_outono + prec_inverno + prec_primav + altitude + temp_verao + temp_inverno + temp_primav + temp_outono + avg_tri + avg_slope + region
cat("municipalities:", nrow(d), "| treated:", sum(d$landslide), "\n")

auc <- function(y, p) { r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0); (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
m1 <- glm(fml, data = d, family = binomial)
stats <- function(m, dat) { m0 <- glm(landslide ~ 1, data = dat, family = binomial); lr <- 2 * (as.numeric(logLik(m)) - as.numeric(logLik(m0))); k <- length(coef(m)) - 1
  c(n = nrow(dat), treated = sum(dat$landslide), pseudo_r2 = 1 - as.numeric(logLik(m)) / as.numeric(logLik(m0)), lr_chi2 = lr, lr_p = pchisq(lr, k, lower.tail = FALSE), auc_in = auc(dat$landslide, fitted(m))) }
s1 <- stats(m1, d)

cv_pred <- function() { p <- rep(NA_real_, nrow(d)); f <- integer(nrow(d))
  for (y in 0:1) { i <- which(d$landslide == y); f[i] <- sample(rep(1:10, length.out = length(i))) }
  for (k in 1:10) p[f == k] <- predict(glm(fml, data = d[f != k, ], family = binomial), newdata = d[f == k, ], type = "response")
  p }
cvp <- replicate(1, cv_pred()); auc_cv <- mean(apply(cvp, 2, function(p) auc(d$landslide, p)))
pbar <- rowMeans(cvp)   # out-of-sample predicted probabilities

cutoff <- mean(d$landslide); yhat <- as.integer(pbar > cutoff)
sens <- mean(yhat[d$landslide == 1] == 1); spec <- mean(yhat[d$landslide == 0] == 0)

#### Table ####
lab <- c("(Intercept)" = "Constant", prec_verao = "Precipitation (summer)", prec_outono = "Precipitation (fall)", prec_inverno = "Precipitation (winter)", prec_primav = "Precipitation (spring)",
         altitude = "Altitude (meters)", temp_verao = "Temperature (summer)", temp_inverno = "Temperature (winter)", temp_primav = "Temperature (spring)", temp_outono = "Temperature (fall)",
         avg_tri = "Terrain ruggedness index", avg_slope = "Average slope (degrees)", regionMidwest = "Region: Midwest", regionNorth = "Region: North", regionSouth = "Region: South", regionSoutheast = "Region: Southeast")
stars <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))
cf <- function(m) { s <- summary(m)$coefficients; data.frame(term = rownames(s), est = s[, 1], se = s[, 2], p = s[, 4]) }
c1 <- cf(m1)
fmt <- function(x, k = 4) sprintf(paste0("%.", k, "f"), x)
num <- function(x) ifelse(x < 0, paste0("$-$", fmt(abs(x), 4)), fmt(x, 4))
rows <- character(0)
for (t in names(lab)) {
  a <- c1[c1$term == t, ]
  rows <- c(rows, sprintf("%s & %s%s \\\\", lab[[t]], num(a$est), stars(a$p)), sprintf(" & (%s) \\\\", fmt(a$se)))
}
pv <- function(p) ifelse(p < 0.01, "$<$0.01", fmt(p, 2))
tex <- c("\\begin{table}[H]", "\\centering", "\\scalebox{0.9}{", "\\begin{threeparttable}",
  "\\caption{Propensity Score Logit and Quality of Prediction}", "\\label{tab:psm_logit}", "\\begin{tabular}{lc}", "\\toprule",
  " & Logit \\\\", " & (before matching) \\\\", "\\midrule", rows, "\\midrule",
  sprintf("Municipalities & %s \\\\", format(n_municipalities, big.mark = ",")),
  sprintf("Municipalities with a landslide & %s \\\\", s1["treated"]),
  sprintf("McFadden pseudo $R^{2}$ & %s \\\\", fmt(s1["pseudo_r2"], 2)),
  sprintf("Likelihood-ratio test of the covariates (p-value) & %s \\\\", pv(s1["lr_p"])),
  sprintf("AUC, out of sample (10-fold cross-validation) & %s \\\\", fmt(auc_cv, 2)),
  sprintf("Municipalities with a landslide predicted to have one (\\%%) & %s \\\\", fmt(100 * sens, 2)),
  sprintf("Municipalities without a landslide predicted not to have one (\\%%) & %s \\\\", fmt(100 * spec, 2)),
  "\\bottomrule", "\\end{tabular}", "\\begin{tablenotes}[flushleft]", "\\footnotesize",
  paste0("\\item \\textit{Notes:} Logit of landslide occurrence (2003--2020) on climate, terrain and region covariates, estimated before the matching; the predicted probability is the propensity score. ",
         "The omitted region is Northeast. One municipality with a missing matching covariate is excluded from estimation. Out-of-sample measures use 10-fold cross-validation. A landslide is predicted for a municipality when its predicted probability exceeds the share of municipalities with a landslide (10.09\\%). ",
         "Standard errors in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.10$."),
  "\\end{tablenotes}", "\\end{threeparttable}", "}", "\\end{table}")
writeLines(tex, paste0(path_output_git, "_table_B1_psm_logit.tex"))
q <- data.frame(measure = c("n", "treated", "McFadden pseudo R2", "LR test p-value", "AUC out of sample (10-fold CV)", "Cutoff (share with a landslide)", "Share of landslide municipalities classified at risk (out of sample)", "Share of municipalities without landslide classified not at risk (out of sample)"),
                value = c(s1["n"], s1["treated"], s1["pseudo_r2"], s1["lr_p"], auc_cv, cutoff, sens, spec))
write.csv(q %>% mutate(value = round(value, 4)), paste0(path_output_git, "psm_logit_quality.csv"), row.names = FALSE)
print(q %>% mutate(value = round(value, 4)), row.names = FALSE); print(round(c1[, 2:4], 4))
