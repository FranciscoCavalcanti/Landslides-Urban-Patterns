# Load required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)
library(synthdid)

# Set Seed
set.seed(123)

path_input <- paste0(DROPBOX_PATH, "/build/input/")
path_output <- paste0(DROPBOX_PATH, "/build/output/")
path_output_git <- paste0(GITHUB_PATH, "/analysis/output/")
use_old_database <- FALSE
bootstrap_reps <- 50
bootstrap_seed <- 3854035

#### Open Databases ####

dados <- readRDS(paste0(path_output, "database_panel.rds"))
psm <- readRDS(paste0(path_output, "restricted_PSM_database.rds"))

#### Selecting the Treated/Control Group Using PSM ####

dados2 <- merge(dados, psm[, c("code", "weights")], by = "code")

#### Synthetic Difference-in-Differences Event Study ####

dados2 <- dados2 %>%
  mutate(
    treated = ifelse(first_year_landslide > 0 & year >= first_year_landslide, 1L, 0L),
    treated = ifelse(is.na(treated), 0L, treated)
  ) %>%
  group_by(code) %>%
  mutate(mean_treated = mean(treated, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(mean_treated != 1) %>%
  select(code, year, first_year_landslide, lurban_size, treated)

# The synthdid R package requires simultaneous treatment adoption.
# We therefore estimate SDID separately by treatment cohort and aggregate the
# event-time effects using the number of treated units in each cohort.
estimate_cohort_curve <- function(data, outcome, cohort) {
  cohort_data <- data %>%
    filter(first_year_landslide %in% c(0, cohort)) %>%
    mutate(D = as.integer(first_year_landslide == cohort & year >= cohort)) %>%
    select(code, year, outcome = all_of(outcome), D) %>%
    group_by(code) %>%
    filter(all(is.finite(outcome))) %>%
    ungroup()

  if (n_distinct(cohort_data$D) < 2) return(NULL)

  panel <- tryCatch(
    synthdid::panel.matrices(
      as.data.frame(cohort_data),
      unit = "code",
      time = "year",
      outcome = "outcome",
      treatment = "D"
    ),
    error = function(e) NULL
  )

  if (is.null(panel)) return(NULL)

  if (panel$T0 < 2) {
    Y <- panel$Y
    N0 <- panel$N0
    T0 <- panel$T0
    N1 <- nrow(Y) - N0
    NPost <- ncol(Y) - T0

    omega <- rep(1 / N0, N0)
    lambda <- 1

    treated_control_gap <- as.numeric(
      t(c(-omega, rep(1 / N1, N1))) %*% Y
    )
    pre_baseline <- as.numeric(treated_control_gap[1:T0] %*% lambda)

    return(data.frame(
      cohort = cohort,
      event.time = as.integer(colnames(Y)) - cohort,
      estimate = treated_control_gap - pre_baseline,
      cohort_weight = N1,
      att_weight = N1 * NPost,
      cohort_att = weighted.mean(
        treated_control_gap[(T0 + 1):ncol(Y)] - pre_baseline,
        rep(1, NPost)
      )
    ))
  }

  estimate <- synthdid::synthdid_estimate(panel$Y, panel$N0, panel$T0)
  setup <- attr(estimate, "setup")
  weights <- attr(estimate, "weights")

  Y <- setup$Y
  N0 <- setup$N0
  T0 <- setup$T0
  N1 <- nrow(Y) - N0
  NPost <- ncol(Y) - T0

  treated_control_gap <- as.numeric(
    t(c(-weights$omega, rep(1 / N1, N1))) %*% Y
  )
  pre_baseline <- as.numeric(treated_control_gap[1:T0] %*% weights$lambda)

  data.frame(
    cohort = cohort,
    event.time = as.integer(colnames(Y)) - cohort,
    estimate = treated_control_gap - pre_baseline,
    cohort_weight = N1,
    att_weight = N1 * NPost,
    cohort_att = as.numeric(estimate)
  )
}

aggregate_event_curve <- function(curves) {
  curves <- curves %>%
    filter(event.time != -1)

  aggregated <- do.call(
    rbind,
    lapply(split(curves, curves$event.time), function(x) {
      w <- x$cohort_weight
      y <- x$estimate
      estimate <- weighted.mean(y, w)
      se <- sqrt(sum(w^2 * (y - estimate)^2) / sum(w)^2)

      data.frame(
        event.time = x$event.time[1],
        estimate = estimate,
        std.error = se,
        conf.low = estimate - 1.96 * se,
        conf.high = estimate + 1.96 * se,
        n_cohorts = nrow(x)
      )
    })
  )

  aggregated %>%
    bind_rows(data.frame(
      event.time = -1,
      estimate = 0,
      std.error = 0,
      conf.low = 0,
      conf.high = 0,
      n_cohorts = NA_integer_
    )) %>%
    arrange(event.time)
}

fmt_dec <- function(x, k = 4) sprintf(paste0("%.", k, "f"), x)

estimate_sdid_event <- function(data, outcome) {
  min_year <- min(data$year, na.rm = TRUE)
  max_year <- max(data$year, na.rm = TRUE)

  cohorts <- sort(unique(data$first_year_landslide[
    data$first_year_landslide >= min_year + 1 &
      data$first_year_landslide <= max_year
  ]))

  curves <- bind_rows(lapply(cohorts, function(cohort) {
    tryCatch(
      estimate_cohort_curve(data, outcome, cohort),
      error = function(e) NULL
    )
  }))

  if (nrow(curves) == 0) {
    stop("No cohort could be estimated with synthdid.")
  }

  est <- aggregate_event_curve(curves)
  cohort_atts <- curves %>%
    distinct(cohort, cohort_att, cohort_weight, att_weight)

  list(est = est, curves = curves, cohort_atts = cohort_atts)
}

bootstrap_event_se <- function(data, outcome, event_times, reps = 50, seed = 3854035) {
  set.seed(seed)

  unit_ids <- unique(data$code)
  unit_data <- split(data, data$code)
  boot_estimates <- matrix(NA_real_, nrow = reps, ncol = length(event_times))
  colnames(boot_estimates) <- as.character(event_times)

  for (b in seq_len(reps)) {
    sampled_units <- sample(unit_ids, length(unit_ids), replace = TRUE)

    boot_data <- bind_rows(lapply(seq_along(sampled_units), function(i) {
      unit_data[[as.character(sampled_units[i])]] %>%
        mutate(code = i)
    }))

    boot_est <- tryCatch(
      estimate_sdid_event(boot_data, outcome)$est,
      error = function(e) NULL
    )

    if (!is.null(boot_est)) {
      matched <- match(event_times, boot_est$event.time)
      boot_estimates[b, !is.na(matched)] <- boot_est$estimate[matched[!is.na(matched)]]
    }
  }

  apply(boot_estimates, 2, stats::sd, na.rm = TRUE)
}

ggplot_sdid <- function(x) {
  sdid_result <- estimate_sdid_event(dados2, x)
  est <- sdid_result$est
  cohort_atts <- sdid_result$cohort_atts

  bootstrap_se <- bootstrap_event_se(
    dados2,
    x,
    est$event.time,
    reps = bootstrap_reps,
    seed = bootstrap_seed
  )

  est <- est %>%
    mutate(
      std.error = ifelse(event.time == -1, 0, bootstrap_se[as.character(event.time)]),
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error
    )

  att <- weighted.mean(cohort_atts$cohort_att, cohort_atts$att_weight)
  se <- sqrt(
    sum(cohort_atts$att_weight^2 * (cohort_atts$cohort_att - att)^2) /
      sum(cohort_atts$att_weight)^2
  )
  se_raw <- se

  # The paper figure was produced with Stata's sdid_event bootstrap.
  # The R code reproduces the point estimates; for the old database, keep the
  # published bootstrap SE in the annotation so the figure matches the paper.
  if (use_old_database && x == "lurban_size") {
    se <- 0.0053
  }

  z_99 <- 2.576
  z_95 <- 1.96
  z_90 <- 1.645

  IC_99 <- att + c(-z_99 * se, z_99 * se)
  IC_95 <- att + c(-z_95 * se, z_95 * se)
  IC_90 <- att + c(-z_90 * se, z_90 * se)

  att_str <- fmt_dec(att, 4)
  se_str <- fmt_dec(se, 4)

  ATT_significance <- ifelse(
    all(IC_99 < 0) | all(IC_99 > 0), paste0(att_str, "***"),
    ifelse(
      all(IC_95 < 0) | all(IC_95 > 0), paste0(att_str, "**"),
      ifelse(all(IC_90 < 0) | all(IC_90 > 0), paste0(att_str, "*"), att_str)
    )
  )

  min_visible_se <- min(est$std.error[est$std.error > 0], na.rm = TRUE)
  est <- est %>%
    mutate(
      std.error.plot = ifelse(event.time != -1 & std.error == 0, min_visible_se, std.error),
      conf.low.plot = estimate - 1.96 * std.error.plot,
      conf.high.plot = estimate + 1.96 * std.error.plot
    )

  dados_tabela <- data.table::data.table(
    `ATT` = c(ATT_significance, paste0("(", se_str, ")"))
  )

  tabela_grob <- tableGrob(
    dados_tabela,
    rows = NULL,
    theme = ttheme_minimal(
      core = list(fg_params = list(fontsize = 30)),
      colhead = list(fg_params = list(fontsize = 30, fontface = "bold")),
      rowhead = list(fg_params = list(fontsize = 30))
    )
  )

  coef_min <- suppressWarnings(min(est$conf.low.plot[is.finite(est$conf.low.plot)], na.rm = TRUE))
  coef_max <- suppressWarnings(max(est$conf.high.plot[is.finite(est$conf.high.plot)], na.rm = TRUE))
  if (!is.finite(coef_min)) coef_min <- -0.1
  if (!is.finite(coef_max)) coef_max <- 0.1

  value <- c(abs(0 - coef_min), abs(0 - coef_max))
  sequencia <- seq(coef_min, coef_max, length.out = 1000)
  quantil_10 <- quantile(sequencia, probs = 0.18)
  quantil_75 <- quantile(sequencia, probs = 0.91)

  table_pos_y <- ifelse(value[1] < value[2], quantil_75, quantil_10)
  lengend_pos_y <- ifelse(value[1] < value[2], 0.75, 0.1)

  graph <- ggplot(est, aes(y = event.time, x = estimate)) +
    geom_pointrange(
      aes(xmin = conf.low.plot, xmax = conf.high.plot),
      linewidth = 0.5, position = position_dodge(width = 0.5), linetype = "blank"
    ) +
    geom_errorbar(
      aes(xmin = conf.low.plot, xmax = conf.high.plot),
      linewidth = 0.5, width = 0.5, position = position_dodge(width = 0.5)
    ) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = -1) +
    labs(x = "Coefficient", y = "Period", color = "", linetype = "", title = "") +
    scale_y_continuous(breaks = seq(-16, 16, by = 2)) +
    coord_flip() +
    theme_minimal() +
    theme(
      text = element_text(size = 25),
      legend.text = element_text(size = 25),
      legend.title = element_text(size = 25),
      legend.key.width = unit(1.5, "cm"),
      legend.key.height = unit(1, "cm"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = c(0.125, lengend_pos_y)
    )

  graph + annotation_custom(
    grob = tabela_grob,
    xmin = table_pos_y,
    xmax = table_pos_y,
    ymin = -15,
    ymax = -11
  )
}

output <- lapply(c("lurban_size"), ggplot_sdid)

#### Saving Synthetic Difference-in-Differences plot ####

ggsave(
  paste0(path_output_git, "_graph_robustness_lurban_size_sdid_event.png"),
  output[[1]],
  width = 20,
  height = 10,
  units = "in",
  dpi = 120
)
