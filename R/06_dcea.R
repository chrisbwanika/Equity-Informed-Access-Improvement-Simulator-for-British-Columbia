# R/06_dcea.R ------------------------------------------------------------------
# Aggregate distributional cost-effectiveness analysis (after Asaria, Griffin &
# Cookson 2016). Steps, for each strategy compared with usual care:
#   1. Baseline distribution of health: quality-adjusted life expectancy (QALE)
#      by group in the whole catchment population (data/groups.csv).
#   2. Health gains by group: incremental QALYs per patient x patients per group.
#   3. Health opportunity costs: incremental cost / k is the health displaced
#      elsewhere in the system; it is shared across groups by an EXPLICIT rule
#      (an oc_* column in groups.csv), varied in scenario analysis.
#   4. Post-intervention distribution: baseline QALE + (gain - opportunity cost)
#      per person in each group.
#   5. Compare distributions with inequality measures and social welfare
#      functions (Atkinson and Kolm equally distributed equivalent health, EDE),
#      reported across a RANGE of inequality aversion, never a single value.

ede_atkinson <- function(h, w, epsilon) {
  if (any(!is.finite(h)) || any(h <= 0)) {
    stop("Atkinson EDE is only defined for strictly positive health values.", call. = FALSE)
  }
  if (epsilon < 0) stop("epsilon must be >= 0.")
  w <- w / sum(w)
  if (abs(epsilon - 1) < 1e-12) return(exp(sum(w * log(h))))
  sum(w * h^(1 - epsilon))^(1 / (1 - epsilon))
}

ede_kolm <- function(h, w, alpha) {
  if (alpha < 0) stop("alpha must be >= 0.")
  w <- w / sum(w)
  if (alpha == 0) return(sum(w * h))
  m <- min(h)                                  # log-sum-exp form for numerical stability
  m - log(sum(w * exp(-alpha * (h - m)))) / alpha
}

dcea_distribution <- function(inc_qaly_pp, inc_cost_pp, groups_df, n_cohort, n_pop, k,
                              oc_col = SETTINGS$oc_base_scenario) {
  if (!oc_col %in% names(groups_df)) stop("Unknown opportunity-cost column: ", oc_col)
  g <- groups_df$group
  patients <- n_cohort * groups_df$cohort_share
  gain <- inc_qaly_pp[g] * patients
  total_inc_cost <- sum(inc_cost_pp[g] * patients)
  opportunity_cost <- (total_inc_cost / k) * groups_df[[oc_col]]
  net <- gain - opportunity_cost
  population <- n_pop * groups_df$pop_share
  data.frame(group = g, patients = patients, health_gain = unname(gain),
             opportunity_cost = unname(opportunity_cost), net_health = unname(net),
             population = population, change_per_capita = unname(net / population),
             baseline_qale = groups_df$baseline_qale,
             post_qale = groups_df$baseline_qale + unname(net / population))
}

dcea_metrics <- function(dist, n_pop, epsilon_grid = SETTINGS$epsilon_grid,
                         alpha_grid = SETTINGS$alpha_grid) {
  w <- dist$population / sum(dist$population)
  lo <- dist$group == "lower_barrier"; hi <- dist$group == "higher_barrier"
  gap_base <- dist$baseline_qale[lo] - dist$baseline_qale[hi]
  gap_post <- dist$post_qale[lo] - dist$post_qale[hi]
  out <- data.frame(total_nhb = sum(dist$net_health),
                    total_health_gain = sum(dist$health_gain),
                    total_opportunity_cost = sum(dist$opportunity_cost),
                    gap_base = gap_base, gap_post = gap_post,
                    gap_reduction = gap_base - gap_post,
                    gap_reduction_days = (gap_base - gap_post) * 365.25)
  for (e in epsilon_grid) {
    d <- ede_atkinson(dist$post_qale, w, e) - ede_atkinson(dist$baseline_qale, w, e)
    out[[paste0("ew_nhb_atkinson_", e)]] <- d * n_pop   # equity-weighted NHB (EDE-equivalent QALYs)
  }
  for (a in alpha_grid) {
    d <- ede_kolm(dist$post_qale, w, a) - ede_kolm(dist$baseline_qale, w, a)
    out[[paste0("ew_nhb_kolm_", a)]] <- d * n_pop
  }
  out
}

# Per-patient incremental QALYs and costs by group, for one strategy vs usual care.
group_increments <- function(group_results, strategy) {
  a <- group_results[group_results$strategy == strategy, ]
  b <- group_results[group_results$strategy == "usual_care", ]
  a <- a[match(GROUPS, a$group), ]; b <- b[match(GROUPS, b$group), ]
  list(qaly = stats::setNames(a$qaly - b$qaly, GROUPS),
       cost = stats::setNames(a$cost - b$cost, GROUPS))
}

run_dcea_deterministic <- function(group_results, groups_df, pars, strategies,
                                   k = gp(pars, "k_threshold")) {
  oc_cols <- grep("^oc_", names(groups_df), value = TRUE)
  dists <- list(); mets <- list()
  for (s in strategies$strategy[-1]) {
    inc <- group_increments(group_results, s)
    for (oc in oc_cols) {
      d <- dcea_distribution(inc$qaly, inc$cost, groups_df, gp(pars, "n_cohort"),
                             gp(pars, "n_population"), k, oc)
      dists[[length(dists) + 1]] <- cbind(strategy = s, oc_scenario = oc, k = k, d)
      mets[[length(mets) + 1]] <- cbind(strategy = s, oc_scenario = oc, k = k,
                                        dcea_metrics(d, gp(pars, "n_population")))
    }
  }
  list(distribution = do.call(rbind, dists), metrics = do.call(rbind, mets))
}

run_dcea_psa <- function(psa, groups_df, strategies, par_df,
                         oc_col = SETTINGS$oc_base_scenario) {
  draws <- psa$draws
  rows <- list()
  for (i in unique(psa$group$iteration)) {
    gr <- psa$group[psa$group$iteration == i, ]
    k <- draws$k_threshold[draws$iteration == i]
    for (s in strategies$strategy[-1]) {
      inc <- group_increments(gr, s)
      d <- dcea_distribution(inc$qaly, inc$cost, groups_df,
                             draws$n_cohort[draws$iteration == i],
                             draws$n_population[draws$iteration == i], k, oc_col)
      rows[[length(rows) + 1]] <- cbind(iteration = i, strategy = s,
                                        dcea_metrics(d, draws$n_population[draws$iteration == i]))
    }
  }
  do.call(rbind, rows)
}

summarise_dcea_psa <- function(dcea_psa) {
  ew_cols <- grep("^ew_nhb_", names(dcea_psa), value = TRUE)
  do.call(rbind, lapply(unique(dcea_psa$strategy), function(s) {
    d <- dcea_psa[dcea_psa$strategy == s, ]
    base <- data.frame(strategy = s,
                       prob_health_gain = mean(d$total_nhb > 0),
                       prob_inequality_reduced = mean(d$gap_reduction > 0),
                       prob_win_win = mean(d$total_nhb > 0 & d$gap_reduction > 0),
                       mean_total_nhb = mean(d$total_nhb),
                       mean_gap_reduction_days = mean(d$gap_reduction_days))
    for (col in ew_cols) base[[paste0("prob_positive_", col)]] <- mean(d[[col]] > 0)
    base
  }))
}

# Equity-efficiency trade-off between two strategies across inequality aversion.
dcea_tradeoff <- function(group_results, groups_df, pars, k_values, a = "targeted_navigation",
                          b = "universal_navigation", epsilon_grid = seq(0, 20, by = 0.5)) {
  do.call(rbind, lapply(k_values, function(k) {
    dist <- lapply(c(a, b), function(s) {
      inc <- group_increments(group_results, s)
      dcea_distribution(inc$qaly, inc$cost, groups_df, gp(pars, "n_cohort"),
                        gp(pars, "n_population"), k)
    })
    w <- groups_df$pop_share
    do.call(rbind, lapply(epsilon_grid, function(e) {
      val <- sapply(dist, function(d) (ede_atkinson(d$post_qale, w, e) -
                                         ede_atkinson(d$baseline_qale, w, e)) * gp(pars, "n_population"))
      data.frame(k = k, epsilon = e, ew_nhb_a = val[1], ew_nhb_b = val[2],
                 a_minus_b = val[1] - val[2], preferred = if (val[1] >= val[2]) a else b)
    }))
  }))
}
