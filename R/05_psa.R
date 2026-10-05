# R/05_psa.R -------------------------------------------------------------------
# Probabilistic sensitivity analysis (the CDA-AMC reference case is probabilistic).
# One parameter draw is shared by ALL strategies and ALL groups within an
# iteration (common random numbers), so incremental results reflect parameter
# uncertainty rather than simulation noise. Every draw is saved for audit.

run_psa <- function(par_df, strategies, groups_df, n = SETTINGS$n_psa,
                    seed = module_seed("psa"), wcc = SETTINGS$wcc_method) {
  draws <- draw_psa(par_df, n, seed)
  strat_rows <- vector("list", n); group_rows <- vector("list", n)
  for (i in seq_len(n)) {
    res <- evaluate_strategies(draws[i, ], strategies, groups_df, wcc)
    strat_rows[[i]] <- cbind(iteration = i, res$strategy)
    group_rows[[i]] <- cbind(iteration = i, res$group[, c("strategy", "group", "cost", "qaly")])
  }
  list(draws = cbind(iteration = seq_len(n), as.data.frame(draws, check.names = FALSE)),
       strategy = do.call(rbind, strat_rows),
       group = do.call(rbind, group_rows))
}

# Matrix of net monetary benefit: rows = iterations, columns = strategies.
nmb_matrix <- function(psa_strategy, k, strategy_order) {
  sapply(strategy_order, function(s) {
    d <- psa_strategy[psa_strategy$strategy == s, ]
    d <- d[order(d$iteration), ]
    net_monetary_benefit(d$qaly, d$cost, k)
  })
}

summarise_psa <- function(psa_strategy, strategy_order) {
  base <- psa_strategy[psa_strategy$strategy == "usual_care", ]
  base <- base[order(base$iteration), ]
  do.call(rbind, lapply(strategy_order, function(s) {
    d <- psa_strategy[psa_strategy$strategy == s, ]; d <- d[order(d$iteration), ]
    q <- function(x) stats::quantile(x, c(0.025, 0.975), names = FALSE)
    dc <- d$cost - base$cost; dq <- d$qaly - base$qaly
    data.frame(strategy = s,
               mean_cost = mean(d$cost), cost_lo = q(d$cost)[1], cost_hi = q(d$cost)[2],
               mean_qaly = mean(d$qaly), qaly_lo = q(d$qaly)[1], qaly_hi = q(d$qaly)[2],
               mean_inc_cost = mean(dc), inc_cost_lo = q(dc)[1], inc_cost_hi = q(dc)[2],
               mean_inc_qaly = mean(dq), inc_qaly_lo = q(dq)[1], inc_qaly_hi = q(dq)[2])
  }))
}

# CEAC: probability each strategy has the highest NMB at each threshold.
# CEAF: the strategy with the highest EXPECTED NMB at each threshold.
# EVPI per patient: E[max NMB] - max E[NMB].
ceac_evpi <- function(psa_strategy, strategy_order, wtp_grid = SETTINGS$wtp_grid) {
  do.call(rbind, lapply(wtp_grid, function(k) {
    m <- nmb_matrix(psa_strategy, k, strategy_order)
    best <- apply(m, 1, which.max)
    exp_nmb <- colMeans(m)
    data.frame(k = k, strategy = strategy_order,
               prob_optimal = tabulate(best, nbins = length(strategy_order)) / nrow(m),
               expected_nmb = exp_nmb,
               on_frontier = seq_along(strategy_order) == which.max(exp_nmb),
               evpi_per_patient = mean(apply(m, 1, max)) - max(exp_nmb))
  }))
}

psa_convergence <- function(psa_strategy, strategy_order, k) {
  m <- nmb_matrix(psa_strategy, k, strategy_order)
  inc <- m[, -1, drop = FALSE] - m[, 1]
  do.call(rbind, lapply(colnames(inc), function(s) {
    data.frame(iteration = seq_len(nrow(inc)), strategy = s,
               running_mean_inmb = cumsum(inc[, s]) / seq_len(nrow(inc)))
  }))
}
