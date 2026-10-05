# R/04_deterministic.R ---------------------------------------------------------
# Deterministic analyses: base case, one-way sensitivity analysis (OWSA) and
# scenario analysis. Built and checked BEFORE the PSA: the PSA reuses the same
# model functions, so an error here would propagate everywhere.

run_base_case <- function(pars, strategies, groups_df, wcc = SETTINGS$wcc_method) {
  res <- evaluate_strategies(pars, strategies, groups_df, wcc)
  k <- gp(pars, "k_threshold")
  s <- res$strategy
  s$nmb <- net_monetary_benefit(s$qaly, s$cost, k)
  s$nhb <- s$qaly - s$cost / k
  s$optimal_at_k <- s$nmb == max(s$nmb)
  ia <- incremental_analysis(s$strategy, s$cost, s$qaly)
  vs_uc <- do.call(rbind, lapply(strategies$strategy[-1], function(x) {
    a <- s[s$strategy == x, ]; b <- s[s$strategy == "usual_care", ]
    data.frame(strategy = x, inc_cost = a$cost - b$cost, inc_qaly = a$qaly - b$qaly,
               inc_ly = a$ly - b$ly, inc_acute_events = a$acute_events - b$acute_events,
               inc_nmb = a$nmb - b$nmb, icer_vs_usual_care = (a$cost - b$cost) / (a$qaly - b$qaly))
  }))
  list(group = res$group, strategy = s, incremental = ia, vs_usual_care = vs_uc)
}

# OWSA: each uncertain parameter is set to the 2.5th and 97.5th percentiles of
# ITS OWN PSA distribution, so the ranges are reproducible rather than ad hoc.
run_owsa <- function(par_df, pars, strategies, groups_df, wcc = SETTINGS$wcc_method,
                     probs = c(0.025, 0.975)) {
  k <- gp(pars, "k_threshold")
  inmb <- function(p) {
    s <- evaluate_strategies(p, strategies, groups_df, wcc)$strategy
    nmb <- net_monetary_benefit(s$qaly, s$cost, k)
    stats::setNames(nmb[-1] - nmb[1], s$strategy[-1])
  }
  base_inmb <- inmb(pars)
  unc <- uncertain_parameters(par_df)
  rows <- lapply(seq_len(nrow(unc)), function(i) {
    q <- quantile_parameter(probs, unc$distribution[i], unc$value[i], unc$se[i])
    lo <- pars; lo[unc$key[i]] <- q[1]
    hi <- pars; hi[unc$key[i]] <- q[2]
    r_lo <- inmb(lo); r_hi <- inmb(hi)
    data.frame(parameter = unc$key[i], strategy = names(base_inmb),
               low_value = q[1], high_value = q[2],
               inmb_low = r_lo, inmb_high = r_hi, inmb_base = base_inmb,
               swing = abs(r_hi - r_lo))
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[order(out$strategy, -out$swing), ]
}

# Scenarios: each row changes one or more inputs; everything else stays at base.
default_scenarios <- function() {
  list(
    list(name = "Base case", changes = c()),
    list(name = "Discount rate 0%", changes = c(discount_rate = 0)),
    list(name = "Discount rate 3%", changes = c(discount_rate = 0.03)),
    list(name = "10-year horizon", changes = c(horizon_years = 10)),
    list(name = "No within-cycle correction", changes = c(), wcc = "none"),
    list(name = "Navigation lasts 2 years", changes = c(nav_cycles = 8)),
    list(name = "Equal uptake (0.80) in all groups",
         changes = c(`uptake@moderate_barrier` = 0.80, `uptake@higher_barrier` = 0.80)),
    list(name = "Equal navigation effect (HR 1.35) in all groups",
         changes = c(`hr_navigation@lower_barrier` = 1.35, `hr_navigation@higher_barrier` = 1.35)),
    list(name = "Navigation cost +50%", changes = c(c_navigation = 1125)),
    list(name = "No mortality gradient between groups",
         changes = c(`hr_bg_mortality@moderate_barrier` = 1, `hr_bg_mortality@higher_barrier` = 1)),
    list(name = "Threshold CAD 30,000 per QALY", changes = c(k_threshold = 30000)),
    list(name = "Threshold CAD 100,000 per QALY", changes = c(k_threshold = 100000))
  )
}

run_scenarios <- function(pars, strategies, groups_df, scenarios = default_scenarios()) {
  rows <- lapply(scenarios, function(sc) {
    p <- pars
    if (length(sc$changes) > 0) {
      if (!all(names(sc$changes) %in% names(p))) stop("Unknown key in scenario: ", sc$name)
      p[names(sc$changes)] <- sc$changes
    }
    wcc <- if (is.null(sc$wcc)) SETTINGS$wcc_method else sc$wcc
    b <- run_base_case(p, strategies, groups_df, wcc)
    v <- b$vs_usual_care
    data.frame(scenario = sc$name, strategy = v$strategy, inc_cost = v$inc_cost,
               inc_qaly = v$inc_qaly, inc_nmb = v$inc_nmb,
               icer_vs_usual_care = v$icer_vs_usual_care,
               optimal_strategy = b$strategy$strategy[which.max(b$strategy$nmb)])
  })
  do.call(rbind, rows)
}
