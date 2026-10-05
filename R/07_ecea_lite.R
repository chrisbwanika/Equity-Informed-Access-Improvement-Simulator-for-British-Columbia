# R/07_ecea_lite.R -------------------------------------------------------------
# ECEA-lite: household NON-MEDICAL financial burden in the first year after
# referral (travel, time off work, missed appointments, lost income during acute
# events). It is not a full extended cost-effectiveness analysis and it does not
# measure catastrophic health expenditure (medical care is publicly insured in BC).
#
# Why a microsimulation? A cohort model gives averages, but financial risk is
# about how many households cross a burden threshold, which needs a
# distribution. Individuals are simulated through the SAME transition arrays as
# the cohort model, and their mean cost is checked against the cohort's expected
# cost (a built-in cross-validation, also run as a test).

household_values <- function(pars, group) {
  c(unresolved = gp(pars, "hh_cost_unresolved", group),
    managed    = gp(pars, "hh_cost_managed", group),
    acute      = gp(pars, "hh_cost_acute", group),
    dead       = 0)
}

# Common random numbers: the same uniforms and incomes are used for usual care and
# for navigation, so differences between strategies are not simulation noise.
ecea_random_inputs <- function(n, seed, n_quarters = 4) {
  set.seed(seed)
  list(u_transition = matrix(stats::runif(n * n_quarters), nrow = n),
       u_uptake = stats::runif(n),
       z_income = stats::rnorm(n))
}

simulate_year1_burden <- function(pars, group, offered, rnd, n_quarters = 4) {
  n <- nrow(rnd$u_transition)
  P <- list(usual = build_transition_array(pars, group, FALSE),
            navigated = build_transition_array(pars, group, TRUE))
  navigated <- offered & (rnd$u_uptake < gp(pars, "uptake", group))
  hh <- household_values(pars, group)
  state <- rep(1L, n)                                   # everyone starts unresolved
  cost <- numeric(n)
  for (t in seq_len(n_quarters)) {
    new_state <- state
    for (arm in c("usual", "navigated")) {
      in_arm <- if (arm == "navigated") navigated else !navigated
      for (s in 1:4) {
        who <- which(in_arm & state == s)
        if (length(who) == 0) next
        cum <- cumsum(P[[arm]][s, , t])
        new_state[who] <- pmin(findInterval(rnd$u_transition[who, t], cum) + 1L, 4L)
      }
    }
    state <- new_state
    cost <- cost + hh[state]                            # cost of the state occupied this quarter
  }
  income <- exp(log(gp(pars, "hh_income_median", group)) +
                  gp(pars, "hh_income_sdlog") * rnd$z_income)
  data.frame(group = group, navigated = navigated, hh_cost = unname(cost),
             income = income, burden_share = unname(cost) / income)
}

# Expected first-year household cost from the cohort model (same convention:
# each quarter is charged for the state occupied at its end, undiscounted).
cohort_year1_household_cost <- function(pars, group, navigated, n_quarters = 4) {
  trace <- run_cohort(build_transition_array(pars, group, navigated),
                      start = c(1, 0, 0, 0))
  sum(trace[2:(n_quarters + 1), ] %*% household_values(pars, group))
}

run_ecea_lite <- function(pars, strategies, n = SETTINGS$n_ecea_micro,
                          seed = module_seed("ecea"), thresholds = c(0.025, 0.05, 0.10)) {
  rows <- list(); checks <- list()
  for (gi in seq_along(GROUPS)) {
    g <- GROUPS[gi]
    rnd <- ecea_random_inputs(n, seed + gi)
    sims <- list(not_offered = simulate_year1_burden(pars, g, FALSE, rnd),
                 offered = simulate_year1_burden(pars, g, TRUE, rnd))
    for (s in seq_len(nrow(strategies))) {
      sim <- if (strategies[s, g] == 1) sims$offered else sims$not_offered
      r <- data.frame(strategy = strategies$strategy[s], group = g,
                      mean_hh_cost = mean(sim$hh_cost), mean_burden_share = mean(sim$burden_share))
      for (th in thresholds) r[[paste0("share_above_", th)]] <- mean(sim$burden_share > th)
      rows[[length(rows) + 1]] <- r
    }
    u <- gp(pars, "uptake", g)
    expected_offered <- u * cohort_year1_household_cost(pars, g, TRUE) +
      (1 - u) * cohort_year1_household_cost(pars, g, FALSE)
    checks[[gi]] <- data.frame(
      group = g,
      cohort_expected_not_offered = cohort_year1_household_cost(pars, g, FALSE),
      micro_mean_not_offered = mean(sims$not_offered$hh_cost),
      micro_se_not_offered = stats::sd(sims$not_offered$hh_cost) / sqrt(n),
      cohort_expected_offered = expected_offered,
      micro_mean_offered = mean(sims$offered$hh_cost),
      micro_se_offered = stats::sd(sims$offered$hh_cost) / sqrt(n))
  }
  results <- do.call(rbind, rows)
  uc <- results[results$strategy == "usual_care", ]
  results$hh_cost_averted <- uc$mean_hh_cost[match(results$group, uc$group)] - results$mean_hh_cost
  key <- paste0("share_above_", gp(pars, "burden_threshold"))
  results$high_burden_averted_per_1000 <-
    (uc[[key]][match(results$group, uc$group)] - results[[key]]) * 1000
  list(results = results, cross_check = do.call(rbind, checks))
}
