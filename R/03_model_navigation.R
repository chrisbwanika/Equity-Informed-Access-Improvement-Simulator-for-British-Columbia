# R/03_model_navigation.R ------------------------------------------------------
# The decision problem: targeted or universal patient navigation versus usual
# care for adults referred into a diagnostic/specialist pathway, stratified by
# structural-barrier group. This file turns parameters into transition arrays and
# state values, then calls the generic engine in R/02_engine.R.
#
# States (quarterly cycles):
#   unresolved : referred, pathway not yet completed (condition unmanaged)
#   managed    : pathway completed, condition under appropriate management
#   acute      : acute event this quarter (a one-cycle "tunnel" state)
#   dead       : absorbing
#
# Transitions are built as SEQUENTIAL CONDITIONAL probabilities, e.g. from
# unresolved: first background death, then (if alive) an acute event, then (if
# no event) pathway completion. Every row therefore sums to 1 by construction and
# no probability can go negative - the class of error that broke the v1 guides.

STATES <- c("unresolved", "managed", "acute", "dead")
OUTCOMES <- c("cost", "qaly", "ly", "acute_events", "time_unresolved", "managed_1y")

n_model_cycles <- function(pars) {
  n <- gp(pars, "horizon_years") / gp(pars, "cycle_length")
  if (abs(n - round(n)) > 1e-9) stop("horizon_years must be a multiple of cycle_length.")
  as.integer(round(n))
}

# Apply a hazard ratio to a per-cycle probability under proportional hazards:
# p = 1 - exp(-rate)  =>  p' = 1 - exp(-hr * rate) = 1 - (1 - p)^hr.
# The result always stays inside (0, 1), unlike multiplying p by a ratio.
scale_prob_by_hr <- function(p, hr) 1 - (1 - p)^hr

background_death_prob <- function(pars, group, n_cycles) {
  cl <- gp(pars, "cycle_length")
  age <- gp(pars, "start_age") + (seq_len(n_cycles) - 1) * cl   # age at start of each cycle
  annual_hazard <- gp(pars, "gompertz_a") * exp(gp(pars, "gompertz_b") * age) *
    gp(pars, "hr_bg_mortality", group)
  1 - exp(-annual_hazard * cl)
}

build_transition_array <- function(pars, group, navigated = FALSE) {
  n_cycles <- n_model_cycles(pars)
  q_bg  <- background_death_prob(pars, group, n_cycles)             # vector, one per cycle
  q_m   <- rep(gp(pars, "p_complete", group), n_cycles)
  if (navigated) {
    active <- seq_len(n_cycles) <= gp(pars, "nav_cycles")
    q_m[active] <- scale_prob_by_hr(q_m[active], gp(pars, "hr_navigation", group))
  }
  q_aU  <- gp(pars, "p_acute_unresolved", group)
  q_aM  <- scale_prob_by_hr(q_aU, gp(pars, "hr_acute_managed"))
  q_dis <- gp(pars, "p_disengage", group)
  q_cf  <- gp(pars, "p_case_fatality")
  q_pm  <- gp(pars, "p_post_acute_managed")

  P <- array(0, dim = c(4, 4, n_cycles), dimnames = list(from = STATES, to = STATES, cycle = NULL))
  # From unresolved
  P["unresolved", "dead", ]       <- q_bg
  P["unresolved", "acute", ]      <- (1 - q_bg) * q_aU
  P["unresolved", "managed", ]    <- (1 - q_bg) * (1 - q_aU) * q_m
  P["unresolved", "unresolved", ] <- (1 - q_bg) * (1 - q_aU) * (1 - q_m)
  # From managed
  P["managed", "dead", ]          <- q_bg
  P["managed", "acute", ]         <- (1 - q_bg) * q_aM
  P["managed", "unresolved", ]    <- (1 - q_bg) * (1 - q_aM) * q_dis
  P["managed", "managed", ]       <- (1 - q_bg) * (1 - q_aM) * (1 - q_dis)
  # From acute (tunnel: nobody stays in "acute" for a second cycle)
  p_die_acute                     <- 1 - (1 - q_bg) * (1 - q_cf)
  P["acute", "dead", ]            <- p_die_acute
  P["acute", "managed", ]         <- (1 - p_die_acute) * q_pm
  P["acute", "unresolved", ]      <- (1 - p_die_acute) * (1 - q_pm)
  # Dead is absorbing
  P["dead", "dead", ]             <- 1
  P
}

state_values <- function(pars, group) {
  cl <- gp(pars, "cycle_length")
  u_m <- gp(pars, "u_managed")
  utility <- c(unresolved = u_m - gp(pars, "dec_unresolved"),
               managed    = u_m,
               acute      = u_m - gp(pars, "dec_acute"),
               dead       = 0)
  if (any(utility < 0 | utility > 1)) {
    stop("A utility fell outside [0, 1]; check u_managed and the decrements.", call. = FALSE)
  }
  list(
    cost = c(unresolved = gp(pars, "c_unresolved"), managed = gp(pars, "c_managed"),
             acute = gp(pars, "c_acute"), dead = 0),
    qaly = utility * cl,
    ly   = c(unresolved = 1, managed = 1, acute = 1, dead = 0) * cl
  )
}

run_group_arm <- function(pars, group, navigated = FALSE, wcc = SETTINGS$wcc_method) {
  P <- build_transition_array(pars, group, navigated)
  trace <- run_cohort(P, start = c(unresolved = 1, managed = 0, acute = 0, dead = 0))
  v <- state_values(pars, group)
  cl <- gp(pars, "cycle_length"); r <- gp(pars, "discount_rate")
  nav_cost <- if (navigated) gp(pars, "c_navigation") else 0  # paid once at time 0
  one_year <- round(1 / cl)
  list(
    trace = trace,
    summary = c(
      cost            = accumulate(trace, v$cost, cl, r, wcc) + nav_cost,
      qaly            = accumulate(trace, v$qaly, cl, r, wcc),
      ly              = accumulate(trace, v$ly,   cl, r, wcc),
      acute_events    = sum(trace[-1, "acute"]),          # each boundary in 'acute' = one event
      time_unresolved = accumulate(trace, c(cl, 0, 0, 0), cl, 0, wcc),  # years, undiscounted
      managed_1y      = trace[one_year + 1, "managed"]
    )
  )
}

# Per-patient results for every group under usual care and under navigation.
evaluate_group_arms <- function(pars, wcc = SETTINGS$wcc_method) {
  out <- lapply(GROUPS, function(g) {
    rbind(usual = run_group_arm(pars, g, FALSE, wcc)$summary,
          navigated = run_group_arm(pars, g, TRUE, wcc)$summary)
  })
  names(out) <- GROUPS
  out
}

# Assemble strategies. In an offered group, a share `uptake` is navigated and the
# rest follow usual care; running the two sub-cohorts separately and weighting
# them is exact (mixing probabilities inside one cohort would not be).
evaluate_strategies <- function(pars, strategies, groups_df, wcc = SETTINGS$wcc_method,
                                arms = NULL) {
  if (is.null(arms)) arms <- evaluate_group_arms(pars, wcc)
  rows <- list()
  for (s in seq_len(nrow(strategies))) {
    for (g in GROUPS) {
      res <- arms[[g]]["usual", ]
      if (strategies[s, g] == 1) {
        u <- gp(pars, "uptake", g)
        res <- u * arms[[g]]["navigated", ] + (1 - u) * arms[[g]]["usual", ]
      }
      rows[[length(rows) + 1]] <- data.frame(strategy = strategies$strategy[s], group = g,
                                             t(res), check.names = FALSE)
    }
  }
  group_results <- do.call(rbind, rows)
  group_results$cohort_share <- groups_df$cohort_share[match(group_results$group, groups_df$group)]
  strategy_results <- do.call(rbind, lapply(strategies$strategy, function(s) {
    d <- group_results[group_results$strategy == s, ]
    data.frame(strategy = s, t(colSums(d[OUTCOMES] * d$cohort_share)), check.names = FALSE)
  }))
  list(group = group_results, strategy = strategy_results)
}

# ---- Decision rules ------------------------------------------------------------
# Full incremental analysis: remove strongly dominated strategies, then
# extendedly dominated ones, then compute ICERs along the efficiency frontier.
incremental_analysis <- function(strategy, cost, effect) {
  d <- data.frame(strategy = strategy, cost = cost, effect = effect,
                  status = "on frontier", stringsAsFactors = FALSE)
  d <- d[order(d$cost, -d$effect), ]
  for (i in seq_len(nrow(d))) {
    dominated_by <- d$cost[-i] <= d$cost[i] & d$effect[-i] >= d$effect[i] &
      (d$cost[-i] < d$cost[i] | d$effect[-i] > d$effect[i])
    if (any(dominated_by)) d$status[i] <- "dominated"
  }
  repeat {
    f <- which(d$status == "on frontier")
    if (length(f) < 3) break
    icer <- diff(d$cost[f]) / diff(d$effect[f])
    ext <- which(icer[-length(icer)] > icer[-1])
    if (length(ext) == 0) break
    d$status[f[ext[1] + 1]] <- "extendedly dominated"
  }
  f <- which(d$status == "on frontier")
  d$inc_cost <- NA_real_; d$inc_effect <- NA_real_; d$icer <- NA_real_
  if (length(f) > 1) {
    d$inc_cost[f[-1]] <- diff(d$cost[f])
    d$inc_effect[f[-1]] <- diff(d$effect[f])
    d$icer[f[-1]] <- d$inc_cost[f[-1]] / d$inc_effect[f[-1]]
  }
  rownames(d) <- NULL
  d
}

net_monetary_benefit <- function(qaly, cost, k) qaly * k - cost
