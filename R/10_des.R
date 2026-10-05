# R/10_des.R -------------------------------------------------------------------
# Discrete event simulation (DES) of a capacity-constrained diagnostic clinic,
# built with simmer. It answers a question the cohort model cannot: when missed
# appointments waste scarce slots, does navigation shorten waits for EVERYONE,
# including patients who are not navigated (a system-level spillover)?
#
# Process: a referral joins a first-come-first-served queue for one of
# `des_capacity` slots; a booked slot is occupied for `des_appt_duration` days
# whether or not the patient attends; after a no-show the patient waits
# `des_rebook_delay` days and re-joins the back of the queue.
#
# Variance reduction: every random input (arrival times, groups, uptake, and the
# uniform number that decides each appointment's attendance) is generated BEFORE
# the simulation and shared by all strategies within a replication (common
# random numbers). simmer itself draws no random numbers here, so each run is
# deterministic given its inputs, and strategy differences are not noise.

des_inputs <- function(pars, groups_df, seed) {
  set.seed(seed)
  horizon <- gp(pars, "des_horizon_days"); rate <- gp(pars, "des_arrival_rate")
  arrivals <- cumsum(stats::rexp(ceiling(rate * horizon * 1.5), rate = rate))
  arrivals <- arrivals[arrivals < horizon]
  n_backlog <- gp(pars, "des_backlog")
  n <- n_backlog + length(arrivals)
  list(n = n,
       time = c(rep(0, n_backlog), arrivals),
       backlog = c(rep(TRUE, n_backlog), rep(FALSE, length(arrivals))),
       group = sample.int(length(GROUPS), n, replace = TRUE, prob = groups_df$cohort_share),
       u_uptake = stats::runif(n),
       u_noshow = matrix(stats::runif(n * gp(pars, "des_max_attempts")), nrow = n))
}

run_des_once <- function(pars, inputs, offered_by_group) {
  p_noshow <- sapply(GROUPS, function(g) gp(pars, "des_p_noshow", g))
  uptake <- sapply(GROUPS, function(g) gp(pars, "uptake", g))
  navigated <- offered_by_group[inputs$group] == 1 & inputs$u_uptake < uptake[inputs$group]
  p_patient <- p_noshow[inputs$group] * ifelse(navigated, gp(pars, "des_rr_noshow_nav"), 1)
  max_att <- gp(pars, "des_max_attempts")

  env <- simmer::simmer("diagnostic_clinic")
  no_show <- function() {
    pid <- simmer::get_attribute(env, "pid")
    a <- simmer::get_attribute(env, "attempt")
    if (a >= max_att) return(0)                # safety cap (probability is negligible)
    as.numeric(inputs$u_noshow[pid, a] < p_patient[pid])
  }
  patient <- simmer::trajectory("referral")
  patient <- simmer::seize(patient, "slot", 1, tag = "book")
  patient <- simmer::set_attribute(patient, "attempt", 1, mod = "+", init = 0)
  patient <- simmer::timeout(patient, gp(pars, "des_appt_duration"))
  patient <- simmer::release(patient, "slot", 1)
  patient <- simmer::set_attribute(patient, "no_show", no_show)
  patient <- simmer::timeout(patient, function()
    simmer::get_attribute(env, "no_show") * gp(pars, "des_rebook_delay"))
  patient <- simmer::rollback(patient, target = "book",
                              check = function() simmer::get_attribute(env, "no_show") == 1)

  env <- simmer::add_resource(env, "slot", capacity = gp(pars, "des_capacity"), queue_size = Inf)
  env <- simmer::add_dataframe(env, "patient", patient,
                               data = data.frame(time = inputs$time, pid = seq_len(inputs$n)),
                               mon = 2, col_time = "time", time = "absolute",
                               col_attributes = "pid")
  simmer::run(env, until = gp(pars, "des_run_until"))

  arr <- simmer::get_mon_arrivals(env)
  att <- simmer::get_mon_attributes(env)
  pid_of <- att[att$key == "pid", c("name", "value")]
  attempts <- stats::aggregate(value ~ name, data = att[att$key == "attempt", ], FUN = max)
  arr$pid <- pid_of$value[match(arr$name, pid_of$name)]
  arr$attempts <- attempts$value[match(arr$name, attempts$name)]
  patients <- data.frame(pid = seq_len(inputs$n), group = GROUPS[inputs$group],
                         backlog = inputs$backlog, navigated = navigated,
                         arrival = inputs$time)
  m <- match(patients$pid, arr$pid)
  patients$finished <- !is.na(m) & arr$finished[m]
  patients$completion_day <- ifelse(patients$finished, arr$end_time[m], NA_real_)
  patients$days_to_completion <- patients$completion_day - patients$arrival
  patients$attempts <- arr$attempts[m]
  res <- simmer::get_mon_resources(env)
  list(patients = patients, resources = res)
}

summarise_des_run <- function(run, pars) {
  p <- run$patients[!run$patients$backlog, ]      # new referrals only
  by_group <- do.call(rbind, lapply(c(GROUPS, "all"), function(g) {
    d <- if (g == "all") p else p[p$group == g, ]
    data.frame(group = g, n = nrow(d), share_finished = mean(d$finished),
               mean_days = mean(d$days_to_completion, na.rm = TRUE),
               p90_days = stats::quantile(d$days_to_completion, 0.9, na.rm = TRUE, names = FALSE),
               share_within_90d = mean(d$finished & d$days_to_completion <= 90),
               mean_attempts = mean(d$attempts, na.rm = TRUE))
  }))
  all_p <- run$patients
  slots_used <- sum(all_p$attempts, na.rm = TRUE)
  wasted <- sum(all_p$attempts - all_p$finished, na.rm = TRUE)
  r <- run$resources
  day_grid <- seq(0, gp(pars, "des_horizon_days"), by = 7)
  queue_at <- r$queue[pmax(1, findInterval(day_grid, r$time))]
  list(by_group = by_group,
       system = data.frame(slots_used = slots_used, share_slots_wasted = wasted / slots_used,
                           final_queue_at_horizon = queue_at[length(queue_at)],
                           mean_queue = mean(queue_at)),
       queue = data.frame(day = day_grid, queue = queue_at))
}

run_des_module <- function(pars, strategies, groups_df, reps = SETTINGS$des_replications,
                           seed = module_seed("des")) {
  by_group <- list(); system <- list(); queue <- list()
  for (r in seq_len(reps)) {
    inputs <- des_inputs(pars, groups_df, seed + r)   # one input set per replication
    for (s in seq_len(nrow(strategies))) {
      offered <- unlist(strategies[s, GROUPS])
      sm <- summarise_des_run(run_des_once(pars, inputs, offered), pars)
      tag <- data.frame(replication = r, strategy = strategies$strategy[s])
      by_group[[length(by_group) + 1]] <- cbind(tag, sm$by_group)
      system[[length(system) + 1]] <- cbind(tag, sm$system)
      queue[[length(queue) + 1]] <- cbind(tag, sm$queue)
    }
  }
  by_group <- do.call(rbind, by_group)
  summary <- stats::aggregate(cbind(mean_days, p90_days, share_within_90d, mean_attempts) ~
                                strategy + group, data = by_group, FUN = mean)
  list(by_group = by_group, system = do.call(rbind, system),
       queue = do.call(rbind, queue), summary = summary)
}
