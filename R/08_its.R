# R/08_its.R -------------------------------------------------------------------
# Interrupted time series (ITS), demonstrated on SYNTHETIC monthly monitoring data.
#
# What this module proves: the estimation pipeline recovers an effect that was
# planted in the data (a parameter-recovery test), handles trend, seasonality and
# autocorrelation, and passes a placebo test. What it does NOT prove: anything
# about real navigation programmes - the data are generated from the model's own
# assumptions, so agreement with the Markov inputs is a consistency check.
#
# Outcome: monthly share of referred patients whose pathway was completed within
# 90 days. Model: segmented regression on the empirical logit with a first
# harmonic for seasonality, estimated by generalised least squares with AR(1)
# errors (nlme::corAR1). Months excluded in sensitivity analysis are "dummied
# out" (one indicator each) rather than deleted, so the monthly series stays
# contiguous and the AR(1) structure remains valid.

ITS_GROUPS <- c("lower_barrier", "higher_barrier")

first_quarter_completion <- function(pars, group, navigated) {
  build_transition_array(pars, group, navigated)["unresolved", "managed", 1]
}

simulate_its_data <- function(pars, seed = module_seed("its")) {
  set.seed(seed)
  n_months <- gp(pars, "its_n_months"); launch <- gp(pars, "its_launch_month")
  month <- seq_len(n_months)
  date <- seq(as.Date("2021-01-01"), by = "month", length.out = n_months)
  month_of_year <- as.integer(format(date, "%m"))
  do.call(rbind, lapply(ITS_GROUPS, function(g) {
    p_uc <- first_quarter_completion(pars, g, FALSE)
    u <- gp(pars, "uptake", g)
    p_itt <- u * first_quarter_completion(pars, g, TRUE) + (1 - u) * p_uc
    planted_level <- stats::qlogis(p_itt) - stats::qlogis(p_uc)
    rho <- gp(pars, "its_ar1_rho"); s <- gp(pars, "its_ar1_sd")
    e <- numeric(n_months)
    e[1] <- stats::rnorm(1, 0, s / sqrt(1 - rho^2))       # stationary start
    for (t in 2:n_months) e[t] <- rho * e[t - 1] + stats::rnorm(1, 0, s)
    season <- -gp(pars, "its_season_amplitude") * cos(2 * pi * (month_of_year - 1) / 12)
    trend <- gp(pars, "its_trend_logit_per_year") * (month - launch) / 12
    post <- as.integer(month >= launch)
    eta <- stats::qlogis(p_uc) + trend + season + planted_level * post + e
    referrals <- stats::rpois(n_months, gp(pars, "its_referrals_month", g))
    completed <- stats::rbinom(n_months, size = referrals, prob = stats::plogis(eta))
    data.frame(group = g, month = month, date = date, month_of_year = month_of_year,
               post = post, time_after = (month - launch) * post,
               referrals = referrals, completed = completed,
               planted_level_logit = planted_level, p_usual_care = p_uc)
  }))
}

its_design <- function(d, launch) {
  d$post <- as.integer(d$month >= launch)
  d$time_after <- (d$month - launch) * d$post
  d$logit_p <- log((d$completed + 0.5) / (d$referrals - d$completed + 0.5))
  d$sin12 <- sin(2 * pi * d$month_of_year / 12)
  d$cos12 <- cos(2 * pi * d$month_of_year / 12)
  d
}

fit_gls_ar1 <- function(formula, d) {
  fit_once <- function(control) tryCatch(
    nlme::gls(formula, data = d, correlation = nlme::corAR1(form = ~ month),
              method = "REML", control = control),
    error = function(e) NULL)
  fit <- fit_once(nlme::glsControl())
  if (is.null(fit)) fit <- fit_once(nlme::glsControl(opt = "optim"))  # fallback optimiser
  if (is.null(fit)) stop("GLS with AR(1) errors did not converge.", call. = FALSE)
  fit
}

fit_its <- function(d, launch, exclude = integer(0)) {
  d <- its_design(d[order(d$month), ], launch)
  terms <- c("month", "post", "time_after", "sin12", "cos12")
  for (m in exclude) {                                # dummy out excluded months
    nm <- paste0("excl_", m); d[[nm]] <- as.integer(d$month == m); terms <- c(terms, nm)
  }
  f <- stats::reformulate(terms, response = "logit_p")
  ols <- stats::lm(f, data = d)
  gls <- fit_gls_ar1(f, d)
  tt <- summary(gls)$tTable
  df_resid <- nrow(d) - nrow(tt)
  coef_row <- function(term) {
    est <- tt[term, "Value"]; se <- tt[term, "Std.Error"]
    data.frame(term = term, estimate = est, se = se,
               ci_lo = est - stats::qt(0.975, df_resid) * se,
               ci_hi = est + stats::qt(0.975, df_resid) * se,
               p_value = tt[term, "p-value"],
               ols_se = summary(ols)$coefficients[term, "Std. Error"])
  }
  phi <- as.numeric(stats::coef(gls$modelStruct$corStruct, unconstrained = FALSE))
  list(model = gls, data = d,
       coefficients = rbind(coef_row("post"), coef_row("time_after")),
       diagnostics = data.frame(
         ar1_phi = phi,
         ljung_box_p_ols = stats::Box.test(stats::residuals(ols), lag = 12, type = "Ljung-Box")$p.value,
         ljung_box_p_gls = stats::Box.test(stats::residuals(gls, type = "normalized"),
                                           lag = 12, type = "Ljung-Box")$p.value))
}

# Bridge: convert a population-level (intention-to-treat) completion probability
# into the per-navigated-patient hazard ratio used by the Markov model, by
# inverting the model's own equations (tested for exact round-trip).
bridge_hazard_ratio <- function(p_itt, pars, group) {
  p_uc <- first_quarter_completion(pars, group, FALSE)
  u <- gp(pars, "uptake", group)
  p_nav <- (p_itt - (1 - u) * p_uc) / u
  P <- build_transition_array(pars, group, FALSE)
  survive_no_event <- P["unresolved", "managed", 1] / gp(pars, "p_complete", group)
  q_nav <- p_nav / survive_no_event; q_uc <- p_uc / survive_no_event
  hr <- log(1 - q_nav) / log(1 - q_uc)
  hr[!is.finite(hr) | q_nav <= 0 | q_nav >= 1] <- NA_real_
  hr
}

run_its <- function(pars, seed = module_seed("its")) {
  dat <- simulate_its_data(pars, seed)
  launch <- gp(pars, "its_launch_month")
  fits <- list(); coefs <- list(); diags <- list(); placebo <- list()
  sens <- list(); bridge <- list(); fitted <- list()
  for (g in ITS_GROUPS) {
    d <- dat[dat$group == g, ]
    f <- fit_its(d, launch)
    coefs[[g]] <- cbind(group = g, analysis = "main", f$coefficients,
                        planted = c(d$planted_level_logit[1], 0))
    diags[[g]] <- cbind(group = g, f$diagnostics)
    # Placebo: pre-launch months only, with a fake launch halfway through.
    pre <- d[d$month < launch, ]
    fp <- fit_its(pre, launch = floor(launch / 2) + 1)
    placebo[[g]] <- cbind(group = g, analysis = "placebo (pre-period only)",
                          fp$coefficients, planted = 0)
    # Sensitivity: drop the first two months after launch (implementation lag).
    fs <- fit_its(d, launch, exclude = c(launch, launch + 1))
    sens[[g]] <- cbind(group = g, analysis = "exclude 2 launch months",
                       fs$coefficients, planted = c(d$planted_level_logit[1], 0))
    # Bridge to the Markov input, with uncertainty from the level-change estimate.
    b <- f$coefficients[f$coefficients$term == "post", ]
    set.seed(seed + 1)                                  # sub-stream for the bridge
    sim_level <- stats::rnorm(10000, b$estimate, b$se)
    p_uc <- d$p_usual_care[1]
    hr_sim <- bridge_hazard_ratio(stats::plogis(stats::qlogis(p_uc) + sim_level), pars, g)
    bridge[[g]] <- data.frame(
      group = g, model_input_hr = gp(pars, "hr_navigation", g),
      its_implied_hr = bridge_hazard_ratio(stats::plogis(stats::qlogis(p_uc) + b$estimate), pars, g),
      its_hr_lo = stats::quantile(hr_sim, 0.025, na.rm = TRUE, names = FALSE),
      its_hr_hi = stats::quantile(hr_sim, 0.975, na.rm = TRUE, names = FALSE))
    # Fitted and counterfactual series (fixed effects only) for the figure.
    X <- f$data
    X_cf <- X; X_cf$post <- 0; X_cf$time_after <- 0
    fitted[[g]] <- data.frame(group = g, date = X$date,
                              observed = X$completed / X$referrals,
                              fitted = stats::plogis(stats::predict(f$model, newdata = X)),
                              counterfactual = stats::plogis(stats::predict(f$model, newdata = X_cf)),
                              post = X$post)
  }
  list(data = dat,
       estimates = do.call(rbind, c(coefs, placebo, sens)),
       diagnostics = do.call(rbind, diags),
       bridge = do.call(rbind, bridge),
       fitted = do.call(rbind, fitted))
}

# Monte Carlo calibration: re-generate the synthetic data many times and check
# that the 95% CI covers the planted effect about 95% of the time and that the
# placebo test is "significant" only about 5% of the time. A single seeded
# dataset can pass or fail a check by chance; this shows the method is calibrated.
its_coverage_study <- function(pars, reps = SETTINGS$its_coverage_reps,
                               seed = module_seed("its")) {
  if (reps < 1) return(NULL)
  launch <- gp(pars, "its_launch_month")
  rows <- lapply(seq_len(reps), function(r) {
    dat <- simulate_its_data(pars, seed + 100 + r)      # sub-streams seed + 101, 102, ...
    do.call(rbind, lapply(ITS_GROUPS, function(g) {
      d <- dat[dat$group == g, ]
      m <- fit_its(d, launch)$coefficients
      pl <- fit_its(d[d$month < launch, ], floor(launch / 2) + 1)$coefficients
      planted <- d$planted_level_logit[1]
      data.frame(replication = r, group = g,
                 bias = m$estimate[1] - planted,
                 covered = m$ci_lo[1] <= planted & planted <= m$ci_hi[1],
                 placebo_false_positive = pl$ci_lo[1] > 0 | pl$ci_hi[1] < 0)
    }))
  })
  d <- do.call(rbind, rows)
  stats::aggregate(cbind(bias, covered, placebo_false_positive) ~ group, data = d, FUN = mean)
}
