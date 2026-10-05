# R/09_rdd.R -------------------------------------------------------------------
# Regression discontinuity design (RDD), demonstrated on SYNTHETIC patient data.
# Setting: patients with a barrier-risk score at or above a cutoff are OFFERED
# navigation; 70% of those offered take it up, and 5% of patients below the
# cutoff are navigated anyway through clinician discretion (a fuzzy design with
# two-sided non-compliance, as is usual in practice).
#   Sharp estimate  = effect of the OFFER at the cutoff (intention to treat).
#   Fuzzy estimate  = effect of NAVIGATION among patients whose uptake the offer
#                     changed (a local effect at the cutoff only).
# Estimation uses rdrobust (local polynomial, MSE-optimal bandwidth, robust
# bias-corrected confidence intervals), with the standard falsification checks.

simulate_rdd_data <- function(pars, seed = module_seed("rdd")) {
  set.seed(seed)
  n <- gp(pars, "rdd_n"); cutoff <- gp(pars, "rdd_cutoff")
  mu <- gp(pars, "rdd_score_mean"); sdv <- gp(pars, "rdd_score_sd")
  # Continuous truncated normal on [0, 100] by inversion: no artificial mass points.
  score <- stats::qnorm(stats::runif(n, stats::pnorm(0, mu, sdv), stats::pnorm(100, mu, sdv)), mu, sdv)
  offered <- as.integer(score >= cutoff)
  p_nav <- ifelse(offered == 1, gp(pars, "rdd_uptake_offered"), gp(pars, "rdd_uptake_not_offered"))
  navigated <- stats::rbinom(n, 1, p_nav)
  p0 <- gp(pars, "rdd_p_cutoff_untreated")
  tau_logit <- stats::qlogis(p0 + gp(pars, "rdd_effect_navigated")) - stats::qlogis(p0)
  eta <- stats::qlogis(p0) + gp(pars, "rdd_slope_logit_per10") * (score - cutoff) / 10 +
    tau_logit * navigated
  data.frame(score = score, offered = offered, navigated = navigated,
             completed_90d = stats::rbinom(n, 1, stats::plogis(eta)),
             age = stats::rnorm(n, gp(pars, "rdd_age_mean"), gp(pars, "rdd_age_sd")))
}

rd_row <- function(fit, label, planted = NA_real_) {
  data.frame(analysis = label,
             estimate = fit$coef[1, 1],                 # conventional point estimate
             robust_ci_lo = fit$ci[3, 1], robust_ci_hi = fit$ci[3, 2],
             robust_p = fit$pv[3, 1],
             bandwidth = fit$bws[1, 1],
             n_left = fit$N_h[1], n_right = fit$N_h[2],
             planted = planted,
             covers_planted = if (is.na(planted)) NA else
               (planted >= fit$ci[3, 1] & planted <= fit$ci[3, 2]))
}

run_rdd <- function(pars, seed = module_seed("rdd")) {
  d <- simulate_rdd_data(pars, seed)
  c0 <- gp(pars, "rdd_cutoff")
  effect <- gp(pars, "rdd_effect_navigated")
  first_stage <- gp(pars, "rdd_uptake_offered") - gp(pars, "rdd_uptake_not_offered")
  sharp <- rdrobust::rdrobust(y = d$completed_90d, x = d$score, c = c0)
  fuzzy <- rdrobust::rdrobust(y = d$completed_90d, x = d$score, c = c0, fuzzy = d$navigated)
  h <- sharp$bws[1, 1]
  est <- rbind(rd_row(sharp, "Sharp: effect of the offer (ITT)", effect * first_stage),
               rd_row(fuzzy, "Fuzzy: effect of navigation (local, compliers)", effect))
  # Falsification 1: no manipulation of the score around the cutoff.
  dens <- rddensity::rddensity(X = d$score, c = c0)
  # Falsification 2: a pre-determined covariate should not jump at the cutoff.
  bal <- rdrobust::rdrobust(y = d$age, x = d$score, c = c0)
  # Falsification 3: placebo cutoffs, each using only one side of the true cutoff.
  plc_lo <- with(d[d$score < c0, ], rdrobust::rdrobust(completed_90d, score, c = c0 - 10))
  plc_hi <- with(d[d$score >= c0, ], rdrobust::rdrobust(completed_90d, score, c = c0 + 10))
  # Robustness: bandwidth sensitivity and a donut hole around the cutoff.
  bw <- do.call(rbind, lapply(c(0.5, 0.75, 1, 1.5, 2), function(m) {
    f <- rdrobust::rdrobust(d$completed_90d, d$score, c = c0, h = m * h)
    cbind(multiplier = m, rd_row(f, paste0("Sharp, bandwidth x", m), effect * first_stage))
  }))
  donut_d <- d[abs(d$score - c0) >= 1, ]
  donut <- rdrobust::rdrobust(donut_d$completed_90d, donut_d$score, c = c0)
  checks <- rbind(
    rd_row(bal, "Covariate balance: age (should be ~0)", 0),
    rd_row(plc_lo, paste0("Placebo cutoff ", c0 - 10, " (should be ~0)"), 0),
    rd_row(plc_hi, paste0("Placebo cutoff ", c0 + 10, " (should be ~0)"), 0),
    rd_row(donut, "Donut hole: exclude |score - cutoff| < 1", effect * first_stage))
  # Plot a window around the cutoff with local linear fits: a global quartic
  # (rdplot's default) wiggles at the edges and misleads the eye.
  near <- abs(d$score - c0) <= 30
  plot <- rdrobust::rdplot(d$completed_90d[near], d$score[near], c = c0, p = 1, hide = TRUE,
                           x.label = "Barrier-risk score", y.label = "Completed within 90 days",
                           title = "Synthetic RDD: completion within 90 days by score")
  list(data = d, estimates = est, checks = checks, bandwidth_sensitivity = bw,
       density_test = data.frame(p_value = dens$test$p_jk,
                                 interpretation = "p > 0.05: no evidence of manipulation"),
       plot = plot$rdplot)
}

# Monte Carlo calibration of the RDD estimators (see its_coverage_study()).
rdd_coverage_study <- function(pars, reps = SETTINGS$rdd_coverage_reps,
                               seed = module_seed("rdd")) {
  if (reps < 1) return(NULL)
  c0 <- gp(pars, "rdd_cutoff"); effect <- gp(pars, "rdd_effect_navigated")
  first_stage <- gp(pars, "rdd_uptake_offered") - gp(pars, "rdd_uptake_not_offered")
  rows <- lapply(seq_len(reps), function(r) {
    d <- simulate_rdd_data(pars, seed + 100 + r)
    sh <- rdrobust::rdrobust(d$completed_90d, d$score, c = c0)
    fz <- rdrobust::rdrobust(d$completed_90d, d$score, c = c0, fuzzy = d$navigated)
    rbind(data.frame(replication = r, estimator = "sharp (ITT)",
                     bias = sh$coef[1, 1] - effect * first_stage,
                     covered = sh$ci[3, 1] <= effect * first_stage & effect * first_stage <= sh$ci[3, 2]),
          data.frame(replication = r, estimator = "fuzzy (compliers)",
                     bias = fz$coef[1, 1] - effect,
                     covered = fz$ci[3, 1] <= effect & effect <= fz$ci[3, 2]))
  })
  d <- do.call(rbind, rows)
  stats::aggregate(cbind(bias, covered) ~ estimator, data = d, FUN = mean)
}
