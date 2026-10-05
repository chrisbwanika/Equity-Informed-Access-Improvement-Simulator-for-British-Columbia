# R/11_figures.R ---------------------------------------------------------------
# Every figure in the report. Plotting is kept apart from computation so the
# analysis functions stay small and testable. Colours are the Okabe-Ito
# colour-blind-safe palette.

STRATEGY_COLOURS <- c(usual_care = "#7F7F7F", targeted_navigation = "#E69F00",
                      universal_navigation = "#0072B2")
STRATEGY_LABELS <- c(usual_care = "Usual care", targeted_navigation = "Targeted navigation",
                     universal_navigation = "Universal navigation")
GROUP_COLOURS <- c(lower_barrier = "#56B4E9", moderate_barrier = "#009E73",
                   higher_barrier = "#D55E00")
GROUP_LABELS <- c(lower_barrier = "Lower barrier", moderate_barrier = "Moderate barrier",
                  higher_barrier = "Higher barrier")

comma <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)

theme_eas <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom", panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold"))
}

plot_trace <- function(pars, group = "higher_barrier", years = 10) {
  cl <- gp(pars, "cycle_length")
  df <- do.call(rbind, lapply(c(FALSE, TRUE), function(nav) {
    tr <- run_group_arm(pars, group, nav)$trace
    keep <- seq_len(round(years / cl) + 1)
    data.frame(year = rep((keep - 1) * cl, ncol(tr)),
               state = rep(colnames(tr), each = length(keep)),
               share = as.vector(tr[keep, ]),
               arm = if (nav) "Navigated" else "Usual care")
  }))
  df$state <- factor(df$state, levels = STATES)
  ggplot2::ggplot(df, ggplot2::aes(year, share, colour = state, linetype = arm)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::labs(title = paste("Cohort trace:", GROUP_LABELS[[group]], "group"),
                  x = "Years since referral", y = "Share of cohort", colour = NULL, linetype = NULL) +
    theme_eas()
}

plot_ce_plane <- function(psa_strategy, k) {
  uc <- psa_strategy[psa_strategy$strategy == "usual_care", ]
  d <- do.call(rbind, lapply(c("targeted_navigation", "universal_navigation"), function(s) {
    a <- psa_strategy[psa_strategy$strategy == s, ]
    data.frame(strategy = s, dq = a$qaly - uc$qaly[match(a$iteration, uc$iteration)],
               dc = a$cost - uc$cost[match(a$iteration, uc$iteration)])
  }))
  ggplot2::ggplot(d, ggplot2::aes(dq, dc, colour = strategy)) +
    ggplot2::geom_point(alpha = 0.25, size = 0.8) +
    ggplot2::geom_abline(slope = k, intercept = 0, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0) + ggplot2::geom_vline(xintercept = 0) +
    ggplot2::scale_colour_manual(values = STRATEGY_COLOURS, labels = STRATEGY_LABELS) +
    ggplot2::labs(title = "Cost-effectiveness plane (vs usual care)",
                  subtitle = paste0("Dashed line: CAD ", comma(k), " per QALY"),
                  x = "Incremental QALYs per patient", y = "Incremental cost per patient (CAD)",
                  colour = NULL) + theme_eas()
}

plot_ceac <- function(ceac) {
  front <- ceac[ceac$on_frontier, ]
  ggplot2::ggplot(ceac, ggplot2::aes(k, prob_optimal, colour = strategy)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(data = front, size = 1.4) +
    ggplot2::scale_colour_manual(values = STRATEGY_COLOURS, labels = STRATEGY_LABELS) +
    ggplot2::scale_x_continuous(labels = comma) +
    ggplot2::labs(title = "Cost-effectiveness acceptability curves",
                  subtitle = "Points mark the frontier: the strategy with the highest expected NMB",
                  x = "Threshold (CAD per QALY)", y = "Probability of highest NMB", colour = NULL) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) + theme_eas()
}

plot_evpi <- function(ceac, n_cohort) {
  e <- unique(ceac[, c("k", "evpi_per_patient")])
  ggplot2::ggplot(e, ggplot2::aes(k, evpi_per_patient * n_cohort)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_x_continuous(labels = comma) +
    ggplot2::scale_y_continuous(labels = comma) +
    ggplot2::labs(title = "Expected value of perfect information",
                  subtitle = "For one annual referral cohort (per-patient EVPI x cohort size)",
                  x = "Threshold (CAD per QALY)", y = "EVPI (CAD)") + theme_eas()
}

plot_tornado <- function(owsa, top = 10) {
  d <- do.call(rbind, lapply(split(owsa, owsa$strategy), function(x) utils::head(x[order(-x$swing), ], top)))
  d$label <- factor(paste(d$strategy, d$parameter), levels = rev(unique(paste(d$strategy, d$parameter))))
  ggplot2::ggplot(d) +
    ggplot2::geom_segment(ggplot2::aes(x = inmb_low, xend = inmb_high, y = label, yend = label),
                          linewidth = 4, colour = "#0072B2", alpha = 0.7) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = inmb_base), linetype = "dashed") +
    ggplot2::scale_y_discrete(labels = function(x) sub("^[a-z_]+ ", "", x)) +
    ggplot2::facet_wrap(~ strategy, scales = "free", ncol = 1,
                        labeller = ggplot2::as_labeller(STRATEGY_LABELS)) +
    ggplot2::labs(title = "One-way sensitivity analysis (incremental NMB vs usual care)",
                  subtitle = "Each bar: parameter at its 2.5th and 97.5th PSA percentiles",
                  x = "Incremental NMB per patient (CAD)", y = NULL) + theme_eas()
}

plot_convergence <- function(conv) {
  ggplot2::ggplot(conv, ggplot2::aes(iteration, running_mean_inmb, colour = strategy)) +
    ggplot2::geom_line() +
    ggplot2::scale_colour_manual(values = STRATEGY_COLOURS, labels = STRATEGY_LABELS) +
    ggplot2::labs(title = "PSA convergence", x = "Iteration",
                  y = "Running mean incremental NMB (CAD)", colour = NULL) + theme_eas()
}

plot_dcea_distribution <- function(dist) {
  d <- dist[dist$oc_scenario == SETTINGS$oc_base_scenario, ]
  d$group <- factor(d$group, levels = GROUPS)
  ggplot2::ggplot(d, ggplot2::aes(group, change_per_capita * 365.25, fill = group)) +
    ggplot2::geom_col() + ggplot2::geom_hline(yintercept = 0) +
    ggplot2::facet_wrap(~ strategy, labeller = ggplot2::as_labeller(STRATEGY_LABELS)) +
    ggplot2::scale_fill_manual(values = GROUP_COLOURS, labels = GROUP_LABELS, guide = "none") +
    ggplot2::scale_x_discrete(labels = GROUP_LABELS) +
    ggplot2::labs(title = "Net health change per person in the population, by group",
                  subtitle = "Health gains minus health opportunity costs (base-case distribution rule)",
                  x = NULL, y = "Quality-adjusted life-days per person") + theme_eas()
}

plot_equity_plane <- function(dcea_psa, dcea_det) {
  base <- dcea_det[dcea_det$oc_scenario == SETTINGS$oc_base_scenario, ]
  ggplot2::ggplot(dcea_psa, ggplot2::aes(total_nhb, gap_reduction_days, colour = strategy)) +
    ggplot2::geom_point(alpha = 0.2, size = 0.8) +
    ggplot2::geom_point(data = base, size = 3, shape = 21, fill = "white", stroke = 1.2) +
    ggplot2::geom_hline(yintercept = 0) + ggplot2::geom_vline(xintercept = 0) +
    ggplot2::scale_colour_manual(values = STRATEGY_COLOURS, labels = STRATEGY_LABELS) +
    ggplot2::labs(title = "Equity-efficiency impact plane (vs usual care)",
                  subtitle = "Top-right quadrant = more total health AND a smaller gap (win-win)",
                  x = "Net health benefit (QALYs, whole population)",
                  y = "Reduction in lower-higher QALE gap (days per person)", colour = NULL) +
    theme_eas()
}

plot_ede_epsilon <- function(tradeoff) {
  d <- rbind(data.frame(k = tradeoff$k, epsilon = tradeoff$epsilon,
                        strategy = "targeted_navigation", value = tradeoff$ew_nhb_a),
             data.frame(k = tradeoff$k, epsilon = tradeoff$epsilon,
                        strategy = "universal_navigation", value = tradeoff$ew_nhb_b))
  d$k_label <- factor(paste0("Threshold CAD ", comma(d$k), " per QALY"),
                      levels = paste0("Threshold CAD ", comma(sort(unique(d$k))), " per QALY"))
  ggplot2::ggplot(d, ggplot2::aes(epsilon, value, colour = strategy)) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::facet_wrap(~ k_label, scales = "free_y") +
    ggplot2::scale_colour_manual(values = STRATEGY_COLOURS, labels = STRATEGY_LABELS) +
    ggplot2::labs(title = "Equity-weighted net health benefit across inequality aversion",
                  subtitle = "Atkinson EDE; epsilon = 0 is ordinary (unweighted) net health benefit",
                  x = "Inequality aversion (Atkinson epsilon)",
                  y = "Equity-weighted NHB (EDE-equivalent QALYs)", colour = NULL) + theme_eas()
}

plot_ecea <- function(ecea_results, threshold) {
  col <- paste0("share_above_", threshold)
  d <- ecea_results; d$value <- d[[col]]; d$group <- factor(d$group, levels = GROUPS)
  ggplot2::ggplot(d, ggplot2::aes(group, value, fill = strategy)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::scale_fill_manual(values = STRATEGY_COLOURS, labels = STRATEGY_LABELS) +
    ggplot2::scale_x_discrete(labels = GROUP_LABELS) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
    ggplot2::labs(title = "Households with a high first-year non-medical burden",
                  subtitle = paste0("Burden above ", 100 * threshold, "% of annual household income"),
                  x = NULL, y = "Share of patients", fill = NULL) + theme_eas()
}

plot_its <- function(fitted) {
  fitted$group <- factor(fitted$group, levels = ITS_GROUPS, labels = GROUP_LABELS[ITS_GROUPS])
  ggplot2::ggplot(fitted, ggplot2::aes(date)) +
    ggplot2::geom_point(ggplot2::aes(y = observed), size = 1, colour = "grey40") +
    ggplot2::geom_line(ggplot2::aes(y = fitted), colour = "#0072B2", linewidth = 0.8) +
    ggplot2::geom_line(data = fitted[fitted$post == 1, ], ggplot2::aes(y = counterfactual),
                       colour = "#D55E00", linetype = "dashed", linewidth = 0.8) +
    ggplot2::facet_wrap(~ group) +
    ggplot2::labs(title = "Interrupted time series (synthetic data)",
                  subtitle = "Blue: fitted model; dashed: counterfactual without navigation",
                  x = NULL, y = "Completed within 90 days") + theme_eas()
}

plot_des_queue <- function(queue) {
  m <- stats::aggregate(queue ~ strategy + day, data = queue, FUN = mean)
  lo <- stats::aggregate(queue ~ strategy + day, data = queue, FUN = stats::quantile, probs = 0.1)
  hi <- stats::aggregate(queue ~ strategy + day, data = queue, FUN = stats::quantile, probs = 0.9)
  m$lo <- lo$queue; m$hi <- hi$queue
  ggplot2::ggplot(m, ggplot2::aes(day, queue, colour = strategy, fill = strategy)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo, ymax = hi), alpha = 0.15, colour = NA) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::scale_colour_manual(values = STRATEGY_COLOURS, labels = STRATEGY_LABELS) +
    ggplot2::scale_fill_manual(values = STRATEGY_COLOURS, labels = STRATEGY_LABELS) +
    ggplot2::labs(title = "Diagnostic waitlist over time (DES)",
                  subtitle = "Mean across replications; band = 10th to 90th percentile",
                  x = "Day", y = "Patients waiting", colour = NULL, fill = NULL) + theme_eas()
}

plot_des_waits <- function(by_group) {
  d <- by_group[by_group$group != "all", ]
  s <- stats::aggregate(mean_days ~ strategy + group, data = d, FUN = mean)
  s$lo <- stats::aggregate(mean_days ~ strategy + group, data = d, FUN = stats::quantile, probs = 0.025)$mean_days
  s$hi <- stats::aggregate(mean_days ~ strategy + group, data = d, FUN = stats::quantile, probs = 0.975)$mean_days
  s$group <- factor(s$group, levels = GROUPS)
  ggplot2::ggplot(s, ggplot2::aes(group, mean_days, fill = strategy)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(0.9)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lo, ymax = hi), width = 0.2,
                           position = ggplot2::position_dodge(0.9)) +
    ggplot2::scale_fill_manual(values = STRATEGY_COLOURS, labels = STRATEGY_LABELS) +
    ggplot2::scale_x_discrete(labels = GROUP_LABELS) +
    ggplot2::labs(title = "Days from referral to completed appointment (DES)",
                  subtitle = "Mean across replications; bars = 95% range across replications",
                  x = NULL, y = "Days", fill = NULL) + theme_eas()
}
