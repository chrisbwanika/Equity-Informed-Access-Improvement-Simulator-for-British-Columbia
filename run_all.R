# run_all.R --------------------------------------------------------------------
# Reproduces every result in this repository from scratch.
#
#   From the project root:   Rscript run_all.R          (terminal)
#   or, in RStudio:          source("run_all.R")        (with the .Rproj open)
#
# Then run the tests (Rscript tests/run_tests.R) and render the report
# (quarto render). Expected run time: a few minutes on a laptop.
# All inputs come from data/*.csv; all settings come from R/00_setup.R.

start_time <- Sys.time()
for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)
assert_project_root()
check_packages()
ensure_output_dirs()

par_df     <- read_parameters()
groups_df  <- read_groups()
strategies <- read_strategies()
pars       <- base_case_values(par_df)
k          <- gp(pars, "k_threshold")

# 1. Deterministic base case ---------------------------------------------------------
message("1/9 Base case")
base <- run_base_case(pars, strategies, groups_df)
write_output(base$group, "base_case_by_group.csv")
write_output(base$strategy, "base_case_by_strategy.csv")
write_output(base$incremental, "base_case_incremental.csv")
write_output(base$vs_usual_care, "base_case_vs_usual_care.csv")

# 2-3. One-way sensitivity and scenario analyses --------------------------------------
message("2/9 One-way sensitivity analysis")
owsa <- run_owsa(par_df, pars, strategies, groups_df)
write_output(owsa, "owsa.csv")
message("3/9 Scenario analysis")
write_output(run_scenarios(pars, strategies, groups_df), "scenarios.csv")

# 4. Probabilistic sensitivity analysis ------------------------------------------------
message("4/9 PSA (", SETTINGS$n_psa, " iterations)")
psa <- run_psa(par_df, strategies, groups_df)
write_output(psa$draws, "psa_parameter_draws.csv")
write_output(psa$strategy, "psa_results_by_strategy.csv")
write_output(psa$group, "psa_results_by_group.csv")
write_output(summarise_psa(psa$strategy, strategies$strategy), "psa_summary.csv")
ceac <- ceac_evpi(psa$strategy, strategies$strategy)
write_output(ceac, "psa_ceac_evpi.csv")
conv <- psa_convergence(psa$strategy, strategies$strategy, k)
write_output(conv, "psa_convergence.csv")

# 5. Distributional cost-effectiveness analysis ------------------------------------------
message("5/9 DCEA")
dcea_det <- run_dcea_deterministic(base$group, groups_df, pars, strategies)
write_output(dcea_det$distribution, "dcea_distribution.csv")
write_output(dcea_det$metrics, "dcea_metrics.csv")
dcea_psa <- run_dcea_psa(psa, groups_df, strategies, par_df)
write_output(dcea_psa, "dcea_psa.csv")
write_output(summarise_dcea_psa(dcea_psa), "dcea_psa_summary.csv")
tradeoff <- dcea_tradeoff(base$group, groups_df, pars, k_values = c(k, 100000))
write_output(tradeoff, "dcea_tradeoff.csv")

# 6. ECEA-lite --------------------------------------------------------------------------
message("6/9 ECEA-lite")
ecea <- run_ecea_lite(pars, strategies)
write_output(ecea$results, "ecea_results.csv")
write_output(ecea$cross_check, "ecea_cross_check.csv")

# 7. Interrupted time series --------------------------------------------------------------
message("7/9 ITS (with ", SETTINGS$its_coverage_reps, " calibration datasets)")
its <- run_its(pars)
write_output(its$data, file.path("generated_data", "synthetic_its.csv"))
write_output(its$estimates, "its_estimates.csv")
write_output(its$diagnostics, "its_diagnostics.csv")
write_output(its$bridge, "its_bridge.csv")
write_output(its_coverage_study(pars), "its_calibration.csv")

# 8. Regression discontinuity ---------------------------------------------------------------
message("8/9 RDD (with ", SETTINGS$rdd_coverage_reps, " calibration datasets)")
rdd <- run_rdd(pars)
write_output(rdd$data, file.path("generated_data", "synthetic_rdd.csv"))
write_output(rdd$estimates, "rdd_estimates.csv")
write_output(rdd$checks, "rdd_checks.csv")
write_output(rdd$bandwidth_sensitivity, "rdd_bandwidth_sensitivity.csv")
write_output(rdd$density_test, "rdd_density_test.csv")
write_output(rdd_coverage_study(pars), "rdd_calibration.csv")

# 9. Discrete event simulation ------------------------------------------------------------------
message("9/9 DES (", SETTINGS$des_replications, " replications per strategy)")
des <- run_des_module(pars, strategies, groups_df)
write_output(des$by_group, "des_by_group.csv")
write_output(des$system, "des_system.csv")
write_output(des$summary, "des_summary.csv")
write_output(des$queue, "des_queue.csv")

# Figures --------------------------------------------------------------------------------------
message("Figures")
save_figure(plot_trace(pars), "fig01_trace.png")
save_figure(plot_ce_plane(psa$strategy, k), "fig02_ce_plane.png")
save_figure(plot_ceac(ceac), "fig03_ceac.png")
save_figure(plot_evpi(ceac, gp(pars, "n_cohort")), "fig04_evpi.png")
save_figure(plot_tornado(owsa), "fig05_tornado.png", height = 7)
save_figure(plot_convergence(conv), "fig06_convergence.png")
save_figure(plot_dcea_distribution(dcea_det$distribution), "fig07_dcea_distribution.png")
save_figure(plot_equity_plane(dcea_psa, dcea_det$metrics), "fig08_equity_plane.png")
save_figure(plot_ede_epsilon(tradeoff), "fig09_ede_epsilon.png")
save_figure(plot_ecea(ecea$results, gp(pars, "burden_threshold")), "fig10_ecea.png")
save_figure(plot_its(its$fitted), "fig11_its.png")
save_figure(rdd$plot, "fig12_rdd.png")
save_figure(plot_des_queue(des$queue), "fig13_des_queue.png")
save_figure(plot_des_waits(des$by_group), "fig14_des_waits.png")

write_run_manifest(start_time)
message("Done in ", format(round(difftime(Sys.time(), start_time, units = "mins"), 1)),
        ". Next: Rscript tests/run_tests.R, then quarto render.")
