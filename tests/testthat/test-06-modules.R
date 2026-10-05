# tests/testthat/test-06-modules.R ---------------------------------------------
# ECEA-lite, ITS, RDD and DES. The stochastic modules use fixed seeds, so these
# tests are deterministic.

test_that("ECEA-lite microsimulation matches the cohort model's expected cost", {
  e <- run_ecea_lite(BASE, STRATEGIES, n = 20000, seed = 7)$cross_check
  expect_true(all(abs(e$micro_mean_not_offered - e$cohort_expected_not_offered) <
                    4 * e$micro_se_not_offered))
  expect_true(all(abs(e$micro_mean_offered - e$cohort_expected_offered) < 4 * e$micro_se_offered))
})

test_that("ECEA-lite: an offer with zero uptake changes nothing (common random numbers)", {
  p <- with_values(c(`uptake@higher_barrier` = 0))
  rnd <- ecea_random_inputs(5000, 8)
  a <- simulate_year1_burden(p, "higher_barrier", FALSE, rnd)
  b <- simulate_year1_burden(p, "higher_barrier", TRUE, rnd)
  expect_identical(a$hh_cost, b$hh_cost)
})

test_that("ITS bridge round trip: the model's own ITT effect maps back to its hazard ratio", {
  for (g in ITS_GROUPS) {
    u <- gp(BASE, "uptake", g)
    p_itt <- u * first_quarter_completion(BASE, g, TRUE) +
      (1 - u) * first_quarter_completion(BASE, g, FALSE)
    expect_equal(bridge_hazard_ratio(p_itt, BASE, g), gp(BASE, "hr_navigation", g), tolerance = 1e-10)
  }
})

test_that("ITS: the 95% CI covers the planted level change, and the slope change is ~0", {
  its <- run_its(BASE)
  main <- its$estimates[its$estimates$analysis == "main", ]
  post <- main[main$term == "post", ]
  expect_true(all(post$ci_lo <= post$planted & post$planted <= post$ci_hi))
  slope <- main[main$term == "time_after", ]
  expect_true(all(slope$ci_lo <= 0 & 0 <= slope$ci_hi))
})

test_that("RDD: estimates cover the planted effects and falsification checks pass", {
  rdd <- run_rdd(BASE)
  expect_true(all(rdd$estimates$covers_planted))
  expect_true(all(rdd$checks$covers_planted))
  expect_gt(rdd$density_test$p_value, 0.05)
})

test_that("DES: with no no-shows every strategy gives identical results", {
  p <- BASE
  for (g in GROUPS) p[paste0("des_p_noshow@", g)] <- 0
  inputs <- des_inputs(p, GROUPS_DF, seed = 123)
  a <- run_des_once(p, inputs, c(0, 0, 0))$patients
  b <- run_des_once(p, inputs, c(1, 1, 1))$patients
  expect_identical(a$completion_day, b$completion_day)
})

test_that("DES: every referral finishes, no time is negative, and navigation shortens waits", {
  inputs <- des_inputs(BASE, GROUPS_DF, seed = 124)
  uc <- run_des_once(BASE, inputs, c(0, 0, 0))$patients
  un <- run_des_once(BASE, inputs, c(1, 1, 1))$patients
  expect_true(all(uc$finished) && all(un$finished))
  expect_true(all(uc$days_to_completion >= 0))
  expect_lt(mean(un$days_to_completion), mean(uc$days_to_completion))
})
