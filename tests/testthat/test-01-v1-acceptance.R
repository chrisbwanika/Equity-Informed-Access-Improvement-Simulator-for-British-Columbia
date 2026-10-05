# tests/testthat/test-01-v1-acceptance.R ---------------------------------------
# One test for each defect found in the v1 blueprint review. If any of these
# fails, a v1 error has come back.

test_that("v1 bug 1: gamma draws use shape and SCALE, so the mean is preserved", {
  set.seed(1)
  x <- draw_parameter(1e5, "gamma", value = 5000, se = 1000)
  expect_equal(mean(x), 5000, tolerance = 0.01)
  # The v1 trap, kept as a reminder: positional arguments make 200 a RATE.
  set.seed(1)
  expect_lt(mean(stats::rgamma(1e5, 25, 200)), 1)
})

test_that("v1 bug 2: navigation cost is charged once, at time 0, per navigated patient", {
  p <- with_values(c(`hr_navigation@lower_barrier` = 1, `hr_navigation@moderate_barrier` = 1,
                     `hr_navigation@higher_barrier` = 1))
  r <- evaluate_strategies(p, STRATEGIES, GROUPS_DF)$group
  for (g in GROUPS) {
    uni <- r[r$strategy == "universal_navigation" & r$group == g, ]
    uc  <- r[r$strategy == "usual_care" & r$group == g, ]
    expect_equal(uni$cost - uc$cost, gp(p, "uptake", g) * gp(p, "c_navigation"), tolerance = 1e-10)
    expect_equal(uni$qaly - uc$qaly, 0, tolerance = 1e-12)
  }
})

test_that("v1 bug 3: equity results come from the inputs, not from the model's design", {
  # (a) Make the three groups identical in every input: the gap cannot change.
  p <- BASE
  for (nm in c("p_complete", "p_acute_unresolved", "p_disengage", "hr_bg_mortality",
               "uptake", "hr_navigation")) {
    for (g in GROUPS) p[paste0(nm, "@", g)] <- gp(BASE, nm, "moderate_barrier")
  }
  g_same <- GROUPS_DF
  g_same$cohort_share <- g_same$pop_share
  g_same$baseline_qale <- 67
  b_same <- run_base_case(p, STRATEGIES, g_same)
  m_same <- run_dcea_deterministic(b_same$group, g_same, p, STRATEGIES)$metrics
  # Universal offer + opportunity costs shared in proportion to population:
  # identical groups gain identically, so the gap cannot move.
  neutral <- m_same$strategy == "universal_navigation" & m_same$oc_scenario == "oc_proportional"
  expect_equal(m_same$gap_reduction[neutral], 0, tolerance = 1e-12)
  # Targeting one group DOES move the gap, even when groups are identical:
  # the distribution follows the strategy and the inputs, not the model's design.
  targeted <- m_same$strategy == "targeted_navigation" & m_same$oc_scenario == "oc_proportional"
  expect_gt(m_same$gap_reduction[targeted], 0)
  # (b) With the real inputs, equity weighting is NOT a fixed multiple of plain NHB
  #     (in v1 it always was exactly 1.0435 x NHB, so it could never change a decision).
  b <- run_base_case(BASE, STRATEGIES, GROUPS_DF)
  m <- run_dcea_deterministic(b$group, GROUPS_DF, BASE, STRATEGIES)$metrics
  m <- m[m$oc_scenario == "oc_proportional", ]
  ratio <- m$ew_nhb_atkinson_10 / m$total_nhb
  expect_gt(abs(ratio[1] - ratio[2]), 0.01 * abs(ratio[2]))
})

test_that("v1 bug 4: Atkinson EDE refuses non-positive values; DCEA applies it to health levels", {
  expect_error(ede_atkinson(c(1, -0.5, 2), rep(1 / 3, 3), 0.5))
  expect_error(ede_atkinson(c(1, 0, 2), rep(1 / 3, 3), 2))
  b <- run_base_case(BASE, STRATEGIES, GROUPS_DF)
  dist <- run_dcea_deterministic(b$group, GROUPS_DF, BASE, STRATEGIES)$distribution
  expect_true(all(dist$baseline_qale > 0) && all(dist$post_qale > 0))
})

test_that("v1 bug 5: validity checks use a tolerance, and the cohort is conserved every cycle", {
  set.seed(2)
  m <- matrix(stats::runif(16), 4)
  m <- m / rowSums(m)                      # valid, but rows equal 1 only to floating point
  P <- array(m, c(4, 4, 50), dimnames = list(STATES, STATES, NULL))
  expect_silent(check_transition_array(P))
  tr <- run_cohort(P, c(1, 0, 0, 0))
  expect_equal(unname(rowSums(tr)), rep(1, 51), tolerance = 1e-12)
})

test_that("v1 bug 6: costs and QALYs are both discounted, at the CDA-AMC rate, with explicit cycles", {
  expect_equal(gp(BASE, "discount_rate"), 0.015)
  expect_equal(gp(BASE, "cycle_length"), 0.25)
  a0 <- run_group_arm(with_values(c(discount_rate = 0)), "higher_barrier")$summary
  a3 <- run_group_arm(with_values(c(discount_rate = 0.03)), "higher_barrier")$summary
  expect_lt(a3[["cost"]], a0[["cost"]])
  expect_lt(a3[["qaly"]], a0[["qaly"]])
})

test_that("v1 bug 7: the engine is generic - it runs models with any number of states", {
  for (S in c(2, 3, 6)) {
    m <- diag(S) * 0.5
    m[, S] <- m[, S] + 0.5
    m[S, ] <- c(rep(0, S - 1), 1)
    P <- array(m, c(S, S, 10), dimnames = list(paste0("s", 1:S), paste0("s", 1:S), NULL))
    tr <- run_cohort(P, c(1, rep(0, S - 1)))
    expect_equal(dim(tr), c(11, S))
    expect_equal(unname(tr[11, S]), 1 - 0.5^10)
  }
})

test_that("v1 bug 8: people move FROM rows TO columns (matrix orientation)", {
  P <- array(c(0.9, 0, 0.1, 1), c(2, 2, 1),
             dimnames = list(c("alive", "dead"), c("alive", "dead"), NULL))
  tr <- run_cohort(P, c(1, 0))
  expect_equal(unname(tr[2, ]), c(0.9, 0.1))   # the wrong orientation gives c(0.9, 0)
})
