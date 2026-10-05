# tests/testthat/test-04-model.R -----------------------------------------------

test_that("every transition array is valid: base case and 200 PSA draws, all groups and arms", {
  draws <- draw_psa(PAR_DF, 200, seed = 11)
  for (i in 0:200) {
    p <- if (i == 0) BASE else draws[i, ]
    for (g in GROUPS) for (nav in c(FALSE, TRUE)) {
      P <- build_transition_array(p, g, nav)
      expect_silent(check_transition_array(P))
      expect_true(all(P["dead", "dead", ] == 1))
      expect_true(all(P["acute", "acute", ] == 0))     # tunnel state: never stay
    }
  }
})

test_that("null test: no effect and no cost means every strategy equals usual care", {
  p <- with_values(c(`hr_navigation@lower_barrier` = 1, `hr_navigation@moderate_barrier` = 1,
                     `hr_navigation@higher_barrier` = 1, c_navigation = 0))
  s <- evaluate_strategies(p, STRATEGIES, GROUPS_DF)$strategy
  for (col in OUTCOMES) expect_equal(s[[col]], rep(s[[col]][1], 3), tolerance = 1e-12)
})

test_that("navigation with HR > 1 raises QALYs and cuts acute events in every group", {
  arms <- evaluate_group_arms(BASE)
  for (g in GROUPS) {
    expect_gt(arms[[g]]["navigated", "qaly"], arms[[g]]["usual", "qaly"])
    expect_lt(arms[[g]]["navigated", "acute_events"], arms[[g]]["usual", "acute_events"])
    expect_gt(arms[[g]]["navigated", "managed_1y"], arms[[g]]["usual", "managed_1y"])
  }
})

test_that("a larger navigation effect gives a larger QALY gain (monotonicity)", {
  gain <- sapply(c(1.2, 1.7, 2.2), function(hr) {
    a <- evaluate_group_arms(with_values(c(`hr_navigation@higher_barrier` = hr)))$higher_barrier
    a["navigated", "qaly"] - a["usual", "qaly"]
  })
  expect_true(all(diff(gain) > 0))
})

test_that("QALYs equal life-years when every alive state has utility 1", {
  p <- with_values(c(u_managed = 1, dec_unresolved = 0, dec_acute = 0))
  a <- run_group_arm(p, "moderate_barrier")$summary
  expect_equal(a[["qaly"]], a[["ly"]])
})

test_that("hazard-ratio scaling keeps probabilities valid and composes correctly", {
  p <- c(0.001, 0.2, 0.9)
  expect_equal(scale_prob_by_hr(p, 1), p)
  expect_true(all(scale_prob_by_hr(p, 5) < 1 & scale_prob_by_hr(p, 0.2) > 0))
  expect_equal(scale_prob_by_hr(scale_prob_by_hr(p, 1.5), 2), scale_prob_by_hr(p, 3))
})

test_that("an offered group is the exact uptake-weighted mix of its two sub-cohorts", {
  arms <- evaluate_group_arms(BASE)
  r <- evaluate_strategies(BASE, STRATEGIES, GROUPS_DF, arms = arms)$group
  u <- gp(BASE, "uptake", "higher_barrier")
  got <- unlist(r[r$strategy == "targeted_navigation" & r$group == "higher_barrier", OUTCOMES])
  want <- u * arms$higher_barrier["navigated", OUTCOMES] + (1 - u) * arms$higher_barrier["usual", OUTCOMES]
  expect_equal(unname(got), unname(want))
})

test_that("incremental analysis handles strong and extended dominance (textbook case)", {
  ia <- incremental_analysis(c("A", "B", "C", "E"), cost = c(0, 100, 200, 300),
                             effect = c(0, 0.5, 1.5, 1.0))
  expect_equal(ia$status[ia$strategy == "E"], "dominated")
  expect_equal(ia$status[ia$strategy == "B"], "extendedly dominated")
  expect_equal(ia$icer[ia$strategy == "C"], 200 / 1.5)
})

test_that("the PSA machinery reproduces the base case when uncertainty shrinks to ~0", {
  tiny <- PAR_DF
  tiny$se <- ifelse(tiny$distribution == "fixed", tiny$se, tiny$se * 1e-6)
  psa <- run_psa(tiny, STRATEGIES, GROUPS_DF, n = 3, seed = 5)
  base <- evaluate_strategies(BASE, STRATEGIES, GROUPS_DF)$strategy
  for (i in 1:3) {
    got <- psa$strategy[psa$strategy$iteration == i, ]
    expect_equal(got$cost, base$cost, tolerance = 1e-5)
    expect_equal(got$qaly, base$qaly, tolerance = 1e-5)
  }
})
