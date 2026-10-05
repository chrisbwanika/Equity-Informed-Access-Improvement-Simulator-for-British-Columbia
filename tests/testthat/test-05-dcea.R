# tests/testthat/test-05-dcea.R ------------------------------------------------

test_that("EDE equals the mean when there is no inequality aversion", {
  h <- c(72, 67, 61); w <- c(0.5, 0.3, 0.2)
  expect_equal(ede_atkinson(h, w, 0), sum(w * h))
  expect_equal(ede_kolm(h, w, 0), sum(w * h))
})

test_that("EDE equals the common value for an equal distribution, at any aversion", {
  for (e in c(0.5, 1, 3, 10)) expect_equal(ede_atkinson(rep(65, 3), c(0.2, 0.3, 0.5), e), 65)
  for (a in c(0.05, 0.5, 5)) expect_equal(ede_kolm(rep(65, 3), c(0.2, 0.3, 0.5), a), 65)
})

test_that("EDE falls as aversion rises; epsilon = 1 is the geometric mean; Kolm is stable", {
  h <- c(72, 67, 61); w <- c(0.5, 0.3, 0.2)
  e <- sapply(c(0, 1, 2, 5, 10), function(x) ede_atkinson(h, w, x))
  expect_true(all(diff(e) < 0))
  expect_equal(ede_atkinson(h, w, 1), exp(sum(w * log(h))))
  k <- sapply(c(0, 0.05, 0.2, 1), function(x) ede_kolm(h, w, x))
  expect_true(all(diff(k) < 0))
  expect_true(is.finite(ede_kolm(h, w, 50)))
})

test_that("identity: equity-weighted NHB with zero aversion equals total net health benefit", {
  b <- run_base_case(BASE, STRATEGIES, GROUPS_DF)
  m <- run_dcea_deterministic(b$group, GROUPS_DF, BASE, STRATEGIES)$metrics
  expect_equal(m$ew_nhb_atkinson_0, m$total_nhb, tolerance = 1e-6)
  expect_equal(m$ew_nhb_kolm_0, m$total_nhb, tolerance = 1e-6)
})

test_that("health opportunity cost equals total incremental cost divided by the threshold", {
  b <- run_base_case(BASE, STRATEGIES, GROUPS_DF)
  m <- run_dcea_deterministic(b$group, GROUPS_DF, BASE, STRATEGIES)$metrics
  v <- b$vs_usual_care
  for (s in v$strategy) {
    expected <- v$inc_cost[v$strategy == s] * gp(BASE, "n_cohort") / gp(BASE, "k_threshold")
    expect_equal(m$total_opportunity_cost[m$strategy == s], rep(expected, 3))
  }
})
