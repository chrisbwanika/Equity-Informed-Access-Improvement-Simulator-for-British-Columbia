# tests/testthat/test-03-engine.R ----------------------------------------------
# Known-answer tests: the engine must reproduce results that can be worked out
# by hand, before it is trusted with anything that cannot.

test_that("known answer: life expectancy in a two-state model matches the closed form", {
  p <- 0.1; n <- 40; dl <- 0.25; r <- 0.015
  P <- array(c(1 - p, 0, p, 1), c(2, 2, n),
             dimnames = list(c("alive", "dead"), c("alive", "dead"), NULL))
  tr <- run_cohort(P, c(1, 0))
  # Undiscounted, counted at the end of each cycle: dl * sum over t of (1 - p)^t
  expect_equal(accumulate(tr, c(dl, 0), dl, 0, "none"), dl * (1 - p) * (1 - (1 - p)^n) / p)
  # Discounted: a geometric series with ratio x = (1 - p) / (1 + r)^dl
  x <- (1 - p) / (1 + r)^dl
  expect_equal(accumulate(tr, c(dl, 0), dl, r, "none"), dl * x * (1 - x^n) / (1 - x))
})

test_that("trapezoid correction: a constant reward with no deaths counts exactly T cycles", {
  P <- array(c(1, 0, 0, 1), c(2, 2, 40), dimnames = list(c("a", "b"), c("a", "b"), NULL))
  tr <- run_cohort(P, c(1, 0))
  expect_equal(accumulate(tr, c(0.25, 0), 0.25, 0, "trapezoid"), 10)
  expect_equal(accumulate(tr, c(0.25, 0), 0.25, 0, "none"), 10)
})

test_that("invalid transition arrays are rejected", {
  expect_error(check_transition_array(array(c(0.9, 0, 0.11, 1), c(2, 2, 1))))   # row sums to 1.01
  expect_error(check_transition_array(array(c(1.1, 0, -0.1, 1), c(2, 2, 1))))   # negative entry
  expect_error(run_cohort(array(c(1, 0, 0, 1), c(2, 2, 1)), c(0.5, 0.6)))        # start not a distribution
})

test_that("discount weights: rate 0 gives ones, and weights fall over time", {
  expect_equal(discount_weights(8, 0.25, 0), rep(1, 9))
  w <- discount_weights(8, 0.25, 0.015)
  expect_true(all(diff(w) < 0))
  expect_equal(w[5], 1 / 1.015)          # boundary 4 = one year
})
