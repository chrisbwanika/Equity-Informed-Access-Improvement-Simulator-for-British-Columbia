# tests/testthat/test-02-parameters.R ------------------------------------------

test_that("method-of-moments converters reproduce the target mean and SE exactly", {
  b <- beta_params(0.3, 0.05)
  a1 <- b$shape1; a2 <- b$shape2
  expect_equal(a1 / (a1 + a2), 0.3)
  expect_equal(sqrt(a1 * a2 / ((a1 + a2)^2 * (a1 + a2 + 1))), 0.05)
  g <- gamma_params(750, 150)
  expect_equal(g$shape * g$scale, 750)
  expect_equal(sqrt(g$shape) * g$scale, 150)
  l <- lognormal_params(1.7, 0.2)
  expect_equal(exp(l$meanlog + l$sdlog^2 / 2), 1.7)
  expect_equal(sqrt((exp(l$sdlog^2) - 1) * exp(2 * l$meanlog + l$sdlog^2)), 0.2)
})

test_that("every uncertain parameter's draws average to its base-case value", {
  unc <- uncertain_parameters(PAR_DF)
  set.seed(3)
  for (i in seq_len(nrow(unc))) {
    x <- draw_parameter(20000, unc$distribution[i], unc$value[i], unc$se[i])
    expect_lt(abs(mean(x) - unc$value[i]), 5 * unc$se[i] / sqrt(20000))
    expect_equal(stats::sd(x), unc$se[i], tolerance = 0.05)
  }
})

test_that("invalid inputs are rejected with an error", {
  expect_error(beta_params(1.2, 0.1))
  expect_error(beta_params(0.5, 0.6))
  expect_error(gamma_params(-1, 1))
  expect_error(lognormal_params(1, 0))
  cols <- c("parameter", "group", "module", "value", "se", "distribution",
            "unit", "description", "source_type", "rationale")
  bad <- PAR_DF[, cols]
  bad$se[bad$parameter == "u_managed"] <- NA
  expect_error(validate_parameters(bad))
  expect_error(validate_parameters(rbind(PAR_DF[, cols], PAR_DF[1, cols])))
})

test_that("share columns sum to 1 and the first strategy is usual care", {
  expect_equal(sum(GROUPS_DF$pop_share), 1)
  expect_equal(sum(GROUPS_DF$cohort_share), 1)
  for (col in grep("^oc_", names(GROUPS_DF), value = TRUE)) expect_equal(sum(GROUPS_DF[[col]]), 1)
  expect_identical(STRATEGIES$strategy[1], "usual_care")
})

test_that("PSA draws are reproducible from the seed and fixed parameters never vary", {
  a <- draw_psa(PAR_DF, 5, seed = 99)
  b <- draw_psa(PAR_DF, 5, seed = 99)
  expect_identical(a, b)
  fixed <- PAR_DF$key[PAR_DF$distribution == "fixed"]
  expect_true(all(apply(a[, fixed, drop = FALSE], 2, function(x) length(unique(x)) == 1)))
})
