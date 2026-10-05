# tests/testthat/test-07-regression.R ------------------------------------------
# Regression tests: results must match stored reference values. A failure means
# the model's numbers changed. If the change was intended, regenerate the
# references with tests/make_reference.R and record why in CHANGELOG.md.

test_that("base case matches the stored reference values", {
  ref <- utils::read.csv(file.path(ROOT, "tests", "reference", "base_case_reference.csv"))
  now <- run_base_case(BASE, STRATEGIES, GROUPS_DF)$group
  now <- now[match(paste(ref$strategy, ref$group), paste(now$strategy, now$group)), ]
  for (col in OUTCOMES) expect_equal(now[[col]], ref[[col]], tolerance = 1e-9)
})

test_that("the first PSA iterations match the stored reference (random streams reproduce)", {
  ref <- utils::read.csv(file.path(ROOT, "tests", "reference", "psa_reference.csv"))
  now <- run_psa(PAR_DF, STRATEGIES, GROUPS_DF, n = max(ref$iteration),
                 seed = module_seed("psa"))$strategy
  expect_equal(now$cost, ref$cost, tolerance = 1e-9)
  expect_equal(now$qaly, ref$qaly, tolerance = 1e-9)
})
