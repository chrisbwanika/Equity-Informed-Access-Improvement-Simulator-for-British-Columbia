# tests/run_tests.R ------------------------------------------------------------
# Runs the whole test suite. From the project root:
#   Rscript tests/run_tests.R      or, in RStudio:   source("tests/run_tests.R")
# Results are also written to outputs/test_results.csv for the report.
res <- testthat::test_dir(file.path("tests", "testthat"), reporter = "summary",
                          stop_on_failure = FALSE)
df <- as.data.frame(res)
dir.create("outputs", showWarnings = FALSE)
utils::write.csv(df[, c("file", "test", "nb", "passed", "failed", "skipped", "error", "warning")],
                 file.path("outputs", "test_results.csv"), row.names = FALSE)
n_bad <- sum(df$failed) + sum(df$error)
cat(sprintf("\n%d tests | %d expectations passed | %d failed or errored\n",
            nrow(df), sum(df$passed), n_bad))
if (n_bad > 0) stop("Some tests failed - fix them before rendering the report.", call. = FALSE)
