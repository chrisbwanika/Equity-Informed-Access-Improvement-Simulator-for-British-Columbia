# Verification record

Organised by the five TECH-VER domains (Büyükkaramikli et al., 2019). "Test" names refer to
files in `tests/testthat/`. Run `Rscript tests/run_tests.R` to reproduce; results are written
to `outputs/test_results.csv` and summarised in the report.

## 1. Input calculations
- Method-of-moments converters reproduce target means and SEs exactly (test-02).
- Every uncertain parameter's draws average to its base-case value; SDs match SEs (test-02).
- Gamma draws use shape and scale by name (test-01, v1 bug 1).
- Invalid inputs, duplicate rows and non-summing shares are rejected (test-02).
- PSA draws reproduce from the seed; fixed parameters never vary (test-02).

## 2. Event-state (patient flow) calculations
- Every transition array is valid for the base case and 200 PSA draws, all groups and arms:
  rows sum to 1 within 1e-10, entries in [0, 1], dead absorbing, tunnel state never repeats (test-04).
- Cohort conserved at every cycle; matrix orientation correct; engine generic in the number of
  states (test-01, bugs 5, 7, 8).
- Known-answer tests against closed-form life expectancy, with and without discounting (test-03).

## 3. Result calculations
- Null test: no effect and no cost gives identical strategies (test-04).
- Cost attachment: navigation cost appears once per navigated patient (test-01, bug 2).
- Direction and monotonicity: larger effects give larger gains (test-04).
- QALYs equal life-years when utilities are 1 (test-04).
- Uptake mixing is the exact weighted average of sub-cohorts (test-04).
- Incremental analysis reproduces a textbook dominance / extended-dominance case (test-04).
- DCEA identities: equity-weighted NHB equals NHB at zero aversion; opportunity cost equals
  incremental cost / threshold; EDE properties (test-05).
- Regression tests against stored reference values (test-07).

## 4. Uncertainty analysis calculations
- PSA reproduces the base case when all SEs shrink to ~0 (test-04).
- Convergence plot of the running mean incremental NMB (report).
- ECEA-lite microsimulation matches the cohort's expected cost within 4 Monte Carlo SEs (test-06).
- ITS and RDD recover planted effects; Monte Carlo calibration of interval coverage and
  placebo false-positive rates (report; `outputs/its_calibration.csv`, `outputs/rdd_calibration.csv`).

## 5. Other overall checks
- DES: identical results across strategies when no-show risk is zero (common random numbers),
  all referrals finish, no negative times, navigation shortens waits (test-06).
- Provenance manifest with MD5 checksums of every output (`outputs/run_manifest.csv`).
- Clean-machine reproduction: `renv::restore()`, `run_all.R`, tests, render.

## Independent review log
| Date | Reviewer | Scope | Findings | Resolved |
|---|---|---|---|---|
| | | | | |
