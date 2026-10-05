# Changelog

All notable changes are recorded here. Version numbers follow semantic versioning
(MAJOR.MINOR.PATCH). Any change to inputs, seeds or reference values must be logged.

## [2.0.0] - 2026-09-21

Complete redesign following a review of the v1 blueprint.

### Fixed (v1 defects, each now covered by a test in `tests/testthat/test-01-v1-acceptance.R`)
- Gamma draws used positional arguments, so a scale was read as a rate. All sampling now
  names its arguments, and PSA means are tested against base-case values.
- Cost attachment was undefined. Costs are now attached to states per cycle, and the
  navigation cost is charged once, at time 0, per navigated patient.
- Groups differed only by an equity weight, so the equity result was fixed by design.
  Groups now differ in access, risk, continuity, mortality, uptake and effect.
- Equity weights were combined with an Atkinson index applied to net health benefit
  (double counting, and undefined for negative values). DCEA now follows the aggregate
  approach: baseline health distribution, health gains, health opportunity costs shared by
  an explicit rule, and Atkinson/Kolm EDE applied to health levels across a range of aversion.
- Exact-equality tests failed correct models. Checks now use numerical tolerances and
  conservation is checked at every cycle.
- Canadian reference-case elements were missing. Added: 1.5% discounting of costs and QALYs,
  explicit quarterly cycles, lifetime horizon, within-cycle correction, deterministic base
  case, CE plane, CEAC/CEAF and EVPI.
- The engine was hard-coded to three states. It is now generic (any number of states,
  time-varying transitions), with matrix orientation tested.

### Added
- Three strategies (usual care, targeted, universal) with full incremental analysis.
- ECEA-lite microsimulation of household non-medical burden, cross-checked against the cohort model.
- ITS, RDD and DES demonstration modules with planted-effect recovery and Monte Carlo calibration.
- Provenance manifest, regression tests, Quarto report, CHEERS 2022 mapping.

### Found and fixed while building v2
- ITS intervals from maximum likelihood were over-confident in short series (coverage about
  90-92%, placebo false positives 8-13.5%). Switched to REML: coverage 93-94%, false positives 5-7%.
- The development build of rdrobust failed on fuzzy designs with one-sided non-compliance;
  the synthetic design now has two-sided non-compliance (more realistic in any case).
- Seed arithmetic could overflow R's integer range and module streams could overlap;
  module seed offsets are now 1,000 apart.
