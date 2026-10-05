# Equity-Informed Access Improvement Simulator for British Columbia

**Version 2.0.0 · synthetic-data capability demonstrator · R**

> **Disclaimer.** All data, inputs and results in this repository are simulated to
> illustrate methods. This is not an official analysis, recommendation, endorsement or
> evaluation by any government, health authority, agency or community. No result
> describes a real population or programme.

## What this is

A fully reproducible prototype of an equity-informed health economic evaluation. It asks a
hypothetical question: for adults referred into a diagnostic or specialist pathway, should
patient navigation be offered to everyone, only to patients facing the highest structural
barriers, or to no one?

**Core (decision-facing):** an equity-stratified cohort state-transition (Markov) model with
deterministic, one-way, scenario and probabilistic sensitivity analyses; distributional
cost-effectiveness analysis (DCEA); and a household non-medical financial-burden extension
(ECEA-lite).

**Demonstration modules:** interrupted time series (ITS) and regression discontinuity (RDD)
analyses of synthetic data with known planted effects, and a discrete event simulation (DES)
of clinic capacity and missed appointments.

## Quick start

1. Install R, RStudio Desktop and Quarto (and Git if you will use version control).
2. Open `equity-access-simulator-bc.Rproj` in RStudio.
3. In the RStudio Console: `renv::restore()` (first time on a new machine).
4. Run everything: `source("run_all.R")` (about 2 minutes).
5. Run the tests: `source("tests/run_tests.R")` (all must pass).
6. Build the report in the RStudio **Terminal**: `quarto render` → `docs/index.html`.

## Repository map

| Path | Contents |
|---|---|
| `data/` | Input registers: `parameters.csv` (every input, distribution and rationale), `groups.csv`, `strategies.csv` |
| `R/` | Functions only, numbered in reading order: settings, parameters, generic engine, model, analyses, figures |
| `run_all.R` | Single entry point that reproduces every output |
| `tests/` | `run_tests.R`, the test suite in `testthat/`, reference values in `reference/` |
| `index.qmd`, `_quarto.yml` | The technical report (reads saved outputs) |
| `methods/` | Analysis plan, model specification, verification record, AI-assistance statement |
| `outputs/` | Generated tables, figures and run manifest (not committed; recreated by `run_all.R`) |
| `docs/` | Rendered report, served by GitHub Pages |

## Reproducibility and audit features

- One master seed with a separate random stream per module; the random-number algorithms are pinned.
- Package versions locked with `renv` (`renv.lock`).
- Every input lives in one register with its distribution, source type and rationale.
- Deterministic results are checked against stored reference values; a provenance manifest
  records software versions, settings and an MD5 checksum of every output.
- 40 automated tests, including one for each defect found in the earlier (v1) design.

## Authorship and AI assistance

See `methods/ai_assistance.md` for how AI tools were used and how their output was verified.

## Licence and citation

- Code (everything in `R/`, `tests/`, and the run scripts): MIT Licence.
  See `LICENSE`.
- Documentation and report (README, `methods/`, and the rendered report):
  Creative Commons Attribution 4.0 International (CC BY 4.0).
  See `LICENSE-DOCS.md`.
- Data: all data in this repository are synthetic. No real or personal data
  were used. The synthetic data carry no restrictions beyond the licences above.
- Dependencies: R packages are not bundled. `renv.lock` records the versions
  used, and each package remains under its own licence.

## How to cite

Bwanika, C. (2026). *Equity-Informed Access Improvement Simulator* (Version 2)
[Computer software]. [repository URL]
Code: MIT licence (`LICENSE`). Documentation and report text: CC BY 4.0. To cite, use
`CITATION.cff`.
