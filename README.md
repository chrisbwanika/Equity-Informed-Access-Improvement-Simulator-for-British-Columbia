# Equity-Informed Access Improvement Simulator for British Columbia

**Version 2.0.0 · synthetic-data capability demonstrator · R**

**Live report:** <https://chrisbwanika.github.io/Equity-Informed-Access-Improvement-Simulator-for-British-Columbia/>

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

1. **Get the code.** On the GitHub repository page, choose **Code → Download ZIP** and extract it
   (or run `git clone <repository URL>` if you use Git).
   The ZIP extracts into a folder inside a folder of the same name. **Open the inner folder**
   (the one that contains `run_all.R`). A short path such as `C:\projects\` is recommended, and
   avoid folders synchronised by OneDrive, Dropbox or Google Drive.
2. **Install R and RStudio Desktop.**
   The project was tested with R 4.6.1 (the minimum supported version is 4.3). Current versions
   of RStudio include Quarto; install Quarto separately only if you use another editor. Git is
   optional and only needed for version control.
3. **Open the project.** Double-click `equity-access-simulator-bc.Rproj` in RStudio. The first
   time, renv installs itself automatically and may report that packages are not installed.
 . **Restore the packages.** In the RStudio Console, run `renv::restore()` and answer **Y** when
   asked to proceed.
   This takes about 1 to 10 minutes (longer on a slow connection). On Windows you do not normally
   need Rtools; install it only if renv reports that a package must be compiled from source.
   Check the result with `renv::status()`, which should report no issues.
5. **Run everything.** `source("run_all.R")` (about 2 to 4 minutes). Run it from the project root,
   with the `.Rproj` open.
 . **Run the tests.** `source("tests/run_tests.R")` (about 1 to 2 minutes). All tests must pass.
   The final line should read:
   `40 tests | 3794 expectations passed | 0 failed or errored`
 . **Build the report.** In the RStudio **Terminal** (not the Console), run `quarto render`
   (under a minute). This creates `docs/index.html`. Run this only after `run_all.R` has finished,
   because the report reads the files in `outputs/`.

**Notes**

- **The rebuilt report differs slightly from the published one.** The "Published" date follows
  the file's last-modified time, and the run timestamps in the Reproducibility section are those
  of the machine that ran it. Numbers and figures are unchanged. Rendering also overwrites the
  tracked file `docs/index.html`.
- **Do not open or save the CSV files in Excel.** Excel changes how some values display (for
  example `3.0` appears as `3`) and can corrupt a saved file. Use a text editor such as VS Code.

## Troubleshooting

| Problem | Likely cause and fix |
|---|---|
| "The working directory must be the project root" | Open `equity-access-simulator-bc.Rproj` instead of opening files one by one. |
| "Missing packages ... Run renv::restore() first" | Run `renv::restore()`, answer **Y**, then `renv::status()`. |
| `quarto render` fails because files in `outputs/` are missing | Run `source("run_all.R")` first. |
| `renv::restore()` mentions Rtools or compiling | Install Rtools matching your R version (Windows only), restart R and try again. |
| Package or file-path errors with very long folder names | Move the project to a short path such as `C:\projects\`. |

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
| `renv.lock`, `renv/`, `.Rprofile` | Locked package versions and the renv set-up that restores them |
| `_dependencies.R` | Lists the packages so renv can detect them; never run directly |
| `CITATION.cff`, `CHANGELOG.md` | Citation metadata and the version history |
| `LICENSE`, `LICENSE-DOCS.md` | Licences for code and for documentation |

## Reproducibility and audit features

- One master seed with a separate random stream per module; the random-number algorithms are pinned.
- Package versions locked with `renv` (`renv.lock`).
- Every input lives in one register with its distribution, source type and rationale.
- Deterministic results are checked against stored reference values; a provenance manifest
  records software versions, settings and an MD5 checksum of every output.
- 40 automated tests, including one for each defect found in the earlier (v1) design.

## Authorship and AI assistance

See `methods/ai_assistance.md` for how AI tools were used and how their output was verified.

## Feedback
Corrections, questions and reviews are welcome: please open an issue.

## Licence and citation

Code: MIT licence (`LICENSE`). Documentation and report text: CC BY 4.0 (`LICENSE-DOCS.md`).
All data in this repository are synthetic. To cite this work, use `CITATION.cff`
(GitHub shows a "Cite this repository" button).
