# Health economic analysis plan (protocol)

Status: synthetic-data capability demonstrator. Version 2.0.0. Plan fixed on 2026-09-21,
before results were interpreted. Any later change goes in the deviations log below.

## Decision problem
Hypothetical question: for adults referred into a diagnostic or specialist pathway, should a
publicly funded health system offer patient navigation to everyone, only to patients facing
the highest structural barriers, or to no one?

## Intended use / not intended for
- Intended: demonstrating an auditable, reproducible workflow for equity-informed economic evaluation.
- Not intended: clinical guidance, funding decisions, operational planning, or any inference
  about real populations, programmes or organisations.

## Population and groups
Synthetic cohort aged 55 at referral, in three neutral structural-barrier groups (lower,
moderate, higher). The groups are analytic constructs, not proxies for any named community.

## Strategies
1. Usual care. 2. Targeted navigation (offered to the higher-barrier group only).
3. Universal navigation (offered to all groups).

## Perspective, horizon, discounting, currency
Publicly funded health-system perspective; household non-medical costs reported separately.
Lifetime horizon (age 55 to 100) in quarterly cycles. 1.5% per year for costs and QALYs,
with 0% and 3% as scenarios. Canadian dollars; synthetic values, so no price-year inflation.

## Outcomes
Costs, QALYs, life-years, acute events, time unresolved, share managed at one year; net
monetary benefit (NMB) and net health benefit (NHB); distribution of health by group;
household non-medical burden by group.

## Pre-specified analyses
1. Deterministic base case with full incremental analysis (dominance, extended dominance).
2. One-way sensitivity analysis: each uncertain input at its 2.5th and 97.5th PSA percentiles.
3. Scenario analyses: discount 0% and 3%; 10-year horizon; no within-cycle correction;
   navigation lasting two years; equal uptake; equal effect; navigation cost +50%; no mortality
   gradient; thresholds of CAD 30,000 and 100,000 per QALY.
4. PSA: 2,000 iterations; CE plane, CEAC, CEAF, EVPI, convergence.
5. DCEA: aggregate method; three opportunity-cost distribution rules; Atkinson
   (epsilon 0-15) and Kolm (alpha 0-0.25) equally distributed equivalent health; equity-efficiency
   plane; trade-off between targeted and universal navigation across epsilon.
6. ECEA-lite: first-year household non-medical cost; share above 2.5%, 5% and 10% of income.
7. Demonstrations: ITS (segmented GLS with AR(1) errors, REML, placebo, exclusion
   sensitivity, 400-dataset calibration); RDD (sharp and fuzzy rdrobust, density, balance,
   placebo cutoffs, bandwidth and donut sensitivity, 100-dataset calibration); DES
   (20 replications per strategy, common random numbers).

## Decision rule
The strategy with the highest expected NMB at the chosen threshold; ICERs reported along the
efficiency frontier. Equity results are reported alongside efficiency results, never folded
into one number without showing the inequality-aversion range.

## Deviations log
| Date | Change | Reason |
|---|---|---|
| 2026-09-21 | ITS estimation changed from ML to REML | Calibration showed ML intervals were over-confident |
| 2026-09-21 | RDD design given two-sided non-compliance | Realism; avoids a development-version rdrobust failure |
