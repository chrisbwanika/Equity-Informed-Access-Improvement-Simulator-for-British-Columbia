# Model specification

Enough detail for someone to re-implement the model independently (parallel programming is
the strongest verification test). Notation: t = cycle (1..T), Δ = cycle length in years
(0.25), g = barrier group.

## 1. States and start
States: unresolved (U), managed (M), acute event (A, one-cycle tunnel), dead (D).
Everyone starts in U. T = horizon_years / Δ = 45 / 0.25 = 180 cycles.

## 2. Per-cycle probabilities (all from data/parameters.csv)
- Background death: age a_t = 55 + (t − 1)Δ; annual hazard h_t = gompertz_a × exp(gompertz_b × a_t)
  × hr_bg_mortality_g; per-cycle probability q_bg,t = 1 − exp(−h_t Δ).
- Pathway completion: q_m = p_complete_g. For navigated patients while t ≤ nav_cycles:
  q_m = 1 − (1 − p_complete_g)^hr_navigation_g (proportional hazards within the cycle).
- Acute event: q_aU = p_acute_unresolved_g; q_aM = 1 − (1 − q_aU)^hr_acute_managed.
- Disengagement: q_dis = p_disengage_g. Case fatality: q_cf. Entry to care after an event: q_pm.

## 3. Transition rows (sequential conditional probabilities; each row sums to 1)
- From U: D = q_bg; A = (1 − q_bg)q_aU; M = (1 − q_bg)(1 − q_aU)q_m; U = (1 − q_bg)(1 − q_aU)(1 − q_m).
- From M: D = q_bg; A = (1 − q_bg)q_aM; U = (1 − q_bg)(1 − q_aM)q_dis; M = (1 − q_bg)(1 − q_aM)(1 − q_dis).
- From A: D = 1 − (1 − q_bg)(1 − q_cf); M = (1 − D)q_pm; U = (1 − D)(1 − q_pm); A = 0.
- From D: D = 1.

## 4. Trace, rewards, discounting, within-cycle correction
- Trace: m_0 = (1, 0, 0, 0); m_t = m_(t−1) P_t (row vector times matrix).
- Per-cycle values: costs c_U, c_M, c_A, 0; QALYs u_s Δ with u_M = u_managed,
  u_U = u_managed − dec_unresolved, u_A = u_managed − dec_acute; life-years Δ for alive states.
- Total = Σ_(t=0..T) w_t d_t (m_t · v), with d_t = (1 + r)^(−tΔ) and trapezoid weights
  w = (½, 1, …, 1, ½). Navigation cost is added once at t = 0 per navigated patient.
- Acute events = Σ_(t=1..T) m_t[A]. Time unresolved = undiscounted trapezoid sum of m_t[U]Δ.

## 5. Strategies and uptake
For an offered group with uptake u: outcome = u × outcome(navigated) + (1 − u) × outcome(usual),
computed from two separately simulated sub-cohorts (exact). Strategy results are
cohort-share-weighted averages across groups.

## 6. Decision rules
NMB = QALYs × k − cost. Incremental analysis: sort by cost; remove strongly dominated, then
extendedly dominated strategies; ICERs along the frontier.

## 7. DCEA
For strategy s versus usual care: gain_g = ΔQALY_g × n_cohort × cohort_share_g;
opportunity cost OC_g = (Σ ΔCost × patients / k) × oc_share_g; net_g = gain_g − OC_g;
post-intervention QALE_g = baseline_qale_g + net_g / (n_population × pop_share_g).
Atkinson EDE_ε = (Σ w_g h_g^(1−ε))^(1/(1−ε)) (geometric mean when ε = 1);
Kolm EDE_α = −(1/α) ln Σ w_g exp(−α h_g). Equity-weighted NHB = ΔEDE × n_population,
which equals total NHB when ε = 0 or α = 0 (tested identity).

## 8. ECEA-lite
Individuals are simulated for four quarters through the same P_t. Household cost is the sum of
the group-specific per-quarter cost of the state occupied at the end of each quarter. Income is
log-normal (median by group, sdlog 0.6). High burden: cost / income above a threshold.

## 9. ITS (synthetic)
Monthly 90-day completion, Jan 2021 to Dec 2025, launch month 37. Data: logit p_t = logit(p_uc)
+ trend + season + level × post + AR(1) noise; completions ~ Binomial(referrals, p_t).
Model: empirical logit ~ month + post + time_after + sin + cos, GLS with AR(1) errors (REML).
Bridge: invert the model's first-quarter completion equations to map the ITS level change to
a per-navigated-patient hazard ratio.

## 10. RDD (synthetic)
Score ~ truncated normal on [0, 100]; offer if score ≥ 60; navigated with probability 0.70 if
offered and 0.05 otherwise. Sharp estimand (offer) = 0.12 × 0.65 = 0.078; fuzzy estimand
(navigation, compliers at the cutoff) = 0.12.

## 11. DES
First-come-first-served queue for 12 slots; a booked slot is occupied for one day whether or
not the patient attends; after a no-show the patient waits 7 days and re-queues. All random
inputs are pre-generated and shared across strategies within a replication.
