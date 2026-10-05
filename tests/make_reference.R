# tests/make_reference.R -------------------------------------------------------
# Regenerates the regression-test reference values. Run it ONLY after an
# intended change to the model or its inputs, and record the reason in
# CHANGELOG.md. From the project root:  Rscript tests/make_reference.R
for (f in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) source(f)
assert_project_root()
par_df <- read_parameters(); groups_df <- read_groups(); strategies <- read_strategies()
base <- run_base_case(base_case_values(par_df), strategies, groups_df)
utils::write.csv(base$group[, c("strategy", "group", OUTCOMES)],
                 file.path("tests", "reference", "base_case_reference.csv"), row.names = FALSE)
# Note: draw_psa() draws each parameter's n values in one block, so the reference
# is a 5-iteration PSA, not the first 5 iterations of the 2,000-iteration run.
psa <- run_psa(par_df, strategies, groups_df, n = 5, seed = module_seed("psa"))
utils::write.csv(psa$strategy[, c("iteration", "strategy", "cost", "qaly")],
                 file.path("tests", "reference", "psa_reference.csv"), row.names = FALSE)
message("Reference values rewritten. Record the reason in CHANGELOG.md.")
