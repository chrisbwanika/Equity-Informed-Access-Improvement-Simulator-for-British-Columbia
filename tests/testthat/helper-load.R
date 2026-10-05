# tests/testthat/helper-load.R -------------------------------------------------
# testthat runs files named helper*.R before any test. This one finds the project
# root (the folder containing run_all.R), loads every function file in R/, and
# reads the input registers once so every test uses the same inputs.

find_project_root <- function(path = getwd()) {
  path <- normalizePath(path, winslash = "/")
  for (i in 1:10) {
    if (file.exists(file.path(path, "run_all.R"))) return(path)
    path <- dirname(path)
  }
  stop("Could not find the project root (the folder that contains run_all.R).")
}

ROOT <- find_project_root()
for (f in sort(list.files(file.path(ROOT, "R"), pattern = "\\.R$", full.names = TRUE))) source(f)

PAR_DF     <- read_parameters(file.path(ROOT, "data", "parameters.csv"))
GROUPS_DF  <- read_groups(file.path(ROOT, "data", "groups.csv"))
STRATEGIES <- read_strategies(file.path(ROOT, "data", "strategies.csv"))
BASE       <- base_case_values(PAR_DF)

# Convenience: a copy of the base case with some values changed.
with_values <- function(changes, pars = BASE) {
  stopifnot(all(names(changes) %in% names(pars)))
  pars[names(changes)] <- changes
  pars
}
