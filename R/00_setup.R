# R/00_setup.R ----------------------------------------------------------------
# Project-wide settings. Every other script takes its settings from here.
# Change a setting ONLY in this file, and record the change in CHANGELOG.md.
# This file defines objects and functions only; it does not run any analysis.

SETTINGS <- list(
  project_version  = "2.0.0",
  master_seed      = 20260921L,  # every random number in the project flows from this
  n_psa            = 2000L,      # PSA iterations (set to 200 while debugging)
  n_ecea_micro     = 20000L,     # simulated patients per group (ECEA-lite)
  des_replications = 20L,        # DES replications per strategy
  its_coverage_reps = 400L,      # ITS Monte Carlo calibration datasets (0 = skip)
  rdd_coverage_reps = 100L,      # RDD Monte Carlo calibration datasets (0 = skip)
  wcc_method       = "trapezoid",# within-cycle correction: "trapezoid" or "none"
  oc_base_scenario = "oc_proportional", # opportunity-cost distribution used in the base case
  wtp_grid         = seq(0, 150000, by = 2500),        # CEAC thresholds (CAD per QALY)
  epsilon_grid     = c(0, 1, 2, 3, 5, 7, 10, 15),      # Atkinson inequality aversion
  alpha_grid       = c(0, 0.025, 0.05, 0.1, 0.15, 0.25), # Kolm inequality aversion
  paths = list(
    data      = "data",
    outputs   = "outputs",
    figures   = file.path("outputs", "figures"),
    generated = file.path("outputs", "generated_data")
  )
)

# One fixed offset per module, so each module has its own reproducible random
# stream: changing or re-running one module never shifts another module's numbers.
# Offsets are 1000 apart so that seeds derived inside a module (seed + group
# index, seed + replication number) can never collide with another module's.
MODULE_SEED_OFFSETS <- c(psa = 1000L, ecea = 2000L, its = 3000L, rdd = 4000L, des = 5000L)

module_seed <- function(module) {
  if (!module %in% names(MODULE_SEED_OFFSETS)) {
    stop("Unknown module '", module, "'.", call. = FALSE)
  }
  SETTINGS$master_seed + MODULE_SEED_OFFSETS[[module]]
}

# Pin the random-number algorithms explicitly (these are R's defaults since R 3.6.0).
# Writing them down protects reproducibility if a default ever changes.
RNGkind(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")

REQUIRED_PACKAGES <- c("ggplot2", "nlme", "rdrobust", "rddensity", "simmer",
                       "testthat", "knitr", "rmarkdown")

check_packages <- function(pkgs = REQUIRED_PACKAGES) {
  ok <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  if (!all(ok)) {
    stop("Missing packages: ", paste(pkgs[!ok], collapse = ", "),
         ". Run renv::restore() first (see README).", call. = FALSE)
  }
  invisible(TRUE)
}

assert_project_root <- function() {
  if (!file.exists("run_all.R") || !dir.exists("R") || !dir.exists("data")) {
    stop("The working directory must be the project root (the folder that ",
         "contains run_all.R). In RStudio, open equity-access-simulator-bc.Rproj.",
         call. = FALSE)
  }
  invisible(TRUE)
}

ensure_output_dirs <- function() {
  for (p in SETTINGS$paths[c("outputs", "figures", "generated")]) {
    dir.create(p, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(TRUE)
}

write_output <- function(df, name) {
  path <- file.path(SETTINGS$paths$outputs, name)
  utils::write.csv(df, path, row.names = FALSE)
  invisible(path)
}

save_figure <- function(plot, name, width = 8, height = 5) {
  path <- file.path(SETTINGS$paths$figures, name)
  ggplot2::ggsave(path, plot, width = width, height = height, dpi = 200, bg = "white")
  invisible(path)
}

# Provenance record written at the end of every run: software versions, settings,
# seeds, timings, and an MD5 checksum of every output file. If two runs give
# identical checksums, they produced byte-identical results.
write_run_manifest <- function(start_time) {
  out_dir <- SETTINGS$paths$outputs
  files <- sort(list.files(out_dir, recursive = TRUE, full.names = TRUE))
  files <- files[basename(files) != "run_manifest.csv"]
  pkg_versions <- vapply(REQUIRED_PACKAGES, function(p) as.character(utils::packageVersion(p)), "")
  info <- c(project_version = SETTINGS$project_version,
            run_started = format(start_time, "%Y-%m-%d %H:%M:%S %Z"),
            run_finished = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
            minutes = sprintf("%.1f", as.numeric(difftime(Sys.time(), start_time, units = "mins"))),
            r_version = R.version.string, platform = R.version$platform,
            master_seed = SETTINGS$master_seed, n_psa = SETTINGS$n_psa,
            n_ecea_micro = SETTINGS$n_ecea_micro, des_replications = SETTINGS$des_replications,
            its_coverage_reps = SETTINGS$its_coverage_reps,
            rdd_coverage_reps = SETTINGS$rdd_coverage_reps, wcc_method = SETTINGS$wcc_method)
  manifest <- rbind(
    data.frame(section = "run", item = names(info), value = unname(info)),
    data.frame(section = "package", item = names(pkg_versions), value = unname(pkg_versions)),
    data.frame(section = "file_md5", item = sub(paste0("^", out_dir, "/"), "", files),
               value = unname(tools::md5sum(files))))
  write_output(manifest, "run_manifest.csv")
}
