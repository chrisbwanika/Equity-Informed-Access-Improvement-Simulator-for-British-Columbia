# R/01_parameters.R ------------------------------------------------------------
# Reads and validates the input registers, converts (mean, SE) into distribution
# parameters, and draws PSA samples. Every model input comes through here.
#
# Convention used everywhere: `value` is the MEAN of the parameter's distribution,
# and `se` is its standard error. The deterministic base case uses `value`, so the
# PSA mean of every parameter equals its base-case value (tested).

GROUPS <- c("lower_barrier", "moderate_barrier", "higher_barrier")
ALLOWED_DISTRIBUTIONS <- c("fixed", "beta", "gamma", "lognormal")
ALLOWED_MODULES <- c("markov", "ecea", "its", "rdd", "des")

param_key <- function(parameter, group) {
  ifelse(group == "all", parameter, paste0(parameter, "@", group))
}

# ---- Method-of-moments conversions (each one is unit-tested) -----------------
beta_params <- function(mean, se) {
  if (any(mean <= 0 | mean >= 1)) stop("Beta mean must be strictly between 0 and 1.")
  if (any(se <= 0)) stop("Beta SE must be positive.")
  if (any(se^2 >= mean * (1 - mean))) stop("Beta SE too large for its mean.")
  common <- mean * (1 - mean) / se^2 - 1
  list(shape1 = mean * common, shape2 = (1 - mean) * common)
}

gamma_params <- function(mean, se) {
  if (any(mean <= 0 | se <= 0)) stop("Gamma mean and SE must be positive.")
  list(shape = (mean / se)^2, scale = se^2 / mean)  # mean = shape * scale
}

lognormal_params <- function(mean, se) {
  if (any(mean <= 0 | se <= 0)) stop("Log-normal mean and SE must be positive.")
  sdlog <- sqrt(log(1 + (se / mean)^2))
  list(meanlog = log(mean) - sdlog^2 / 2, sdlog = sdlog)  # preserves the arithmetic mean
}

# Draw n values. Every argument is NAMED: rgamma(1, 25, 200) would silently read
# 200 as a rate, which is exactly the bug found in the v1 blueprint.
draw_parameter <- function(n, distribution, value, se) {
  switch(distribution,
    fixed = rep(value, n),
    beta = {
      b <- beta_params(value, se)
      stats::rbeta(n = n, shape1 = b$shape1, shape2 = b$shape2)
    },
    gamma = {
      g <- gamma_params(value, se)
      stats::rgamma(n = n, shape = g$shape, scale = g$scale)
    },
    lognormal = {
      l <- lognormal_params(value, se)
      stats::rlnorm(n = n, meanlog = l$meanlog, sdlog = l$sdlog)
    },
    stop("Unknown distribution: ", distribution)
  )
}

quantile_parameter <- function(p, distribution, value, se) {
  switch(distribution,
    fixed = rep(value, length(p)),
    beta = { b <- beta_params(value, se)
             stats::qbeta(p, shape1 = b$shape1, shape2 = b$shape2) },
    gamma = { g <- gamma_params(value, se)
              stats::qgamma(p, shape = g$shape, scale = g$scale) },
    lognormal = { l <- lognormal_params(value, se)
                  stats::qlnorm(p, meanlog = l$meanlog, sdlog = l$sdlog) },
    stop("Unknown distribution: ", distribution)
  )
}

# ---- Reading and validation ----------------------------------------------------
read_parameters <- function(path = file.path("data", "parameters.csv")) {
  df <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  validate_parameters(df)
  df$key <- param_key(df$parameter, df$group)
  df
}

validate_parameters <- function(df) {
  required <- c("parameter", "group", "module", "value", "se", "distribution",
                "unit", "description", "source_type", "rationale")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) stop("parameters.csv is missing columns: ",
                                paste(missing, collapse = ", "))
  if (anyDuplicated(paste(df$parameter, df$group))) stop("Duplicate parameter/group rows.")
  if (!all(df$group %in% c("all", GROUPS))) stop("Unknown group in parameters.csv.")
  if (!all(df$module %in% ALLOWED_MODULES)) stop("Unknown module in parameters.csv.")
  if (!all(df$distribution %in% ALLOWED_DISTRIBUTIONS)) stop("Unknown distribution.")
  if (any(!is.finite(df$value))) stop("Every parameter needs a finite value.")
  uncertain <- df$distribution != "fixed"
  if (any(!is.finite(df$se[uncertain]) | df$se[uncertain] <= 0)) {
    stop("Every uncertain parameter needs a positive SE: ",
         paste(df$parameter[uncertain & !(is.finite(df$se) & df$se > 0)], collapse = ", "))
  }
  for (i in which(uncertain)) {  # triggers the checks inside the converters
    switch(df$distribution[i],
           beta = beta_params(df$value[i], df$se[i]),
           gamma = gamma_params(df$value[i], df$se[i]),
           lognormal = lognormal_params(df$value[i], df$se[i]))
  }
  invisible(TRUE)
}

read_groups <- function(path = file.path("data", "groups.csv")) {
  g <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (!identical(g$group, GROUPS)) stop("groups.csv must list the groups in the order: ",
                                        paste(GROUPS, collapse = ", "))
  share_cols <- c("pop_share", "cohort_share", grep("^oc_", names(g), value = TRUE))
  for (col in share_cols) {
    if (abs(sum(g[[col]]) - 1) > 1e-9) stop("Column ", col, " in groups.csv must sum to 1.")
    if (any(g[[col]] < 0)) stop("Column ", col, " has negative shares.")
  }
  if (any(g$baseline_qale <= 0)) stop("Baseline QALE must be positive.")
  g
}

read_strategies <- function(path = file.path("data", "strategies.csv")) {
  s <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (!all(GROUPS %in% names(s))) stop("strategies.csv needs one column per group.")
  if (!all(unlist(s[GROUPS]) %in% c(0, 1))) stop("Offer flags must be 0 or 1.")
  if (s$strategy[1] != "usual_care" || any(unlist(s[1, GROUPS]) != 0)) {
    stop("The first strategy must be usual_care with no offers.")
  }
  s
}

# ---- Parameter sets --------------------------------------------------------------
# A parameter set is a named numeric vector; names are keys such as
# "discount_rate" or "p_complete@higher_barrier".
base_case_values <- function(df) stats::setNames(df$value, df$key)

gp <- function(pars, name, group = "all") {
  key <- if (group == "all") name else paste0(name, "@", group)
  if (!key %in% names(pars)) stop("Parameter not found: ", key, call. = FALSE)
  pars[[key]]
}

draw_psa <- function(df, n, seed, modules = "markov") {
  set.seed(seed)
  draws <- matrix(rep(df$value, each = n), nrow = n, dimnames = list(NULL, df$key))
  for (i in which(df$module %in% modules & df$distribution != "fixed")) {
    draws[, i] <- draw_parameter(n, df$distribution[i], df$value[i], df$se[i])
  }
  draws
}

uncertain_parameters <- function(df, modules = "markov") {
  df[df$module %in% modules & df$distribution != "fixed", , drop = FALSE]
}
