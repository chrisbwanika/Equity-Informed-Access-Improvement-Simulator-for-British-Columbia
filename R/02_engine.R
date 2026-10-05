# R/02_engine.R ----------------------------------------------------------------
# A generic cohort state-transition engine. It knows nothing about navigation,
# barrier groups or costs: it accepts ANY number of states and ANY time-varying
# transition array, so it can be reused for a different decision problem by
# writing a new model file (see R/03_model_navigation.R) and leaving this untouched.
#
# Conventions
#   P      : array [from state, to state, cycle], dimension S x S x T.
#   trace  : matrix (T + 1) x S. Row 1 is time 0 (the start); row t + 1 is the
#            cohort distribution after t cycles ("boundary t").
#   values : per-cycle value of occupying each state (a cost per cycle, or
#            utility x cycle length for QALYs). A length-S vector, or a
#            (T + 1) x S matrix when values change over time.

check_transition_array <- function(P, tol = 1e-10) {
  d <- dim(P)
  if (length(d) != 3 || d[1] != d[2]) stop("P must be an S x S x T array.")
  if (any(!is.finite(P))) stop("P contains missing or infinite values.")
  if (any(P < -tol | P > 1 + tol)) stop("P contains probabilities outside [0, 1].")
  row_sums <- apply(P, c(1, 3), sum)
  if (any(abs(row_sums - 1) > tol)) {
    bad <- which(abs(row_sums - 1) > tol, arr.ind = TRUE)[1, ]
    names_from <- if (is.null(dimnames(P)[[1]])) seq_len(d[1]) else dimnames(P)[[1]]
    stop(sprintf("Row '%s' in cycle %d sums to %.12f, not 1.",
                 names_from[bad[1]], bad[2], row_sums[bad[1], bad[2]]), call. = FALSE)
  }
  invisible(TRUE)
}

run_cohort <- function(P, start) {
  check_transition_array(P)
  S <- dim(P)[1]; n_cycles <- dim(P)[3]
  if (length(start) != S) stop("start must have one entry per state.")
  if (abs(sum(start) - 1) > 1e-12 || any(start < 0)) stop("start must be a probability vector.")
  trace <- matrix(NA_real_, nrow = n_cycles + 1, ncol = S,
                  dimnames = list(cycle = 0:n_cycles, state = dimnames(P)[[1]]))
  trace[1, ] <- start
  for (t in seq_len(n_cycles)) {
    # Row vector (current distribution) times matrix: people move FROM rows TO columns.
    trace[t + 1, ] <- trace[t, ] %*% P[, , t]
  }
  trace
}

discount_weights <- function(n_cycles, cycle_length, rate) {
  1 / (1 + rate)^((0:n_cycles) * cycle_length)
}

# Within-cycle correction weights applied to boundary values 0..T.
# "trapezoid": average of start and end of each cycle (equivalent to the
#              half-cycle correction). "none": count each cycle at its end.
wcc_weights <- function(n_cycles, method = c("trapezoid", "none")) {
  method <- match.arg(method)
  if (method == "trapezoid") c(0.5, rep(1, n_cycles - 1), 0.5) else c(0, rep(1, n_cycles))
}

accumulate <- function(trace, values, cycle_length, discount_rate,
                       wcc = c("trapezoid", "none")) {
  n_cycles <- nrow(trace) - 1
  if (is.null(dim(values))) {
    if (length(values) != ncol(trace)) stop("values must have one entry per state.")
    per_boundary <- as.vector(trace %*% values)
  } else {
    if (!all(dim(values) == dim(trace))) stop("values matrix must match the trace.")
    per_boundary <- rowSums(trace * values)
  }
  w <- wcc_weights(n_cycles, wcc) * discount_weights(n_cycles, cycle_length, discount_rate)
  sum(w * per_boundary)
}
