# Parameters of one table at x: as the PediTools engine, with the package's
# `extrapolate` modes. Rows are interpolated linearly; at (within floating-point noise of) a row its values are used as
# published; the table is never interpolated across its step (x_break); between an end row and a coverage bound
# beyond it, and beyond the coverage with extrapolate = "linear", the parameters are on the line through the two end
# rows.

# Relative tolerance for x on a row or a bound (as the PediTools engine)
x_tolerance <- 1e-9
# For age-based tables, an x at most this many minutes outside a coverage bound is taken at the bound
bound_minutes <- 5
days_per_unit <- c(days = 1, weeks = 7, months = 30.4375, years = 365.25)

tol <- function(x) x_tolerance * pmax(1, abs(x))

bound_tol <- function(x, x_units) {
  if (x_units %in% names(days_per_unit)) {
    pmax(bound_minutes / 1440 * days_per_unit[["days"]] / days_per_unit[[x_units]], tol(x))
  } else {
    tol(x)
  }
}

# The parameter columns at x of one table (meta: its row of the tables; rows: its parameter rows, increasing in x).
# Returns a list: the parameters (one vector per column), allowed (x within the range extrapolate allows),
# extrapolated (on the line beyond the end rows or the step), beyond (outside the published coverage).
params_at <- function(x, meta, rows, columns, extrapolate) {
  xs <- rows$x
  n <- length(xs)
  lo <- meta$x_min
  hi <- meta$x_max
  brk <- meta$x_break
  out <- list(allowed = rep(FALSE, length(x)), extrapolated = rep(FALSE, length(x)), beyond = rep(FALSE, length(x)))
  for (column in columns) out[[column]] <- rep(NA_real_, length(x))
  finite <- !is.na(x) & is.finite(x)

  # Within the bound tolerance outside the coverage: at the bound
  low <- finite & x < lo & x >= lo - bound_tol(lo, meta$x_units)
  high <- finite & x > hi & x <= hi + bound_tol(hi, meta$x_units)
  x[low] <- lo
  x[high] <- hi
  in_coverage <- finite & x >= lo & x <= hi
  out$allowed <- switch(extrapolate,
    coverage = in_coverage,
    none = in_coverage & x >= xs[1] - tol(xs[1]) & x <= xs[n] + tol(xs[n]),
    linear = finite
  )
  out$beyond <- out$allowed & !in_coverage

  # Each x is decided by the first rule that applies: a row used as published (row), or the line through rows pair
  # and pair + 1
  st <- list(row = rep(NA_integer_, length(x)), pair = rep(NA_integer_, length(x)), todo = out$allowed)
  decide <- function(st, cond, row = NULL, pair = NULL) {
    hit <- st$todo & cond
    if (!is.null(row)) st$row[hit] <- if (length(row) == 1) row else row[hit]
    if (!is.null(pair)) st$pair[hit] <- pair
    st$todo <- st$todo & !cond
    st
  }
  has_break <- !is.na(brk)
  step_lo <- has_break && abs(lo - brk) <= tol(brk)
  step_hi <- has_break && abs(hi - brk) <= tol(brk)
  # Beyond a coverage bound at the step: the two rows on the coverage side of it
  if (step_lo) st <- decide(st, x < lo, pair = which(xs > brk)[1])
  if (step_hi) st <- decide(st, x > hi, pair = max(which(xs < brk)) - 1L)
  st <- decide(st, x < xs[1] - tol(xs[1]), pair = 1L)
  st <- decide(st, x > xs[n] + tol(xs[n]), pair = n - 1L)
  st <- decide(st, x <= xs[1], row = 1L)
  st <- decide(st, x >= xs[n], row = n)
  i <- findInterval(x, xs)
  i[!st$todo] <- NA_integer_
  below <- xs[pmax(i, 1L)]
  above <- xs[pmin(i + 1L, n)]
  # Between the two rows around the step: the row on x's side (at the step itself: after it)
  if (has_break) st <- decide(st, below < brk & brk < above, row = ifelse(x >= brk - tol(brk), i + 1L, i))
  st <- decide(st, abs(x - below) <= tol(below), row = i)
  st <- decide(st, abs(x - above) <= tol(above), row = i + 1L)
  row <- st$row
  pair <- st$pair
  pair[st$todo] <- i[st$todo]

  exact <- !is.na(row)
  line <- !is.na(pair)
  if (any(line)) {
    k <- pair[line]
    t <- (x[line] - xs[k]) / (xs[k + 1L] - xs[k])
  }
  for (column in columns) {
    v <- rows[[column]]
    out[[column]][exact] <- v[row[exact]]
    if (any(line)) out[[column]][line] <- v[k] + (v[k + 1L] - v[k]) * t
  }
  beyond_rows <- x < xs[1] - tol(xs[1]) | x > xs[n] + tol(xs[n])
  out$extrapolated <- line & (beyond_rows | (step_lo & x < lo) | (step_hi & x > hi))
  out
}
