# The INTERGROWTH-21st models, as the PediTools engine: for each chart, measure and sex, contiguous parts, each used
# from its start until the next part starts: an ST3 day table (the skew t distribution of gamlss.dist), or a normal
# model whose mean and SD are equations in x.

# ST3: a Student t with tau degrees of freedom, scaled by sigma/nu below mu and by sigma*nu above it.
#   F(y) = 2 T(nu (y - mu) / sigma) / (1 + nu^2)                         for y < mu
#   1 - F(y) = 2 nu^2 P(T > (y - mu) / (sigma nu)) / (1 + nu^2)         for y >= mu
# Z = Phi^-1(F(y)), from the smaller tail.
st3_z <- function(y, mu, sigma, nu, tau) {
  z <- rep(NA_real_, length(y))
  ok <- !is.na(y) & is.finite(y) & valid_st3(mu, sigma, nu, tau)
  if (!any(ok)) {
    return(z)
  }
  y <- y[ok]
  mu <- mu[ok]
  sigma <- sigma[ok]
  nu <- nu[ok]
  tau <- tau[ok]
  nu2 <- nu * nu
  below <- y < mu
  lower <- upper <- rep(NA_real_, length(y))
  lower[below] <- 2 * stats::pt(-nu[below] * (y[below] - mu[below]) / sigma[below], tau[below],
    lower.tail = FALSE
  ) / (1 + nu2[below])
  upper[below] <- 1 - lower[below]
  upper[!below] <- 2 * nu2[!below] * stats::pt((y[!below] - mu[!below]) / (sigma[!below] * nu[!below]),
    tau[!below],
    lower.tail = FALSE
  ) / (1 + nu2[!below])
  lower[!below] <- 1 - upper[!below]
  out <- ifelse(lower <= 0.5, stats::qnorm(lower), stats::qnorm(upper, lower.tail = FALSE))
  out[!is.finite(out)] <- NA_real_
  z[ok] <- out
  z
}

# The value at Z: the ST3 quantile of Phi(Z), from the smaller tail.
st3_y <- function(z, mu, sigma, nu, tau) {
  y <- rep(NA_real_, length(z))
  ok <- !is.na(z) & is.finite(z) & valid_st3(mu, sigma, nu, tau)
  if (!any(ok)) {
    return(y)
  }
  z <- z[ok]
  mu <- mu[ok]
  sigma <- sigma[ok]
  nu <- nu[ok]
  tau <- tau[ok]
  nu2 <- nu * nu
  p <- stats::pnorm(z)
  q <- stats::pnorm(z, lower.tail = FALSE)
  below <- p < 1 / (1 + nu2)
  out <- rep(NA_real_, length(z))
  out[below] <- mu[below] + (sigma[below] / nu[below]) * stats::qt(p[below] * (1 + nu2[below]) / 2, tau[below])
  out[!below] <- mu[!below] + sigma[!below] * nu[!below] *
    stats::qt(q[!below] * (1 + nu2[!below]) / (2 * nu2[!below]), tau[!below], lower.tail = FALSE)
  out[!is.finite(out)] <- NA_real_
  y[ok] <- out
  y
}

valid_st3 <- function(mu, sigma, nu, tau) {
  fin <- function(v) !is.na(v) & is.finite(v)
  fin(mu) & fin(sigma) & sigma > 0 & fin(nu) & nu > 0 & fin(tau) & tau > 0
}

# Normal model: on the log scale for log(y)
normal_z <- function(y, mu, sigma, y_scale) {
  z <- rep(NA_real_, length(y))
  ok <- !is.na(y) & is.finite(y) & valid_normal(mu, sigma) & (y_scale == "identity" | y > 0)
  log_scale <- ok & y_scale == "log"
  identity <- ok & y_scale == "identity"
  z[log_scale] <- (log(y[log_scale]) - mu[log_scale]) / sigma[log_scale]
  z[identity] <- (y[identity] - mu[identity]) / sigma[identity]
  z
}

normal_y <- function(z, mu, sigma, y_scale) {
  y <- rep(NA_real_, length(z))
  ok <- !is.na(z) & is.finite(z) & valid_normal(mu, sigma)
  v <- mu + z * sigma
  y[ok] <- ifelse(y_scale[ok] == "log", exp(v[ok]), v[ok])
  y[!is.finite(y)] <- NA_real_
  y
}

valid_normal <- function(mu, sigma) {
  !is.na(mu) & is.finite(mu) & !is.na(sigma) & is.finite(sigma) & sigma > 0
}

# One equation at x: transform(sum of coef * t^power * log(t)^log_power), t = x / x_scale, the terms added in their
# order (as the engine; not sum(), which accumulates in extended precision).
equation_at <- function(x, terms) {
  t <- x / terms$x_scale[1]
  log_t <- rep(NaN, length(t))
  log_t[!is.na(t) & t > 0] <- log(t[!is.na(t) & t > 0])
  total <- rep(0, length(x))
  for (k in seq_len(nrow(terms))) {
    v <- terms$coef[k] * (if (terms$power[k] == 0) 1 else t^terms$power[k])
    if (terms$log_power[k] > 0) v <- v * log_t^terms$log_power[k]
    total <- total + v
  }
  switch(terms$transform[1],
    exp = exp(total),
    sqrt = ifelse(!is.na(total) & total >= 0, sqrt(pmax(total, 0)), NaN),
    total
  )
}

# The parts of one model table (meta: its row of the tables), in order of their start (cached for the session)
model_parts <- function(key, meta) {
  if (!is.null(cache$models[[key]])) {
    return(cache$models[[key]])
  }
  parts <- build_parts(pt_data(), meta)
  if (is.null(cache$models)) cache$models <- list()
  cache$models[[key]] <- parts
  parts
}

# The parts of a model table from data with st3 and equations rows
build_parts <- function(data, meta) {
  sex <- meta$sex
  st3 <- data$st3
  if (is.null(st3)) st3 <- builtin_data$st3[0, ]
  in_st3 <- st3$chart == meta$chart & st3$measure == meta$measure & (is.na(sex) | st3$sex == sex)
  s <- st3[in_st3, ]
  eq <- data$equations
  if (is.null(eq)) eq <- builtin_data$equations[0, ]
  for_sex <- is.na(eq$sex) | (!is.na(sex) & eq$sex == sex)
  boys_term <- eq$male == 1
  in_eq <- eq$chart == meta$chart & eq$measure == meta$measure & for_sex & (!boys_term | (!is.na(sex) & sex == "m"))
  e <- eq[in_eq, ]
  parts <- list()
  if (nrow(s)) {
    s <- s[order(s$x), ]
    parts[[length(parts) + 1]] <- list(from = s$x[1], model = "st3", rows = s[c("x", "mu", "sigma", "nu", "tau")])
  }
  for (from in unique(e$x_from)) {
    r <- e[e$x_from == from, ]
    parts[[length(parts) + 1]] <- list(
      from = from, model = "normal", y_scale = r$y_scale[1],
      mu = r[r$param == "mu", ], sigma = r[r$param == "sigma", ]
    )
  }
  parts[order(vapply(parts, function(p) p$from, numeric(1)))]
}

# The distribution of one model table at x: as params_at() for LMS tables. Returns a list: part ("st3" or
# "normal"), mu, sigma, nu, tau, y_scale, allowed, extrapolated, beyond. extrapolate "none" is the coverage (the
# equations have no rows; the ST3 rows span their part).
model_at <- function(x, meta, parts, extrapolate) {
  n <- length(x)
  lo <- meta$x_min
  hi <- meta$x_max
  out <- list(
    part = rep(NA_character_, n), mu = rep(NA_real_, n), sigma = rep(NA_real_, n), nu = rep(NA_real_, n),
    tau = rep(NA_real_, n), y_scale = rep(NA_character_, n)
  )
  finite <- !is.na(x) & is.finite(x)
  low <- finite & x < lo & x >= lo - bound_tol(lo, meta$x_units)
  high <- finite & x > hi & x <= hi + bound_tol(hi, meta$x_units)
  x[low] <- lo
  x[high] <- hi
  in_coverage <- finite & x >= lo & x <= hi
  out$allowed <- if (extrapolate == "linear") finite else in_coverage
  out$beyond <- out$allowed & !in_coverage
  out$extrapolated <- out$beyond

  # The part: the last one starting at or before x (below the first: the first)
  starts <- vapply(parts, function(p) p$from, numeric(1))
  index <- rep(1L, n)
  for (k in seq_along(parts)) index[finite & x >= starts[k] - tol(starts[k])] <- k
  for (k in seq_along(parts)) {
    i <- which(out$allowed & index == k)
    if (!length(i)) next
    p <- parts[[k]]
    out$part[i] <- p$model
    if (p$model == "st3") {
      rows_meta <- list(
        x_min = p$rows$x[1], x_max = p$rows$x[nrow(p$rows)], x_break = NA_real_,
        x_units = meta$x_units
      )
      at <- params_at(x[i], rows_meta, p$rows, c("mu", "sigma", "nu", "tau"), "linear")
      for (v in c("mu", "sigma", "nu", "tau")) out[[v]][i] <- at[[v]]
      out$extrapolated[i] <- out$extrapolated[i] | at$extrapolated
    } else {
      out$mu[i] <- equation_at(x[i], p$mu)
      out$sigma[i] <- equation_at(x[i], p$sigma)
      out$y_scale[i] <- p$y_scale
    }
  }
  out
}

#' The ST3 distribution on your own parameters
#'
#' Z-scores of measurements, and measurements at Z-scores, by the skew t distribution "ST3" of the R package
#' gamlss.dist (the INTERGROWTH-21st newborn size standards from 33+0 weeks), from mu, sigma, nu and tau given
#' directly (e.g. from [growth_params()]).
#'
#' ST3 is a Student t with tau degrees of freedom, scaled by sigma/nu below mu and by sigma nu above it: with T the
#' t distribution function, \eqn{F(y) = 2 T(\nu (y - \mu)/\sigma)/(1 + \nu^2)} for \eqn{y < \mu} and
#' \eqn{1 - F(y) = 2 \nu^2 (1 - T((y - \mu)/(\sigma \nu)))/(1 + \nu^2)} for \eqn{y \ge \mu}; \eqn{Z = \Phi^{-1}(F(y))}
#' (from the smaller tail, so that Z stays accurate far into the tails). mu is not the median unless nu = 1.
#'
#' Arguments are recycled: each has length 1 or the common length.
#'
#' @param y Measurements.
#' @param z Z-scores.
#' @param mu,sigma,nu,tau The ST3 parameters (sigma, nu and tau positive).
#' @return A numeric vector; `NA` where an input is missing or not valid.
#' @references Rigby RA, Stasinopoulos DM, Heller GZ, De Bastiani F. Distributions for Modeling Location, Scale,
#'   and Shape: Using GAMLSS in R. Chapman and Hall/CRC; 2019.
#' @seealso [growth_z()] and [growth_y()] to use a chart's parameters at any x; [growth_lms_z()] for LMS.
#' @export
#' @examples
#' # INTERGROWTH-21st newborn weight, boys, 231 days (33+0 weeks)
#' growth_st3_z(c(1.5, 2, 2.5), mu = 1.905973, sigma = 0.4027349, nu = 1.095261, tau = 17.05299)
#' growth_st3_y(c(-2, 0, 2), mu = 1.905973, sigma = 0.4027349, nu = 1.095261, tau = 17.05299)
growth_st3_z <- function(y, mu, sigma, nu, tau) {
  a <- recycle(y = y, mu = mu, sigma = sigma, nu = nu, tau = tau)
  st3_z(a$y, a$mu, a$sigma, a$nu, a$tau)
}

#' @rdname growth_st3_z
#' @export
growth_st3_y <- function(z, mu, sigma, nu, tau) {
  a <- recycle(z = z, mu = mu, sigma = sigma, nu = nu, tau = tau)
  st3_y(a$z, a$mu, a$sigma, a$nu, a$tau)
}
