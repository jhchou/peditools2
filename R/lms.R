# The LMS method (Cole & Green 1992) and WHO's restricted LMS method ("SD23"), as the PediTools engine.
# Vectorised; NA where undefined.

# |L| below this is treated as L = 0 (the log form), as the engine
l_zero <- 1e-9

# Z of y by plain LMS: ((y/M)^L - 1) / (L S), or log(y/M) / S for L = 0. NA unless y > 0 and finite.
lms_z <- function(y, l, m, s) {
  z <- rep(NA_real_, length(y))
  ok <- !is.na(y) & is.finite(y) & y > 0 & valid_lms(l, m, s)
  log_form <- ok & abs(l) < l_zero
  power <- ok & !log_form
  z[log_form] <- log(y[log_form] / m[log_form]) / s[log_form]
  z[power] <- ((y[power] / m[power])^l[power] - 1) / (l[power] * s[power])
  z
}

# y at Z by plain LMS: M (1 + L S Z)^(1/L), or M exp(S Z) for L = 0. NA where 1 + L S Z <= 0 (no value).
lms_y <- function(z, l, m, s) {
  y <- rep(NA_real_, length(z))
  ok <- !is.na(z) & is.finite(z) & valid_lms(l, m, s)
  log_form <- ok & abs(l) < l_zero
  base <- 1 + l * s * z
  power <- ok & !log_form & !is.na(base) & base > 0
  y[log_form] <- m[log_form] * exp(s[log_form] * z[log_form])
  y[power] <- m[power] * base[power]^(1 / l[power])
  y
}

valid_lms <- function(l, m, s) {
  !is.na(l) & is.finite(l) & !is.na(m) & is.finite(m) & m > 0 & !is.na(s) & is.finite(s) & s > 0
}

# Z by WHO's restricted LMS method: within +-3 the plain Z; beyond, a straight line whose spread is the distance
# between the 2 and 3 SD values: 3 + (y - SD3) / (SD3 - SD2) above, -3 + (y - SD-3) / (SD-2 - SD-3) below.
sd23_z <- function(y, l, m, s) {
  z <- lms_z(y, l, m, s)
  above <- !is.na(z) & z > 3
  below <- !is.na(z) & z < -3
  if (any(above)) {
    sd3 <- lms_y(3, l[above], m[above], s[above])
    sd2 <- lms_y(2, l[above], m[above], s[above])
    z[above] <- 3 + (y[above] - sd3) / (sd3 - sd2)
  }
  if (any(below)) {
    sd3 <- lms_y(-3, l[below], m[below], s[below])
    sd2 <- lms_y(-2, l[below], m[below], s[below])
    z[below] <- -3 + (y[below] - sd3) / (sd2 - sd3)
  }
  z
}

# y at Z by the restricted method (the inverse of sd23_z): SD3 + (Z - 3)(SD3 - SD2) above, SD-3 + (Z + 3)(SD-2 -
# SD-3) below. NA where a cut-off has no value, or the result is not positive.
sd23_y <- function(z, l, m, s) {
  y <- lms_y(z, l, m, s)
  far <- !is.na(z) & is.finite(z) & abs(z) > 3
  if (any(far)) {
    sign <- ifelse(z[far] > 0, 1, -1)
    sd3 <- lms_y(3 * sign, l[far], m[far], s[far])
    sd2 <- lms_y(2 * sign, l[far], m[far], s[far])
    out <- sd3 + (z[far] - 3 * sign) * abs(sd3 - sd2)
    y[far] <- ifelse(!is.na(out) & out > 0, out, NA_real_)
  }
  y
}

# Whether the 2 and 3 SD values exist on both sides (needed by the restricted method)
has_cutoffs <- function(l, m, s) {
  !is.na(lms_y(-3, l, m, s)) & !is.na(lms_y(-2, l, m, s)) & !is.na(lms_y(2, l, m, s)) & !is.na(lms_y(3, l, m, s))
}

#' The LMS method on your own parameters
#'
#' Z-scores of measurements, and measurements at Z-scores, from L, M and S given directly (e.g. from
#' [growth_params()] or a table of your own), by the LMS method or WHO's restricted LMS method.
#'
#' The LMS method (Cole and Green 1992): \eqn{Z = ((y/M)^L - 1)/(L S)}, or \eqn{\log(y/M)/S} for \eqn{L = 0};
#' \eqn{y = M (1 + L S Z)^{1/L}}, or \eqn{M e^{S Z}} for \eqn{L = 0}. The restricted method (WHO's "SD23") is the
#' same within +-3 SD; beyond, the distribution is a straight line whose spread is the distance between the 2 and 3
#' SD values: \eqn{Z = 3 + (y - SD_3)/(SD_3 - SD_2)} above, \eqn{-3 + (y - SD_{-3})/(SD_{-2} - SD_{-3})} below.
#'
#' Arguments are recycled: each has length 1 or the common length.
#'
#' @param y Measurements (positive).
#' @param z Z-scores.
#' @param L,M,S The LMS parameters (M and S positive).
#' @param sd23 `TRUE` for WHO's restricted LMS method; `FALSE` (default) for plain LMS.
#' @return A numeric vector: `NA` where the input is missing or not valid, or the value does not exist (e.g.
#'   \eqn{1 + L S Z \le 0}).
#' @references Cole TJ, Green PJ. Smoothing reference centile curves: the LMS method and penalized likelihood. Stat
#'   Med. 1992;11(10):1305-19.
#'
#'   WHO Multicentre Growth Reference Study Group. WHO Child Growth Standards: Length/height-for-age,
#'   weight-for-age, weight-for-length, weight-for-height and body mass index-for-age: Methods and development.
#'   Geneva: World Health Organization; 2006 (chapter 7).
#' @seealso [growth_z()] and [growth_y()] to use a chart's parameters at any x.
#' @export
#' @examples
#' growth_lms_z(c(8.5, 10.2), L = -0.0757, M = 8.9014, S = 0.10846)
#' growth_lms_y(-2:2, L = -0.0757, M = 8.9014, S = 0.10846)
#'
#' # Beyond +-3 SD the restricted method differs
#' growth_lms_z(14, L = -0.0757, M = 8.9014, S = 0.10846, sd23 = c(FALSE, TRUE))
growth_lms_z <- function(y, L, M, S, sd23 = FALSE) {
  a <- recycle(y = y, L = L, M = M, S = S, sd23 = sd23)
  check_flag(a$sd23, "sd23")
  out <- lms_z(a$y, a$L, a$M, a$S)
  out[a$sd23] <- sd23_z(a$y, a$L, a$M, a$S)[a$sd23]
  out
}

#' @rdname growth_lms_z
#' @export
growth_lms_y <- function(z, L, M, S, sd23 = FALSE) {
  a <- recycle(z = z, L = L, M = M, S = S, sd23 = sd23)
  check_flag(a$sd23, "sd23")
  out <- lms_y(a$z, a$L, a$M, a$S)
  out[a$sd23] <- sd23_y(a$z, a$L, a$M, a$S)[a$sd23]
  out
}

check_flag <- function(flag, name) {
  if (!is.logical(flag) || anyNA(flag)) {
    stop("`", name, "` must be TRUE or FALSE.", call. = FALSE)
  }
}
