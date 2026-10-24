#' Z-scores and probabilities of measurements
#'
#' The Z-score (`growth_z()`) or the probability (`growth_p()`, the percentile / 100) of each measurement on a
#' chart, by the chart's published method unless switched. Vectorised over every argument.
#'
#' The parameters at x are interpolated linearly between the rows of the chart's table (never across its step,
#' `x_break` in [growth_charts()]). Methods:
#' - **LMS** (Cole and Green): every LMS chart.
#' - **WHO's restricted LMS method beyond +-3 SD** ("SD23"): the charts whose authors prescribe it (`z_method`
#'   `"sd23"` in [growth_charts()]); `sd23 = TRUE` applies it to any LMS chart, `FALSE` turns it off.
#' - **CDC's extended BMI method** at or above the 95th percentile of `cdc_2000_bmi` (CDC's recommendation, on by
#'   default): the probability is \eqn{0.90 + 0.10 \Phi((BMI - P95)/\sigma)}, with
#'   \eqn{\sigma = 0.3728 + 0.5196 a - 0.0091 a^2} (boys) or \eqn{0.8334 + 0.3712 a - 0.0011 a^2} (girls), a the age
#'   in years, and \eqn{Z = \Phi^{-1}(p)}. `cdc_bmi_ext = FALSE` gives the plain LMS values.
#' - **INTERGROWTH-21st** (`z_method` `"cdf"`): \eqn{Z = \Phi^{-1}(F(y))} from the distribution of the part used at
#'   x: the ST3 skew t ([growth_st3_z()]) or a normal distribution whose mean and SD are equations in x (of log(y)
#'   when `y_scale` is `"log"`). Newborn size uses the very preterm equations before 231 days and the ST3 table from
#'   231 days; the parts are never mixed. `sd23 = TRUE` applies WHO's restricted method to these too.
#'
#' @param y Measurements, in the chart's measure units (`measure_units` in [growth_charts()]); positive on LMS
#'   charts and the INTERGROWTH-21st equations on the log scale.
#' @param x Ages, or lengths or heights for weight-for-length and weight-for-height, in the chart's x units
#'   (`x_units` in [growth_charts()]; to convert: [growth_convert()], [growth_age_days()], [growth_pma()],
#'   [growth_corrected_age()]).
#' @param sex `"m"`, `"f"`, `"male"` or `"female"`, in any case; other values are `NA` (with a warning). Ignored for
#'   charts with one table for both sexes.
#' @param chart,measure Chart and measure ids, as in [growth_charts()].
#' @param extrapolate Which x are calculated: `"coverage"` (default) the chart's published coverage (`x_min` to
#'   `x_max` in [growth_charts()], which may reach half a row step beyond the end rows of tables whose rows are band
#'   midpoints); `"none"` only between the table's first and last rows (for INTERGROWTH-21st charts, the coverage);
#'   `"linear"` any x, the parameters beyond the coverage on the line through the two end rows (for the
#'   INTERGROWTH-21st equations, the equations themselves). Outside: `NA`, with one warning per call.
#' @param sd23 `NULL` (default): WHO's restricted LMS method where the chart's authors prescribe it; `TRUE`: on
#'   every chart (beyond +-3 SD, the line whose spread is the distance between the 2 and 3 SD values of the chart's
#'   own distribution); `FALSE`: off.
#' @param cdc_bmi_ext `NULL` (default) or `TRUE`: the CDC extended BMI method on `cdc_2000_bmi` (an error on any
#'   other chart with `TRUE`); `FALSE`: plain LMS.
#' @return A numeric vector, as long as the longest argument (each has length 1 or that length). `NA` where an input
#'   is missing or not valid, or x is outside the range allowed by `extrapolate` (see [growth_detail()] for why).
#' @seealso [growth_y()] for the measurement at a Z-score or probability; [growth_detail()] for the parameters, the
#'   method used and the status of each value.
#' @export
#' @examples
#' # Weight 8.2 kg of a girl of 9.5 months, CDC 2000 infant chart
#' growth_z(8.2, 9.5, "f", "cdc_2000_infant", "weight")
#' growth_p(8.2, 9.5, "f", "cdc_2000_infant", "weight")
#'
#' # Vectorised: one call for a data frame
#' d <- data.frame(wt = c(7.1, 9.8, 12.4), age = c(6, 12, 24), sex = c("m", "f", "m"))
#' growth_z(d$wt, d$age, d$sex, "who_2006_infant", "weight")
#'
#' # WHO's restricted method beyond +-3 SD, and plain LMS
#' growth_z(3.5, 6, "m", "who_2006_infant", "weight")
#' growth_z(3.5, 6, "m", "who_2006_infant", "weight", sd23 = FALSE)
#'
#' # CDC extended BMI above the 95th percentile (age in months)
#' growth_z(30, 150, "m", "cdc_2000_bmi", "bmi")
#' growth_z(30, 150, "m", "cdc_2000_bmi", "bmi", cdc_bmi_ext = FALSE)
growth_z <- function(y, x, sex, chart, measure, extrapolate = "coverage", sd23 = NULL, cdc_bmi_ext = NULL) {
  out <- evaluate(x, sex, chart, measure,
    y = y, extrapolate = extrapolate, sd23 = sd23,
    cdc_bmi_ext = cdc_bmi_ext
  )
  warn_status(out$status, extrapolate)
  out$z
}

#' @rdname growth_z
#' @export
growth_p <- function(y, x, sex, chart, measure, extrapolate = "coverage", sd23 = NULL, cdc_bmi_ext = NULL) {
  out <- evaluate(x, sex, chart, measure,
    y = y, extrapolate = extrapolate, sd23 = sd23,
    cdc_bmi_ext = cdc_bmi_ext
  )
  warn_status(out$status, extrapolate)
  out$p
}

#' Measurements at Z-scores or probabilities
#'
#' The measurement at each Z-score or probability on a chart: the inverse of [growth_z()] and [growth_p()], by the
#' same methods (the restricted LMS method beyond +-3 SD where it applies; on `cdc_2000_bmi`, from the 95th
#' percentile, \eqn{BMI = P95 + \sigma \Phi^{-1}((p - 0.90)/0.10)}). Vectorised over every argument.
#'
#' @inheritParams growth_z
#' @param z Z-scores. Give `z` or `p`, not both.
#' @param p Probabilities (the percentile / 100), strictly between 0 and 1.
#' @return A numeric vector of measurements in the chart's measure units; `NA` where an input is missing or not
#'   valid, x is outside the range allowed, or the method gives no value (e.g. the LMS curve has none at a very low
#'   Z when L > 0).
#' @seealso [growth_z()].
#' @export
#' @examples
#' # The 3rd, 50th and 97th percentile weights of a girl of 9.5 months
#' growth_y(p = c(0.03, 0.5, 0.97), x = 9.5, sex = "f", chart = "cdc_2000_infant", measure = "weight")
#'
#' # Expected growth: the weight at 12 months at the same Z-score as 8.2 kg at 9.5 months
#' z <- growth_z(8.2, 9.5, "f", "cdc_2000_infant", "weight")
#' growth_y(z, 12, "f", "cdc_2000_infant", "weight")
growth_y <- function(z, x, sex, chart, measure, p = NULL, extrapolate = "coverage", sd23 = NULL,
                     cdc_bmi_ext = NULL) {
  if (missing(z) == is.null(p)) {
    stop("Give exactly one of `z` and `p`.", call. = FALSE)
  }
  out <- if (missing(z)) {
    evaluate(x, sex, chart, measure, p = p, extrapolate = extrapolate, sd23 = sd23, cdc_bmi_ext = cdc_bmi_ext)
  } else {
    evaluate(x, sex, chart, measure, z = z, extrapolate = extrapolate, sd23 = sd23, cdc_bmi_ext = cdc_bmi_ext)
  }
  warn_status(out$status, extrapolate)
  out$y
}

#' Parameters at any x
#'
#' A chart's parameters at each x: interpolated between the table's rows as for [growth_z()], with the same
#' `extrapolate` modes. Vectorised over every argument.
#'
#' @inheritParams growth_z
#' @return A data frame, one row per element: `chart`, `measure`, `sex` (`NA` for a chart with one table for both
#'   sexes), `x`, `family` (the chart's, as in [growth_charts()]), `part` (the distribution used at x: `"lms"`,
#'   `"st3"` or `"normal"`), the parameters (`L`, `M`, `S` for LMS; `mu`, `sigma`, `nu`, `tau` for ST3; `mu`,
#'   `sigma` and `y_scale` for the INTERGROWTH-21st equations, where `y_scale` `"log"` means they are the mean and SD
#'   of log(y); only the columns of the families of the charts given), `status` (`"ok"`, `"missing"`, `"outside"`
#'   or `"undefined"`), `extrapolated` (the parameters are on the line beyond the end rows, or the equations beyond
#'   the coverage) and `beyond_coverage` (x is outside the published coverage, with `extrapolate = "linear"`).
#' @seealso [growth_table()] and [growth_equations()] for the published parameters; [growth_lms_z()] and
#'   [growth_st3_z()] to use parameters directly.
#' @export
#' @examples
#' growth_params(c(9.5, 10, 10.5), "f", "cdc_2000_infant", "weight")
#'
#' # Fenton 2003: published from 22 0/7 weeks, the first row at 22.5 weeks
#' growth_params(22.2, NA, "fenton_2003", "weight")
growth_params <- function(x, sex, chart, measure, extrapolate = "coverage") {
  out <- evaluate(x, sex, chart, measure, extrapolate = extrapolate)
  out[c(
    "chart", "measure", "sex", "x", "family", "part", intersect(dist_columns, names(out)), "status",
    "extrapolated", "beyond_coverage"
  )]
}

#' Results in detail
#'
#' Everything about each value: the result, the parameters at x, the method applied and the status, for checking or
#' studying how the methods behave. The arguments are those of [growth_z()] (a measurement `y`) or [growth_y()] (a
#' Z-score `z` or a probability `p`); give exactly one of `y`, `z`, `p`. No warnings: the status says why a value is
#' `NA`.
#'
#' @inheritParams growth_z
#' @param y Measurements; or give `z` or `p` instead.
#' @param z Z-scores.
#' @param p Probabilities (the percentile / 100).
#' @return A data frame, one row per element:
#'   \describe{
#'     \item{chart, measure, sex, x}{The inputs (`sex` normalised to `"m"`/`"f"`, `NA` for a chart with one table
#'       for both sexes).}
#'     \item{y, z, p}{The measurement, Z-score and probability: the one given as given, the others calculated.}
#'     \item{method}{`"lms"`, `"sd23"` (the restricted method; also within +-3 SD, where it equals plain LMS) or
#'       `"cdc_bmi_ext"` (the CDC extended BMI method was used for this value).}
#'     \item{status}{`"ok"`; `"missing"` (an input is `NA`); `"outside"` (x outside the range allowed by
#'       `extrapolate`); `"invalid"` (a measurement not finite, or not positive on an LMS chart or an
#'       INTERGROWTH-21st equation on the log scale; an infinite Z; a probability not strictly between 0 and 1);
#'       `"undefined"` (the parameters or the method give no value there).}
#'     \item{family, part, extrapolated, beyond_coverage}{As in [growth_params()].}
#'     \item{L, M, S, mu, sigma, nu, tau, y_scale}{The parameters at x, as in [growth_params()].}
#'     \item{p95, ext_sigma}{On `cdc_2000_bmi` with the extended method: the BMI at the 95th percentile and the
#'       extension's sigma (`NA` otherwise), e.g. for the BMI as a percentage of P95.}
#'   }
#' @export
#' @examples
#' growth_detail(c(3.5, 7.9, 12.5), 6, "m", "who_2006_infant", "weight")
#'
#' # BMI as a percentage of the 95th percentile
#' d <- growth_detail(c(24, 30, 36), 150, "m", "cdc_2000_bmi", "bmi")
#' 100 * d$y / d$p95
growth_detail <- function(y = NULL, x, sex, chart, measure, z = NULL, p = NULL, extrapolate = "coverage",
                          sd23 = NULL, cdc_bmi_ext = NULL) {
  if (sum(!is.null(y), !is.null(z), !is.null(p)) != 1) {
    stop("Give exactly one of `y`, `z` and `p`.", call. = FALSE)
  }
  evaluate(x, sex, chart, measure,
    y = y, z = z, p = p, extrapolate = extrapolate, sd23 = sd23,
    cdc_bmi_ext = cdc_bmi_ext
  )
}
