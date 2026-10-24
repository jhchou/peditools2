# Conversion helpers, outside the calculations (which take the chart's own units): the constants of the PediTools
# engine.

# dimension => unit => factor to the dimension's base unit (exact by definition: 1 lb = 453.59237 g, 1 oz = 1/16 lb,
# 1 in = 2.54 cm, 1 month = 365.25/12 days)
unit_factors <- list(
  mass = c(g = 1, kg = 1000, lb = 453.59237, oz = 28.349523125),
  length = c(mm = 0.1, cm = 1, m = 100, `in` = 2.54, ft = 30.48),
  time = c(days = 1, weeks = 7, months = 30.4375, years = 365.25),
  bmi = c(`kg/m2` = 1, `g/cm2*10` = 1)
)
# The due date, 40 0/7 weeks, in days of gestation (corrected age 0)
due_date_days <- 280

#' Convert units
#'
#' Converts values between units of the same kind, with the constants PediTools uses: mass (`"g"`, `"kg"`, `"lb"`,
#' `"oz"`), length (`"mm"`, `"cm"`, `"m"`, `"in"`, `"ft"`), time (`"days"`, `"weeks"`, `"months"`, `"years"`) and BMI
#' (`"kg/m2"`, and Olsen 2015's `"g/cm2*10"`, numerically the same). The factors are exact by definition: 1 lb =
#' 453.59237 g, 1 oz = 1/16 lb, 1 in = 2.54 cm, 1 week = 7 days, 1 month = 30.4375 days (365.25 / 12), 1 year =
#' 365.25 days, as in the CDC and WHO tables.
#'
#' The calculation functions take values in the chart's own units only ([growth_charts()]: `x_units`,
#' `measure_units`); convert before calling them.
#'
#' @param x Numeric values.
#' @param from,to Units (each of length 1 or the length of `x`).
#' @return The values in `to` units.
#' @export
#' @examples
#' growth_convert(c(30, 365), "days", "months")
#' growth_convert(7.5, "lb", "kg")
#'
#' # Ages in days to the x of a chart in months
#' x_units <- growth_charts("cdc_2000_infant")$x_units[1]
#' growth_convert(c(14, 200, 400), "days", x_units)
growth_convert <- function(x, from, to) {
  a <- recycle(x = x, from = from, to = to)
  if (!is.numeric(a$x) && !all(is.na(a$x))) stop("`x` must be numeric.", call. = FALSE)
  dimension <- function(units) {
    d <- rep(NA_character_, length(units))
    for (name in names(unit_factors)) d[units %in% names(unit_factors[[name]])] <- name
    unknown <- unique(units[is.na(d)])
    if (length(unknown)) {
      stop("Unknown unit", if (length(unknown) > 1) "s", ": ", paste0("\"", unknown, "\"", collapse = ", "),
        ". Known: ", paste(unlist(lapply(unit_factors, names)), collapse = ", "), ".",
        call. = FALSE
      )
    }
    d
  }
  d_from <- dimension(a$from)
  d_to <- dimension(a$to)
  bad <- d_from != d_to
  if (any(bad)) {
    stop("Cannot convert ", a$from[bad][1], " to ", a$to[bad][1], ".", call. = FALSE)
  }
  factors <- unlist(unname(unit_factors))
  out <- as.numeric(a$x) * factors[a$from] / factors[a$to]
  same <- a$from == a$to
  out[same] <- as.numeric(a$x)[same]
  unname(out)
}

#' Age in days between dates
#'
#' Whole days from the date of birth to the date of measurement, as PediTools counts them (calendar dates, so no
#' daylight-saving effects). Negative when the date is before the birth.
#'
#' @param birth,date Dates: `Date` vectors, or character in the form `"YYYY-MM-DD"` (each of length 1 or a common
#'   length). Date-times (`POSIXct`) are refused: convert them with `as.Date()` in the time zone of your data, so
#'   that a date does not shift by a day.
#' @return Integer days; `NA` where a date is missing or not a valid date (with a warning).
#' @export
#' @examples
#' growth_age_days("2024-02-10", c("2024-02-29", "2024-03-01", "2025-02-10"))
growth_age_days <- function(birth, date) {
  a <- recycle(birth = birth, date = date)
  as_date <- function(v, name) {
    if (inherits(v, "POSIXt")) {
      stop("`", name, "`: date-times are not accepted; use as.Date() with the time zone of your data.", call. = FALSE)
    }
    if (inherits(v, "Date")) {
      return(v)
    }
    if (!is.character(v) && !all(is.na(v))) {
      stop("`", name, "` must be Date or character \"YYYY-MM-DD\".", call. = FALSE)
    }
    v <- as.character(v)
    d <- as.Date(v, format = "%Y-%m-%d")
    # as.Date() accepts trailing text and some impossible days: check the round trip
    ok <- !is.na(d) & format(d, "%Y-%m-%d") == trimws(v)
    bad <- !is.na(v) & !ok
    if (any(bad)) {
      warning(sum(bad), " `", name, "` value", if (sum(bad) > 1) "s", " not a date \"YYYY-MM-DD\" (e.g. \"",
        v[bad][1], "\"); set to NA.",
        call. = FALSE
      )
    }
    d[!ok] <- NA
    d
  }
  as.integer(as_date(a$date, "date") - as_date(a$birth, "birth"))
}

#' Corrected age and postmenstrual age
#'
#' The corrected age (the age from the due date, 40 0/7 weeks) and the postmenstrual age (PMA: the gestational age at
#' birth plus the age) of an infant born preterm, as PediTools calculates them: corrected age = age - (280 days - GA
#' at birth), PMA = GA at birth + age.
#'
#' Whether and until when to correct (e.g. to 2 years, and for which gestational ages) is a clinical convention left
#' to you: the corrected age is calculated for any GA, and is negative before the due date.
#'
#' @param age_days Chronological ages in days (e.g. from [growth_age_days()]).
#' @param ga_weeks,ga_days The gestational age at birth: completed weeks and days (`ga_days` 0 to 6; default 0). A
#'   GA in days only can be given as `ga_weeks = 0, ga_days = <days>`.
#' @param units The units of the result: `"days"` (default for the corrected age) or `"weeks"` (default for the PMA,
#'   the x of the preterm charts), or `"months"`, `"years"`.
#' @return Numeric ages in `units`.
#' @export
#' @examples
#' # Born at 28 3/7 weeks, 100 days old
#' growth_corrected_age(100, 28, 3)
#' growth_corrected_age(100, 28, 3, units = "months")
#' growth_pma(100, 28, 3)
#'
#' # The x for Fenton 2013 from a GA at birth and a date
#' age <- growth_age_days("2026-01-05", "2026-02-20")
#' growth_pma(age, 26, 4)
growth_corrected_age <- function(age_days, ga_weeks, ga_days = 0, units = "days") {
  a <- ga_args(age_days, ga_weeks, ga_days)
  growth_convert(a$age_days - (due_date_days - a$ga), "days", check_time_units(units))
}

#' @rdname growth_corrected_age
#' @export
growth_pma <- function(age_days, ga_weeks, ga_days = 0, units = "weeks") {
  a <- ga_args(age_days, ga_weeks, ga_days)
  growth_convert(a$ga + a$age_days, "days", check_time_units(units))
}

ga_args <- function(age_days, ga_weeks, ga_days) {
  a <- recycle(age_days = age_days, ga_weeks = ga_weeks, ga_days = ga_days)
  for (name in names(a)) {
    if (!is.numeric(a[[name]]) && !all(is.na(a[[name]]))) stop("`", name, "` must be numeric.", call. = FALSE)
  }
  if (any(a$ga_weeks < 0 | a$ga_days < 0, na.rm = TRUE)) {
    stop("`ga_weeks` and `ga_days` must not be negative.", call. = FALSE)
  }
  list(age_days = as.numeric(a$age_days), ga = 7 * as.numeric(a$ga_weeks) + as.numeric(a$ga_days))
}

check_time_units <- function(units) {
  if (!is.character(units) || length(units) != 1 || !units %in% names(unit_factors$time)) {
    stop("`units` must be one of ", paste0("\"", names(unit_factors$time), "\"", collapse = ", "), ".", call. = FALSE)
  }
  units
}
