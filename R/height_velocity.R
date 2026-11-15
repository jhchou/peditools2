# Height velocity on Kelly 2014's annual height velocity reference: the PediTools engine's Calc\HeightVelocity
# (phase 25 B).

velocity_charts <- c(
  all = "kelly_2014", earlier = "kelly_2014_earlier", average = "kelly_2014_average",
  later = "kelly_2014_later"
)
# The earlier measurement from this age (2 years), and the interval rules in days, as lenient as calendar months
# allow (the website since 2026-11-15): 6 months from 180 days; Kelly's annual interval 333 to 398 days (11 and 13
# months); the 12-month note below 364 days. Before, months of 30.4375 days (182.625, 334.8125, 395.6875, 365.25).
velocity_from_days <- 731
velocity_min_days <- 180
velocity_note_days <- 364
velocity_annual_days <- c(333, 398)

#' Height velocity
#'
#' Height velocity between two measurements, with its SDS on Kelly 2014's reference for annual height velocity in
#' US children, as on the PediTools height velocity calculator. Vectorised over every argument.
#'
#' The velocity is \eqn{(height_2 - height_1) / (interval / 365.25)} cm/year, given from 2 years of age (the earlier
#' measurement at 731 days or later) for an interval of at least 6 months (180 days or more: the shortest 6 calendar
#' months, 181 days, less a day, as on the website). The SDS is Kelly's LMS at
#' the interval's mid-age in years, \eqn{(age_1 + age_2) / 2 / 365.25}, on `kelly_2014` (all children, Supplemental
#' Table 2a) and, with a puberty onset, also on that group's table (`kelly_2014_earlier`, `kelly_2014_average`,
#' `kelly_2014_later`; Supplemental Table 2b), each within its own mid-ages, never extrapolated. Kelly's onset groups:
#' earlier, boys before 10.2 years, girls before 9.6; average, boys 10.2 to 11.8, girls 9.6 to 11.1; later, boys
#' after 11.8, girls after 11.1.
#'
#' Kelly's reference is for annual velocity, an interval of 11 to 13 months (as calendar months, leniently: 333 to 398
#' days); outside it the SDS is still given, as on the website, with `annual` `FALSE` and `interval_note` saying which
#' side. Under 12 months is under 364 days. A velocity of 0 or below has no SDS (the
#' LMS method needs a positive value). M is printed to 0.1 cm/year in the paper, so near the end of growth, where M is
#' small, the SDS is much less precise than earlier.
#'
#' @param sex `"m"`, `"f"`, `"male"` or `"female"`, in any case. Other values are `NA` (with a warning).
#' @param age1_days,height1 The earlier measurement: age in days (to convert: [growth_age_days()],
#'   [growth_convert()]) and height in cm.
#' @param age2_days,height2 The latest measurement, likewise; after the earlier one.
#' @param onset Optional: the puberty onset, `"earlier"`, `"average"` or `"later"`; `NA` or `NULL` for none.
#' @return A data frame, one row per element (after recycling) and table: all children, then the onset's table when
#'   given.
#'   \describe{
#'     \item{row}{The element's position in the input.}
#'     \item{sex, age1_days, height1, age2_days, height2, onset}{The input (`sex` as `"m"` or `"f"`).}
#'     \item{group, chart}{`"all"` or the onset, and Kelly's chart.}
#'     \item{velocity}{cm/year; `NA` before 2 years or for an interval under 6 months (180 days).}
#'     \item{gain}{`height2 - height1`, cm.}
#'     \item{interval_days, interval_months, mid_age_days}{The interval (months of 30.4375 days) and its mid-age.}
#'     \item{z, p}{The SDS and probability on the chart.}
#'     \item{status}{`"ok"`; `"under_2_years"` (the earlier measurement before 731 days: no velocity);
#'       `"short_interval"` (under 6 months: no velocity); `"not_positive"` (a velocity of 0 or below: no SDS);
#'       `"outside"` (the mid-age outside the chart's mid-ages: no SDS); `"invalid"` (sex, an age or a height missing
#'       or not a number, or the latest measurement not after the earlier one).}
#'     \item{annual}{Whether the interval is within Kelly's 11 to 13 months, 333 to 398 days (`NA` without a
#'       velocity).}
#'     \item{interval_note}{`"under_11_months"` (under 333 days) or `"over_13_months"` (over 398 days): outside
#'       Kelly's annual interval, the website warns; `"under_12_months"` (333 to 364 days: the website notes it); or
#'       `NA`.}
#'   }
#' @references
#' Kelly A, Winer KK, Kalkwarf H, Oberfield SE, Lappe J, Gilsanz V, Zemel BS. Age-based reference ranges for annual
#' height velocity in US children. J Clin Endocrinol Metab 2014;99:2104-2112.
#' @seealso The PediTools height velocity calculator, <https://peditools.org/>, and its section of the Sources and
#'   methods page; Kelly's charts in [growth_charts()] work with [growth_z()] as any other.
#' @export
#' @examples
#' # A boy measured at 9 and 10 years, 130 and 135.5 cm: 5.5 cm/year, the median at mid-age 9.5
#' days <- growth_convert(c(9, 10), "years", "days")
#' growth_height_velocity("m", days[1], 130, days[2], 135.5)
#'
#' # With a puberty onset: all children and the average-maturing table
#' r <- growth_height_velocity("f", 4000, 140, 4380, 147.2, onset = "average")
#' r[c("group", "velocity", "z", "status")]
#'
#' # Vectorised; an 8-month interval keeps its SDS, flagged as not annual
#' d <- data.frame(
#'   sex = c("m", "f"), a1 = c(3300, 3650), h1 = c(131, 138),
#'   a2 = c(3543, 4015), h2 = c(135, 144)
#' )
#' r <- growth_height_velocity(d$sex, d$a1, d$h1, d$a2, d$h2)
#' r[c("row", "velocity", "z", "annual", "interval_note")]
growth_height_velocity <- function(sex, age1_days, height1, age2_days, height2, onset = NULL) {
  if (is.null(onset)) onset <- NA_character_
  a <- recycle(
    sex = sex, age1_days = age1_days, height1 = height1, age2_days = age2_days, height2 = height2,
    onset = onset
  )
  for (v in c("age1_days", "height1", "age2_days", "height2")) {
    if (!is.numeric(a[[v]]) && !all(is.na(a[[v]]))) stop("`", v, "` must be numeric.", call. = FALSE)
    a[[v]] <- as.numeric(a[[v]])
  }
  onset <- as.character(a$onset)
  if (!all(is.na(onset) | onset %in% c("earlier", "average", "later"))) {
    stop("`onset` must be \"earlier\", \"average\", \"later\" or NA.", call. = FALSE)
  }
  n <- length(a$sex)
  sex <- parse_sex(a$sex)

  interval <- a$age2_days - a$age1_days
  months <- interval / 30.4375
  mid <- (a$age1_days + a$age2_days) / 2
  gain <- a$height2 - a$height1
  invalid <- is.na(sex) | !is.finite(a$age1_days) | !is.finite(a$age2_days) | !is.finite(a$height1) |
    !is.finite(a$height2) | a$age1_days < 0 | !(interval > 0)
  invalid[is.na(invalid)] <- TRUE
  status <- rep(NA_character_, n)
  status[invalid] <- "invalid"
  status[!invalid & a$age1_days < velocity_from_days - 1e-9] <- "under_2_years"
  status[is.na(status) & interval < velocity_min_days - 1e-9] <- "short_interval"
  has_velocity <- is.na(status)
  velocity <- ifelse(has_velocity, gain / (interval / 365.25), NA_real_)
  note <- rep(NA_character_, n)
  note[has_velocity & interval < velocity_annual_days[1] - 1e-9] <- "under_11_months"
  note[has_velocity & interval > velocity_annual_days[2] + 1e-9] <- "over_13_months"
  note[has_velocity & is.na(note) & interval < velocity_note_days - 1e-9] <- "under_12_months"
  annual <- ifelse(has_velocity, note %in% c(NA, "under_12_months"), NA)
  status[has_velocity & velocity <= 0] <- "not_positive"

  # One row per element and table: all children, then the onset's
  rows <- c(seq_len(n), which(!is.na(onset)))
  group <- c(rep("all", n), onset[!is.na(onset)])
  ord <- order(rows, group != "all")
  rows <- rows[ord]
  group <- group[ord]
  chart <- unname(velocity_charts[group])
  z <- p <- rep(NA_real_, length(rows))
  st <- status[rows]
  todo <- which(is.na(st))
  if (length(todo)) {
    e <- evaluate(mid[rows[todo]] / 365.25, sex[rows[todo]], chart[todo], "height_velocity", y = velocity[rows[todo]])
    ok <- e$status == "ok"
    z[todo[ok]] <- e$z[ok]
    p[todo[ok]] <- e$p[ok]
    st[todo] <- ifelse(ok, "ok", "outside")
  }
  data.frame(
    row = rows, sex = sex[rows], age1_days = a$age1_days[rows], height1 = a$height1[rows],
    age2_days = a$age2_days[rows], height2 = a$height2[rows], onset = onset[rows], group = group, chart = chart,
    velocity = velocity[rows], gain = gain[rows], interval_days = interval[rows], interval_months = months[rows],
    mid_age_days = mid[rows], z = z, p = p, status = st, annual = annual[rows], interval_note = note[rows],
    stringsAsFactors = FALSE
  )
}
