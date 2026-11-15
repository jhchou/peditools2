# WHO growth increments, 0 to 24 months (WHO Child Growth Standards: growth velocity, 2009): the PediTools engine's
# Calc\WhoIncrements (phase 25 D). The parameters: WHO's 2009 increment tables, in the package data.

incr_measures <- c(weight = "weight", head_circ = "head_circ", length = "length")
incr_units <- c(weight = "g", head_circ = "cm", length = "cm")
incr_max_months <- 24
# The due date (40 0/7 weeks) and 37 0/7 weeks: below it, the results by corrected age too
incr_due_days <- 280
incr_preterm_days <- 259

# PHP's round() of a non-negative number (half away from zero; R's round() goes to even)
round_half_up <- function(x) floor(x + 0.5)

# WHO's target age in days for a whole month: 1 month is 4 weeks (28 days), other months round(m x 30.4375)
incr_target_days <- function(month) ifelse(month == 1, 28, round_half_up(month * 30.4375))

# WHO's tolerance around a target month (WHO 2009, Table 2; 6 and 12 months take the stricter range)
incr_tolerance <- function(month) ifelse(month <= 6, 3, ifelse(month <= 12, 5, 7))

# An age in days on WHO's month scale: piecewise linear through the target ages; NA beyond 24 months or below 0
incr_month_scale <- function(days) {
  knots <- incr_target_days(0:incr_max_months)
  out <- rep(NA_real_, length(days))
  ok <- !is.na(days) & days >= 0 & days <= knots[incr_max_months + 1] + 1e-9
  out[ok] <- stats::approx(knots, 0:incr_max_months, pmin(days[ok], knots[incr_max_months + 1]))$y
  out
}

# WHO's Z of an increment y with parameters p (L, M, S, delta): the restricted LMS method, weight shifted by delta; a
# shifted increment of 0 or below takes the linear part below -3 SD where that cut-off exists, else NA
incr_z <- function(y, p) {
  x <- y + p[["delta"]]
  l <- p[["L"]]
  m <- p[["M"]]
  s <- p[["S"]]
  if (x > 0) {
    z <- lms_z(x, l, m, s)
    if (abs(z) <= 3) {
      return(z)
    }
    sign <- if (z > 0) 1 else -1
    sd3 <- lms_y(3 * sign, l, m, s)
    sd2 <- lms_y(2 * sign, l, m, s)
    return(3 * sign + (x - sd3) / abs(sd3 - sd2))
  }
  sd3 <- lms_y(-3, l, m, s)
  sd2 <- lms_y(-2, l, m, s)
  if (is.na(sd3) || is.na(sd2) || !(sd3 > 0)) {
    return(NA_real_)
  }
  -3 + (x - sd3) / (sd2 - sd3)
}

# The rows of one measure and sex, split by interval length (a data frame per length: start, interval, L, M, S, delta)
incr_tables <- function(measure, sex) {
  d <- builtin_data$who_increments
  d <- d[d$measure == measure & d$sex == sex, ]
  d <- d[order(d$length_months, d$start_months), ]
  split(d, d$length_months)
}

# One length's parameters at a start month, interpolated linearly between whole months; NULL outside its starts
incr_along_start <- function(rows, s) {
  if (s < min(rows$start_months) - 1e-9 || s > max(rows$start_months) + 1e-9) {
    return(NULL)
  }
  lo <- floor(s + 1e-9)
  last <- max(rows$start_months)
  pick <- function(month) unlist(rows[rows$start_months == month, c("L", "M", "S", "delta")])
  if (lo >= last) {
    return(pick(last))
  }
  pick(lo) + (s - lo) * (pick(lo + 1) - pick(lo))
}

# One measure's results between two visits, as Calc\WhoIncrements::calculate(): a data frame of result rows
incr_measure <- function(measure, sex, days1, y1, days2, y2) {
  gain <- y2 - y1
  result <- function(kind, p, length, start, used, label = NA_character_, between = NA_character_) {
    z <- incr_z(used, p)
    data.frame(
      kind = kind, interval = label, start_months = start, end_months = start + length, length_months = length,
      between_lengths = between, gain = gain, gain_used = used, L = p[["L"]], M = p[["M"]], S = p[["S"]],
      delta = p[["delta"]], z = z, p = stats::pnorm(z), status = if (is.na(z)) "no_z" else "ok",
      stringsAsFactors = FALSE
    )
  }
  none <- function(status) {
    data.frame(
      kind = NA_character_, interval = NA_character_, start_months = NA_real_, end_months = NA_real_,
      length_months = NA_real_, between_lengths = NA_character_, gain = gain, gain_used = NA_real_, L = NA_real_,
      M = NA_real_, S = NA_real_, delta = NA_real_, z = NA_real_, p = NA_real_, status = status,
      stringsAsFactors = FALSE
    )
  }
  s1 <- incr_month_scale(days1)
  s2 <- incr_month_scale(days2)
  if (is.na(s1) || is.na(s2)) {
    return(none("outside"))
  }
  tables <- incr_tables(measure, sex)
  lengths <- as.numeric(names(tables))
  out <- list()

  # WHO, as published: every interval the two visits meet within the tolerance
  for (k in seq_along(tables)) {
    rows <- tables[[k]]
    for (j in seq_len(nrow(rows))) {
      start <- rows$start_months[j]
      end <- start + lengths[k]
      fits <- abs(days1 - incr_target_days(start)) <= incr_tolerance(start) + 1e-9 &&
        abs(days2 - incr_target_days(end)) <= incr_tolerance(end) + 1e-9
      if (fits) {
        out[[length(out) + 1]] <- result("published", unlist(rows[j, c("L", "M", "S", "delta")]), lengths[k], start,
          gain,
          label = rows$interval[j]
        )
      }
    }
  }

  # WHO's suggestion, when no interval matched, for an interval within a month of WHO's shortest to longest length
  observed <- s2 - s1
  if (!length(out) && observed >= min(lengths) - 1 + 1e-9 && observed <= max(lengths) + 1 - 1e-9) {
    start <- round_half_up(s1)
    best <- NA
    for (k in seq_along(tables)) {
      if (!any(tables[[k]]$start_months == start)) next
      l <- lengths[k]
      nearer <- is.na(best) || abs(l - observed) < abs(best - observed) - 1e-9 ||
        (abs(abs(l - observed) - abs(best - observed)) <= 1e-9 && l > best) # a tie: the longer
      if (nearer) {
        best <- l
      }
    }
    if (!is.na(best)) {
      rows <- tables[[as.character(best)]]
      row <- rows[rows$start_months == start, ]
      p <- unlist(row[c("L", "M", "S", "delta")])
      end <- start + best
      who_days <- incr_target_days(end) - incr_target_days(start)
      if (abs((days2 - days1) - who_days) <= incr_tolerance(end) + 1e-9) {
        out[[length(out) + 1]] <- result("tabulated", p, best, start, gain, label = row$interval)
      } else {
        out[[length(out) + 1]] <- result("prorated", p, best, start, gain * who_days / (days2 - days1),
          label = row$interval
        )
      }
    }
  }

  # Interpolated: along the start month for the WHO lengths either side, then between those lengths
  if (observed >= lengths[1] - 1e-9 && observed <= max(lengths) + 1e-9) {
    below <- lengths[1]
    above <- max(lengths)
    for (l in lengths) {
      if (l <= observed + 1e-9) below <- l
      if (l >= observed - 1e-9 && l < above + 1e-9) {
        above <- l
        break
      }
    }
    p_below <- incr_along_start(tables[[as.character(below)]], s1)
    p_above <- if (above == below) p_below else incr_along_start(tables[[as.character(above)]], s1)
    if (!is.null(p_below) && !is.null(p_above)) {
      w <- if (above == below) 0 else (observed - below) / (above - below)
      p <- p_below + w * (p_above - p_below)
      out[[length(out) + 1]] <- result("interpolated", p, observed, s1, gain,
        between = if (above == below) NA_character_ else paste0(below, "-", above)
      )
    }
  }
  if (!length(out)) {
    return(none("no_interval"))
  }
  do.call(rbind, out)
}

#' WHO growth increments
#'
#' The gain in weight, length and head circumference between two visits from birth to 24 months, with its Z-score on
#' the WHO growth velocity standards (2009), as on the PediTools WHO growth increments calculator. Vectorised over
#' every argument.
#'
#' WHO tabulates fixed intervals starting at whole months: weight over 1 month (to 12 months) and 2, 3, 4 and 6
#' months; length over 2, 3, 4 and 6 months; head circumference over 2 and 3 months (to 12 months) and 4 and 6
#' months. WHO's target ages are 4 weeks (28 days) for 1 month and `round(m * 30.4375)` days for the others. Each
#' measure gives up to three kinds of result:
#'
#' - `published`: every interval whose start and end the visits meet within WHO's tolerance of the target ages
#'   (\eqn{\pm}3 days to 6 months, \eqn{\pm}5 to 12, \eqn{\pm}7 to 24; WHO 2009, Table 2), the gain as measured.
#' - Otherwise WHO's suggestion (WHO 2009, Discussion), for an interval within a month of WHO's shortest to longest
#'   length: the interval from the whole month nearest the earlier visit, with the WHO length nearest the observed one
#'   (on a tie, the longer); `tabulated` when the observed length is within the tolerance of that length (the gain as
#'   measured), else `prorated` (the gain scaled to the interval's days, assuming constant growth: `gain_used`).
#' - `interpolated`: the visits' ages on WHO's month scale (linear between the target ages), L, M, S and delta
#'   interpolated linearly along the start month (WHO's "more precise option") for the WHO lengths either side of the
#'   observed length, then between those lengths (PediTools' own extension, not WHO's: `between_lengths`). Never beyond
#'   WHO's tables.
#'
#' Z-scores by WHO's restricted LMS method (WHO 2009, Ch. 6), weight increments shifted by delta. A shifted increment
#' of 0 or below lies below -3 SD and takes the linear part there; in the second year WHO's length and head
#' circumference distributions reach 0 before -3 SD (\eqn{1 - 3LS \le 0}), and there a gain of 0 or below has no
#' Z-score (`status` `"no_z"`).
#'
#' With a gestational age at birth below 37 weeks the results by corrected age follow those by chronological age;
#' a corrected age before the due date has none (`status` `"before_due"`). A measure is assessed when it is given at
#' both visits.
#'
#' @param sex `"m"`, `"f"`, `"male"` or `"female"`, in any case. Other values are `NA` (with a warning).
#' @param age1_days,age2_days The earlier and the latest visit's age in days (to convert: [growth_age_days()],
#'   [growth_convert()]); the latest after the earlier.
#' @param weight1,weight2 Optional: weight in kg at the two visits (the gain is reported in g, WHO's unit).
#' @param head1,head2,length1,length2 Optional: head circumference and length in cm at the two visits.
#' @param ga_weeks,ga_days Optional: the gestational age at birth, completed weeks and days (as in
#'   [growth_corrected_age()]); `NULL` or `NA` for none.
#' @return A data frame, one row per element (after recycling), age, measure and result, in that order.
#'   \describe{
#'     \item{row}{The element's position in the input.}
#'     \item{sex}{`"m"` or `"f"`.}
#'     \item{age}{`"chronological"` or `"corrected"`.}
#'     \item{age1_days, age2_days}{The visits' ages used (corrected ages for `"corrected"`).}
#'     \item{measure, units}{`"weight"` (g), `"head_circ"` or `"length"` (cm).}
#'     \item{kind}{`"published"`, `"tabulated"`, `"prorated"` or `"interpolated"`; `NA` without a result.}
#'     \item{interval}{WHO's interval label (e.g. `"2-4 mo"`, `"0 - 4 wks"`); `NA` when interpolated.}
#'     \item{start_months, end_months, length_months}{The interval on WHO's month scale (interpolated: the visits').}
#'     \item{between_lengths}{Interpolated between two WHO lengths, e.g. `"2-3"`; else `NA`.}
#'     \item{gain}{The increment as measured, in `units`.}
#'     \item{gain_used}{The increment assessed: as measured, or prorated.}
#'     \item{L, M, S, delta}{The parameters used.}
#'     \item{z, p}{WHO's Z-score and its probability.}
#'     \item{status}{`"ok"`; `"no_z"` (a gain of 0 or below where WHO's distribution has reached 0); `"outside"` (a
#'       visit beyond 0 to 24 months); `"no_interval"` (no WHO interval fits the visits, none to interpolate);
#'       `"before_due"` (the corrected age of the earlier visit before the due date); `"invalid"` (sex or an age
#'       missing or not a number, the latest visit not after the earlier one, or no measure at both visits; `measure`
#'       `NA`).}
#'   }
#' @references
#' WHO Multicentre Growth Reference Study Group. WHO Child Growth Standards: Growth velocity based on weight, length
#' and head circumference: Methods and development. Geneva: World Health Organization, 2009.
#' @seealso The PediTools WHO growth increments calculator, <https://peditools.org/>, and its section of the Sources
#'   and methods page.
#' @export
#' @examples
#' # WHO's worked example (2009, Ch. 6): a boy gaining 2200 g from 4 to 6 months, z* 3.58
#' r <- growth_increments("m", 122, 183, weight1 = 7, weight2 = 9.2)
#' r[c("kind", "interval", "gain", "z")]
#'
#' # Visits off WHO's schedule: WHO's suggestion and the interpolated result
#' r <- growth_increments("f", 73, 149, weight1 = 5.3, weight2 = 6.6, length1 = 58, length2 = 63.2)
#' r[c("measure", "kind", "interval", "gain_used", "z", "between_lengths")]
#'
#' # Born at 32 weeks: by chronological and by corrected age
#' r <- growth_increments("m", 120, 183, weight1 = 4.9, weight2 = 6.4, ga_weeks = 32)
#' r[c("age", "age1_days", "kind", "z")]
growth_increments <- function(sex, age1_days, age2_days, weight1 = NULL, weight2 = NULL, head1 = NULL, head2 = NULL,
                              length1 = NULL, length2 = NULL, ga_weeks = NULL, ga_days = 0) {
  none <- function(v) if (is.null(v)) NA_real_ else v
  a <- recycle(
    sex = sex, age1_days = age1_days, age2_days = age2_days, weight1 = none(weight1), weight2 = none(weight2),
    head1 = none(head1), head2 = none(head2), length1 = none(length1), length2 = none(length2),
    ga_weeks = none(ga_weeks), ga_days = ga_days
  )
  for (v in setdiff(names(a), "sex")) {
    if (!is.numeric(a[[v]]) && !all(is.na(a[[v]]))) stop("`", v, "` must be numeric.", call. = FALSE)
    a[[v]] <- as.numeric(a[[v]])
  }
  if (any(a$ga_weeks < 0 | a$ga_days < 0, na.rm = TRUE)) {
    stop("`ga_weeks` and `ga_days` must not be negative.", call. = FALSE)
  }
  n <- length(a$sex)
  sex <- parse_sex(a$sex)
  ga <- 7 * a$ga_weeks + a$ga_days # NA without a GA at birth
  values <- list(
    weight = cbind(a$weight1, a$weight2) * 1000, head_circ = cbind(a$head1, a$head2),
    length = cbind(a$length1, a$length2)
  )
  out <- list()
  for (i in seq_len(n)) {
    given <- names(values)[vapply(values, function(v) all(is.finite(v[i, ])), logical(1))]
    bad <- is.na(sex[i]) || !is.finite(a$age1_days[i]) || !is.finite(a$age2_days[i]) || a$age1_days[i] < 0 ||
      !(a$age2_days[i] > a$age1_days[i]) || !length(given)
    if (bad) {
      out[[length(out) + 1]] <- data.frame(
        row = i, sex = sex[i], age = "chronological", age1_days = a$age1_days[i], age2_days = a$age2_days[i],
        measure = NA_character_, units = NA_character_, kind = NA_character_, interval = NA_character_,
        start_months = NA_real_, end_months = NA_real_, length_months = NA_real_, between_lengths = NA_character_,
        gain = NA_real_, gain_used = NA_real_, L = NA_real_, M = NA_real_, S = NA_real_, delta = NA_real_,
        z = NA_real_, p = NA_real_, status = "invalid", stringsAsFactors = FALSE
      )
      next
    }
    ages <- list(chronological = c(a$age1_days[i], a$age2_days[i]))
    if (!is.na(ga[i]) && ga[i] < incr_preterm_days) {
      ages$corrected <- ages$chronological - (incr_due_days - ga[i])
    }
    for (age in names(ages)) {
      d <- ages[[age]]
      for (m in given) {
        y <- values[[m]][i, ]
        r <- if (d[1] < 0) {
          data.frame(
            kind = NA_character_, interval = NA_character_, start_months = NA_real_, end_months = NA_real_,
            length_months = NA_real_, between_lengths = NA_character_, gain = y[2] - y[1], gain_used = NA_real_,
            L = NA_real_, M = NA_real_, S = NA_real_, delta = NA_real_, z = NA_real_, p = NA_real_,
            status = "before_due", stringsAsFactors = FALSE
          )
        } else {
          incr_measure(m, sex[i], d[1], y[1], d[2], y[2])
        }
        out[[length(out) + 1]] <- cbind(
          data.frame(
            row = i, sex = sex[i], age = age, age1_days = d[1], age2_days = d[2], measure = m,
            units = unname(incr_units[m]), stringsAsFactors = FALSE
          ),
          r
        )
      }
    }
  }
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}
