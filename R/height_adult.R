# Predicted adult height: the PediTools engine's Calc\AdultHeight (phase 25 C), Bayley-Pinneau and Khamis-Roche,
# from PediTools' tables of the two methods (built into the package data).

# Bayley-Pinneau's tables by the bone age's lead or lag (one year or more, in months of 30.4375 days)
bp_lead_months <- 12
kr_years <- c(4, 17.5)

#' Read Greulich-Pyle bone ages
#'
#' Bone ages as typed on the PediTools predicted adult height calculator, in years and months as Greulich-Pyle's
#' standards are written: `"11-6"`, `"11y 6m"`, `"11 y 6 m"`, `"11 years 6 months"`, or whole years `"11"`. A decimal
#' (`"11.6"`: 11.6 years, or 11 years 6 months?) is refused, as are months over 11. Vectorised.
#'
#' @param text Character vector of bone ages.
#' @return Integer months; `NA` for a blank or `NA` element, and, with a warning, for one that cannot be read.
#' @seealso [growth_height_adult()].
#' @export
#' @examples
#' growth_bone_age(c("11-6", "11y 6m", "13", "7-10"))
#' growth_bone_age("11.6") # NA, with a warning: years and months only
growth_bone_age <- function(text) {
  if (!is.character(text) && !all(is.na(text))) stop("`text` must be a character vector.", call. = FALSE)
  s <- tolower(trimws(gsub("[\u2010-\u2015\u2212]", "-", as.character(text)), whitespace = "[ \t\r\n\v\f]"))
  out <- rep(NA_integer_, length(s))
  pats <- c(
    "^(\\d{1,2})\\s*-\\s*(\\d{1,2})$",
    "^(\\d{1,2})\\s*(?:y|yr|yrs|year|years)\\s*(?:(\\d{1,2})\\s*(?:m|mo|mos|month|months))?$",
    "^(\\d{1,2})()$"
  )
  bad <- !is.na(s) & s != ""
  for (pat in pats) {
    hit <- bad & grepl(pat, s, perl = TRUE)
    if (any(hit)) {
      years <- as.integer(sub(pat, "\\1", s[hit], perl = TRUE))
      m <- sub(pat, "\\2", s[hit], perl = TRUE)
      months <- ifelse(m == "", 0L, suppressWarnings(as.integer(m)))
      ok <- months <= 11L
      out[which(hit)[ok]] <- years[ok] * 12L + months[ok]
      bad[which(hit)[ok]] <- FALSE
    }
  }
  if (any(bad)) {
    warning("Could not read bone age", if (sum(bad) > 1) "s", " ",
      paste0("\"", unique(text[bad]), "\"", collapse = ", "),
      ": enter years and months, e.g. 11-6 or 11y 6m (months 0 to 11).",
      call. = FALSE
    )
  }
  out
}

#' Predicted adult height
#'
#' A child's predicted adult height by Bayley-Pinneau (from a Greulich-Pyle bone age) and Khamis-Roche (from weight
#' and both parents' heights, without a bone age), as on the PediTools predicted adult height calculator. Vectorised
#' over every argument.
#'
#' - **Bayley-Pinneau** (Bayley and Pinneau 1952): adult height = height / (% of mature height at the bone age) x 100,
#'   the percentages of the paper's Tables IIA-IIE (boys) and IIIA-IIIF (girls). The table follows the bone age's
#'   lead or lag on the age (months of 30.4375 days): within a year, average; a year or more ahead, accelerated; a
#'   year or more behind, retarded. Between the bone ages printed, the percentage is interpolated linearly. Where the
#'   matching table does not cover the bone age (e.g. boys retarded beyond 13-0), the average table is used and
#'   `substituted` is `TRUE` (the website warns); outside the average table too, there is no prediction.
#' - **Khamis-Roche** (Khamis and Roche 1994, with the coefficients of its 1995 erratum): adult stature (in) =
#'   \eqn{\beta_0 + \beta_1 stature + \beta_2 weight + \beta_3 midparent}, stature and the mid-parent stature
#'   \eqn{(father + mother)/2} in inches, weight in pounds, the coefficients by half year from 4 to 17.5 years
#'   (interpolated linearly between half years; never outside). White US children of the Fels Longitudinal Study.
#'   `mad_in` and `p90_in` are its 50% (median absolute deviation) and 90% error bounds for the age, **read from the
#'   paper's Figures 1 and 2** (the paper tabulates only their averages: boys 0.851 and 2.101 in, girls 0.657 and
#'   1.675 in).
#'
#' @param sex `"m"`, `"f"`, `"male"` or `"female"`, in any case. Other values are `NA` (with a warning).
#' @param age_days Age in days (to convert: [growth_age_days()], [growth_convert()]).
#' @param height Height in cm.
#' @param bone_age_months Optional: the Greulich-Pyle bone age in whole months (to read "11-6" and the like:
#'   [growth_bone_age()]); gives Bayley-Pinneau. `NA` or `NULL` for none.
#' @param weight,father,mother Optional: weight in kg and the parents' heights in cm; all three give Khamis-Roche.
#'   `NA` or `NULL` for none.
#' @return A data frame, one row per element (after recycling) and method given: `khamis_roche` when any of
#'   `weight`, `father`, `mother` is given, then `bayley_pinneau` when a bone age is.
#'   \describe{
#'     \item{row}{The element's position in the input.}
#'     \item{sex, age_days, height, bone_age_months, weight, father, mother}{The input (`sex` as `"m"` or `"f"`).}
#'     \item{method}{`"khamis_roche"` or `"bayley_pinneau"`.}
#'     \item{predicted}{The predicted adult height, cm.}
#'     \item{table_kind, table_used}{Bayley-Pinneau: the table the bone age calls for (`"average"`, `"accelerated"`,
#'       `"retarded"`) and the one used.}
#'     \item{lead_months}{Bayley-Pinneau: the bone age minus the age, months.}
#'     \item{percent}{Bayley-Pinneau: the percentage of mature height used.}
#'     \item{substituted}{Bayley-Pinneau: whether the average table stood in for the matching one.}
#'     \item{predicted_in, mad_in, p90_in}{Khamis-Roche: the prediction in inches, and the 50% and 90% error bounds
#'       for the age (in, read from the paper's figures).}
#'     \item{status}{`"ok"`; `"outside"` (Bayley-Pinneau: the bone age outside every table; Khamis-Roche: the age
#'       outside 4 to 17.5 years); `"invalid"` (sex, the age or the height missing or not valid; a bone age not in
#'       whole months; Khamis-Roche without all three of weight and both parents' heights).}
#'   }
#' @references
#' Bayley N, Pinneau SR. Tables for predicting adult height from skeletal age: revised for use with the
#' Greulich-Pyle hand standards. J Pediatr 1952;40:423-441.
#'
#' Khamis HJ, Roche AF. Predicting adult stature without using skeletal age: the Khamis-Roche method. Pediatrics
#' 1994;94:504-507. Erratum: Pediatrics 1995;95:457.
#' @seealso The PediTools predicted adult height calculator, <https://peditools.org/>, and its section of the Sources
#'   and methods page.
#' @export
#' @examples
#' # A 13-year-old boy, bone age 13-3, 60 in (152.4 cm): Bayley-Pinneau's own example, 67.4 in
#' r <- growth_height_adult("m", growth_convert(13, "years", "days"), 152.4, growth_bone_age("13-3"))
#' r[c("method", "predicted", "table_used", "percent")]
#' r$predicted / 2.54
#'
#' # Both methods: a boy of 10, 139.7 cm, 31.8 kg, parents 177.8 and 167.6 cm, bone age 10-0
#' r <- growth_height_adult("m", 3652.5, 139.7,
#'   bone_age_months = 120, weight = 31.8, father = 177.8, mother = 167.6
#' )
#' r[c("method", "predicted", "mad_in", "p90_in", "status")]
#'
#' # A bone age the matching table does not cover: the average table, flagged
#' r <- growth_height_adult("m", growth_convert(15, "years", "days"), 160, growth_bone_age("13-6"))
#' r[c("table_kind", "table_used", "substituted", "predicted")]
growth_height_adult <- function(sex, age_days, height, bone_age_months = NULL, weight = NULL, father = NULL,
                                mother = NULL) {
  if (is.null(bone_age_months)) bone_age_months <- NA_real_
  if (is.null(weight)) weight <- NA_real_
  if (is.null(father)) father <- NA_real_
  if (is.null(mother)) mother <- NA_real_
  a <- recycle(
    sex = sex, age_days = age_days, height = height, bone_age_months = bone_age_months, weight = weight,
    father = father, mother = mother
  )
  for (v in c("age_days", "height", "bone_age_months", "weight", "father", "mother")) {
    if (!is.numeric(a[[v]]) && !all(is.na(a[[v]]))) stop("`", v, "` must be numeric.", call. = FALSE)
    a[[v]] <- as.numeric(a[[v]])
  }
  sex <- parse_sex(a$sex)
  base_bad <- is.na(sex) | !is.finite(a$age_days) | a$age_days < 0 | !is.finite(a$height) | a$height <= 0
  base_bad[is.na(base_bad)] <- TRUE
  hp <- builtin_data$height_prediction

  # Khamis-Roche: when any of its inputs is given
  kr_given <- !is.na(a$weight) | !is.na(a$father) | !is.na(a$mother)
  kr <- data.frame(row = which(kr_given), method = rep("khamis_roche", sum(kr_given)), stringsAsFactors = FALSE)
  kr$predicted <- kr$predicted_in <- kr$mad_in <- kr$p90_in <- rep(NA_real_, nrow(kr))
  kr$status <- rep(NA_character_, nrow(kr))
  for (k in seq_len(nrow(kr))) {
    i <- kr$row[k]
    kr_inputs <- c(a$weight[i], a$father[i], a$mother[i])
    if (base_bad[i] || !all(is.finite(kr_inputs)) || any(kr_inputs <= 0)) {
      kr$status[k] <- "invalid"
      next
    }
    years <- a$age_days[i] / 365.25
    if (years < kr_years[1] - 1e-9 || years > kr_years[2] + 1e-9) {
      kr$status[k] <- "outside"
      next
    }
    cf <- adult_interpolate(hp$khamis_roche[hp$khamis_roche$sex == sex[i], ], years)
    er <- adult_interpolate(hp$khamis_roche_errors[hp$khamis_roche_errors$sex == sex[i], ], years)
    midparent <- (a$father[i] + a$mother[i]) / 2
    inches <- cf[["intercept"]] + cf[["stature_in"]] * (a$height[i] * 1 / 2.54) +
      cf[["weight_lb"]] * (a$weight[i] * 1000 * 1 / 453.59237) + cf[["midparent_in"]] * (midparent * 1 / 2.54)
    kr$predicted_in[k] <- inches
    kr$predicted[k] <- inches * 2.54
    kr$mad_in[k] <- er[["mad_in"]]
    kr$p90_in[k] <- er[["p90_in"]]
    kr$status[k] <- "ok"
  }

  # Bayley-Pinneau: when a bone age is given
  bp_given <- !is.na(a$bone_age_months)
  bp <- data.frame(row = which(bp_given), method = rep("bayley_pinneau", sum(bp_given)), stringsAsFactors = FALSE)
  bp$predicted <- bp$lead_months <- bp$percent <- rep(NA_real_, nrow(bp))
  bp$table_kind <- bp$table_used <- bp$status <- rep(NA_character_, nrow(bp))
  bp$substituted <- rep(NA, nrow(bp))
  for (k in seq_len(nrow(bp))) {
    i <- bp$row[k]
    bone <- a$bone_age_months[i]
    if (base_bad[i] || !is.finite(bone) || bone < 0 || bone != round(bone)) {
      bp$status[k] <- "invalid"
      next
    }
    tables <- hp$bayley_pinneau[hp$bayley_pinneau$sex == sex[i], ]
    lead <- bone - a$age_days[i] / 30.4375
    kind <- if (lead >= bp_lead_months - 1e-9) {
      "accelerated"
    } else if (lead <= -bp_lead_months + 1e-9) {
      "retarded"
    } else {
      "average"
    }
    bp$table_kind[k] <- kind
    bp$lead_months[k] <- lead
    used <- kind
    percent <- bp_percent_at(tables[tables$kind == kind, ], bone)
    substituted <- FALSE
    if (is.na(percent) && kind != "average") {
      used <- "average"
      percent <- bp_percent_at(tables[tables$kind == "average", ], bone)
      substituted <- !is.na(percent)
    }
    if (is.na(percent)) {
      bp$status[k] <- "outside"
      next
    }
    bp$table_used[k] <- used
    bp$percent[k] <- percent
    bp$substituted[k] <- substituted
    bp$predicted[k] <- a$height[i] / percent * 100
    bp$status[k] <- "ok"
  }

  cols <- c(
    "row", "method", "predicted", "table_kind", "table_used", "lead_months", "percent", "substituted",
    "predicted_in", "mad_in", "p90_in", "status"
  )
  fill <- function(d) {
    for (c in setdiff(cols, names(d))) {
      d[[c]] <- rep(if (c %in% c("table_kind", "table_used")) NA_character_ else NA, nrow(d))
    }
    d[cols]
  }
  out <- rbind(fill(kr), fill(bp))
  out <- out[order(out$row, out$method != "khamis_roche"), , drop = FALSE]
  out$substituted <- as.logical(out$substituted)
  for (c in c("predicted", "lead_months", "percent", "predicted_in", "mad_in", "p90_in")) {
    out[[c]] <- as.numeric(out[[c]])
  }
  r <- out$row
  res <- data.frame(
    row = r, sex = sex[r], age_days = a$age_days[r], height = a$height[r], bone_age_months = a$bone_age_months[r],
    weight = a$weight[r], father = a$father[r], mother = a$mother[r], stringsAsFactors = FALSE
  )
  res <- cbind(res, out[setdiff(cols, "row")])
  rownames(res) <- NULL
  res
}

# A Bayley-Pinneau table's percentage at a bone age (months), linear between its columns; NA outside it
bp_percent_at <- function(columns, months) {
  columns <- columns[order(columns$bone_age_months), ]
  m <- columns$bone_age_months
  p <- columns$percent
  n <- length(m)
  if (months < m[1] || months > m[n]) {
    return(NA_real_)
  }
  for (i in seq_len(n - 1)) {
    if (months <= m[i + 1]) {
      if (months == m[i]) {
        return(p[i])
      }
      return(p[i] + (p[i + 1] - p[i]) * (months - m[i]) / (m[i + 1] - m[i]))
    }
  }
  p[n]
}

# Rows by age (column `age`), every other numeric column interpolated linearly at x, as the engine does
adult_interpolate <- function(rows, x) {
  rows <- rows[order(rows$age), ]
  vals <- setdiff(names(rows), c("sex", "age"))
  n <- nrow(rows)
  for (i in seq_len(n - 1)) {
    if (x <= rows$age[i + 1] + 1e-9) {
      t <- (x - rows$age[i]) / (rows$age[i + 1] - rows$age[i])
      t <- max(0, min(1, t))
      return(vapply(vals, function(v) rows[[v]][i] + (rows[[v]][i + 1] - rows[[v]][i]) * t, numeric(1)))
    }
  }
  vapply(vals, function(v) rows[[v]][n], numeric(1))
}
