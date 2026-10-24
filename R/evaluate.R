# The calculation shared by growth_z(), growth_p(), growth_y(), growth_params() and growth_detail(): one row per
# element (after recycling), with the table, the parameters at x, the method and the results.

# Phi^-1(0.95): the CDC extended BMI method applies from the 95th percentile
z95 <- stats::qnorm(0.95)
cdc_bmi_chart <- "cdc_2000_bmi"
extrapolate_modes <- c("coverage", "none", "linear")

# Recycles arguments to a common length: each has length 1 or the common length (an error otherwise).
recycle <- function(...) {
  args <- list(...)
  sizes <- lengths(args)
  n <- unique(sizes[sizes != 1L])
  if (length(n) > 1) {
    stop("Arguments must have length 1 or a common length; got lengths ",
      paste0("`", names(args), "` ", sizes, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (!length(n)) n <- 1L
  lapply(args, function(a) if (length(a) == n) a else rep(a, length.out = n))
}

check_option <- function(value, name) {
  if (!is.null(value) && !(is.logical(value) && length(value) == 1 && !is.na(value))) {
    stop("`", name, "` must be NULL, TRUE or FALSE.", call. = FALSE)
  }
}

# Validates and resolves the inputs: returns a data frame with one row per element: chart, measure, sex, x, the
# table key ("" when the row has no table: missing data) and status ("ok" or "missing").
resolve_rows <- function(x, sex, chart, measure, ...) {
  a <- recycle(x = x, sex = sex, chart = chart, measure = measure, ...)
  if (!is.character(a$chart) && !all(is.na(a$chart))) stop("`chart` must be a character vector.", call. = FALSE)
  if (!is.character(a$measure) && !all(is.na(a$measure))) stop("`measure` must be a character vector.", call. = FALSE)
  if (!is.numeric(a$x) && !all(is.na(a$x))) stop("`x` must be numeric.", call. = FALSE)
  chart <- as.character(a$chart)
  measure <- as.character(a$measure)
  tables <- pt_data()$tables
  known <- !is.na(chart) & !is.na(measure)
  check_charts(chart[known], tables)
  pairs <- unique(data.frame(chart = chart[known], measure = measure[known], stringsAsFactors = FALSE))
  missing <- pairs[!table_key(pairs$chart, pairs$measure) %in% table_key(tables$chart, tables$measure), ]
  if (nrow(missing)) {
    stop("Unknown chart/measure (", nrow(missing), "):\n",
      paste0("  ", missing$chart, " / ", missing$measure, collapse = "\n"),
      "\nSee growth_charts() for the available ids.",
      call. = FALSE
    )
  }
  sex <- parse_sex(a$sex)
  both <- table_key(chart, measure) %in% table_key(tables$chart, tables$measure)[is.na(tables$sex)]
  key <- ifelse(both, table_key(chart, measure), table_key(chart, measure, sex))
  usable <- known & (both | !is.na(sex))
  # An added chart may have a table for one sex only: none for the other
  no_table <- usable & !key %in% table_key(tables$chart, tables$measure, tables$sex)
  key[!usable | no_table] <- ""
  rows <- data.frame(
    chart = chart, measure = measure, sex = sex, x = as.numeric(a$x), table = key,
    status = ifelse(no_table, "undefined", ifelse(usable & !is.na(a$x), "ok", "missing")), stringsAsFactors = FALSE
  )
  rows$sex[both] <- NA_character_
  attr(rows, "extra") <- a[setdiff(names(a), c("x", "sex", "chart", "measure"))]
  rows
}

# Parameter rows of one table, by key (cached for the session)

table_rows <- function(key) {
  if (is.null(cache$lms)) {
    lms <- pt_data()$lms
    cache$lms <- split(lms[c("x", "L", "M", "S")], table_key(lms$chart, lms$measure, lms$sex))
  }
  cache$lms[[key]]
}

# The distribution of each row: part ("lms", "st3", "normal") and its parameters (columns of d)
dist_columns <- c("L", "M", "S", "mu", "sigma", "nu", "tau", "y_scale")

take <- function(d, i) lapply(d, function(v) v[i])

valid_dist <- function(d) {
  ok <- rep(FALSE, length(d$part))
  lms <- !is.na(d$part) & d$part == "lms"
  st3 <- !is.na(d$part) & d$part == "st3"
  normal <- !is.na(d$part) & d$part == "normal"
  ok[lms] <- valid_lms(d$L[lms], d$M[lms], d$S[lms])
  ok[st3] <- valid_st3(d$mu[st3], d$sigma[st3], d$nu[st3], d$tau[st3])
  ok[normal] <- valid_normal(d$mu[normal], d$sigma[normal]) & d$y_scale[normal] %in% c("identity", "log")
  ok
}

# Whether a row's distribution is for positive values only (LMS, and the equations on the log scale); ST3 and the
# equations on the identity scale take any finite value, as the engine
positive_only <- function(d) {
  !is.na(d$part) & (d$part == "lms" | (d$part == "normal" & !is.na(d$y_scale) & d$y_scale == "log"))
}

# Z of y by each row's own distribution (plain: LMS, or Phi^-1(F(y)))
plain_z <- function(y, d) {
  z <- rep(NA_real_, length(y))
  for (part in c("lms", "st3", "normal")) {
    i <- !is.na(d$part) & d$part == part
    if (!any(i)) next
    z[i] <- switch(part,
      lms = lms_z(y[i], d$L[i], d$M[i], d$S[i]),
      st3 = st3_z(y[i], d$mu[i], d$sigma[i], d$nu[i], d$tau[i]),
      normal = normal_z(y[i], d$mu[i], d$sigma[i], d$y_scale[i])
    )
  }
  z
}

# y at Z (z recycled to the rows of d)
plain_y <- function(z, d) {
  n <- length(d$part)
  z <- rep_len(z, n)
  y <- rep(NA_real_, n)
  for (part in c("lms", "st3", "normal")) {
    i <- !is.na(d$part) & d$part == part
    if (!any(i)) next
    y[i] <- switch(part,
      lms = lms_y(z[i], d$L[i], d$M[i], d$S[i]),
      st3 = st3_y(z[i], d$mu[i], d$sigma[i], d$nu[i], d$tau[i]),
      normal = normal_y(z[i], d$mu[i], d$sigma[i], d$y_scale[i])
    )
  }
  y
}

# WHO's restricted method on any distribution: beyond +-3, the line whose spread is the distance between the 2 and 3
# SD values (on LMS, the same arithmetic as sd23_z() and sd23_y())
restricted_z <- function(y, d) {
  z <- plain_z(y, d)
  for (sign in c(1, -1)) {
    far <- !is.na(z) & sign * z > 3
    if (!any(far)) next
    sd3 <- plain_y(3 * sign, take(d, far))
    sd2 <- plain_y(2 * sign, take(d, far))
    z[far] <- if (sign > 0) 3 + (y[far] - sd3) / (sd3 - sd2) else -3 + (y[far] - sd3) / (sd2 - sd3)
  }
  z
}

restricted_y <- function(z, d) {
  y <- plain_y(z, d)
  far <- !is.na(z) & is.finite(z) & abs(z) > 3
  if (any(far)) {
    sign <- ifelse(z[far] > 0, 1, -1)
    sd3 <- plain_y(3 * sign, take(d, far))
    sd2 <- plain_y(2 * sign, take(d, far))
    out <- sd3 + (z[far] - 3 * sign) * abs(sd3 - sd2)
    y[far] <- ifelse(!is.na(out) & (out > 0 | !positive_only(take(d, far))), out, NA_real_)
  }
  y
}

# The full calculation. Exactly one of y, z, p is given (or none, for the parameters only).
evaluate <- function(x, sex, chart, measure, y = NULL, z = NULL, p = NULL, extrapolate = "coverage",
                     sd23 = NULL, cdc_bmi_ext = NULL) {
  extrapolate <- match.arg(extrapolate, extrapolate_modes)
  check_option(sd23, "sd23")
  check_option(cdc_bmi_ext, "cdc_bmi_ext")
  given <- list(y = y, z = z, p = p)[!vapply(list(y, z, p), is.null, logical(1))]
  rows <- do.call(resolve_rows, c(list(x = x, sex = sex, chart = chart, measure = measure), given))
  input <- attr(rows, "extra")
  n <- nrow(rows)
  tables <- pt_data()$tables
  meta <- tables[match(rows$table, table_key(tables$chart, tables$measure, tables$sex)), ]

  # Methods: the chart's published one unless switched
  is_bmi <- rows$chart %in% cdc_bmi_chart
  if (isTRUE(cdc_bmi_ext) && any(!is_bmi & !is.na(rows$chart))) {
    stop("`cdc_bmi_ext = TRUE`: the CDC extended BMI method is defined only for ", cdc_bmi_chart, ", not ",
      paste(unique(rows$chart[!is_bmi & !is.na(rows$chart)]), collapse = ", "), ".",
      call. = FALSE
    )
  }
  ext <- is_bmi & !isFALSE(cdc_bmi_ext)
  if (isTRUE(sd23) && any(ext)) {
    stop("`sd23 = TRUE` conflicts with the CDC extended BMI method (`cdc_bmi_ext`, on by default for ",
      cdc_bmi_chart, "). Set `cdc_bmi_ext = FALSE` to use SD23 on this chart.",
      call. = FALSE
    )
  }
  restricted <- if (is.null(sd23)) !is.na(meta$z_method) & meta$z_method == "sd23" else rep(sd23, n)

  # The distribution at x, table by table
  d <- list(part = rep(NA_character_, n))
  for (v in dist_columns) d[[v]] <- if (v == "y_scale") rep(NA_character_, n) else rep(NA_real_, n)
  extrapolated <- beyond <- rep(FALSE, n)
  for (key in setdiff(unique(rows$table[rows$status == "ok"]), "")) {
    i <- which(rows$table == key & rows$status == "ok")
    m <- meta[i[1], ]
    if (m$family == "lms") {
      at <- params_at(rows$x[i], m, table_rows(key), c("L", "M", "S"), extrapolate)
      at$part <- rep("lms", length(i))
    } else {
      at <- model_at(rows$x[i], m, model_parts(key, m), extrapolate)
    }
    for (v in c("part", intersect(dist_columns, names(at)))) d[[v]][i] <- at[[v]]
    extrapolated[i] <- at$extrapolated
    beyond[i] <- at$beyond
    rows$status[i[!at$allowed]] <- "outside"
  }
  ok <- rows$status == "ok"
  rows$status[ok & !valid_dist(d)] <- "undefined"
  ok <- rows$status == "ok"
  for (v in names(d)) d[[v]][!ok] <- NA

  # CDC extended BMI: P95 by the chart's LMS, and the extension's sigma at the age in years
  p95 <- ext_sigma <- rep(NA_real_, n)
  ext <- ext & ok
  if (any(ext)) {
    p95[ext] <- lms_y(z95, d$L[ext], d$M[ext], d$S[ext])
    years <- rows$x[ext] * days_per_unit[["months"]] / days_per_unit[["years"]]
    ext_sigma[ext] <- ifelse(rows$sex[ext] == "m", 0.3728 + 0.5196 * years - 0.0091 * years * years,
      0.8334 + 0.3712 * years - 0.0011 * years * years
    )
  }

  # Results: the given column keeps its values; the computed ones are NA unless the status is "ok"
  status <- rows$status
  method <- ifelse(restricted, "sd23", ifelse(!is.na(meta$z_method) & meta$z_method == "cdf", "cdf", "lms"))
  y_out <- z_out <- p_out <- rep(NA_real_, n)
  flag <- function(status, bad, why) replace(status, status == "ok" & bad, why)
  z_of <- function(y) ifelse(restricted, restricted_z(y, d), plain_z(y, d))
  y_of <- function(z) ifelse(restricted, restricted_y(z, d), plain_y(z, d))
  if (!is.null(given$y)) {
    y_out <- as.numeric(input$y)
    status <- flag(status, is.na(y_out), "missing")
    status <- flag(status, !is.finite(y_out) | (y_out <= 0 & positive_only(d)), "invalid")
    ok <- status == "ok"
    z_out[ok] <- z_of(y_out)[ok]
    p_out <- stats::pnorm(z_out)
    upper <- ok & ext & y_out >= p95
    if (any(upper)) {
      e <- (y_out[upper] - p95[upper]) / ext_sigma[upper]
      p_out[upper] <- 0.9 + 0.1 * stats::pnorm(e)
      z_out[upper] <- stats::qnorm(0.1 * stats::pnorm(e, lower.tail = FALSE), lower.tail = FALSE)
      method[upper] <- "cdc_bmi_ext"
    }
    status <- flag(status, is.na(z_out), "undefined")
  } else if (!is.null(given$z) || !is.null(given$p)) {
    if (!is.null(given$z)) {
      z_out <- as.numeric(input$z)
      status <- flag(status, is.na(z_out), "missing")
      status <- flag(status, !is.finite(z_out), "invalid")
      p_out <- stats::pnorm(z_out)
      upper_tail <- stats::pnorm(z_out, lower.tail = FALSE)
    } else {
      p_out <- as.numeric(input$p)
      status <- flag(status, is.na(p_out), "missing")
      status <- flag(status, !(p_out > 0 & p_out < 1), "invalid")
      z_out[status == "ok"] <- stats::qnorm(p_out[status == "ok"])
      upper_tail <- 1 - p_out
    }
    ok <- status == "ok"
    y_out[ok] <- y_of(z_out)[ok]
    upper <- ok & ext & z_out >= z95
    if (any(upper)) {
      y_out[upper] <- p95[upper] + ext_sigma[upper] * stats::qnorm(10 * upper_tail[upper], lower.tail = FALSE)
      method[upper] <- "cdc_bmi_ext"
    }
    status <- flag(status, is.na(y_out), "undefined")
  }
  ok <- status == "ok"
  if (is.null(given$y)) y_out[!ok] <- NA_real_
  if (is.null(given$z)) z_out[!ok] <- NA_real_
  if (is.null(given$p)) p_out[!ok] <- NA_real_

  out <- data.frame(
    chart = rows$chart, measure = rows$measure, sex = rows$sex, x = rows$x, y = y_out, z = z_out, p = p_out,
    method = ifelse(ok, method, NA_character_), status = status, family = meta$family, part = d$part,
    extrapolated = extrapolated & ok, beyond_coverage = beyond & ok, stringsAsFactors = FALSE
  )
  for (v in dist_columns) out[[v]] <- d[[v]]
  out$p95 <- p95
  out$ext_sigma <- ext_sigma
  out <- out[c(setdiff(names(out), dist_columns[!dist_columns %in% used_columns(meta$family)]))]
  rownames(out) <- NULL
  out
}

# The parameter columns of the families of the charts in a call (L, M, S for LMS; mu, sigma, nu, tau for ST3; mu,
# sigma, y_scale for the equations); L, M, S when there is none.
used_columns <- function(family) {
  family <- unique(family[!is.na(family)])
  st3 <- any(grepl("st3", family))
  normal <- any(grepl("normal", family))
  out <- c(
    if (any(family == "lms") || !length(family)) c("L", "M", "S"),
    if (st3 || normal) c("mu", "sigma"),
    if (st3) c("nu", "tau"),
    if (normal) "y_scale"
  )
  dist_columns[dist_columns %in% out]
}

# One warning per call for the values that came out NA for a reason other than missing data
warn_status <- function(status, extrapolate) {
  counts <- table(status[!status %in% c("ok", "missing")])
  if (!length(counts)) {
    return(invisible())
  }
  text <- c(
    outside = paste0("x outside the range allowed by `extrapolate = \"", extrapolate, "\"`"),
    invalid = "a measurement, Z-score or probability that is not valid",
    undefined = "no value (the parameters or the method give none there)"
  )
  warning(paste0(counts, " value", ifelse(counts > 1, "s", ""), " NA: ", text[names(counts)], collapse = "; "),
    ". See growth_detail().",
    call. = FALSE
  )
}
