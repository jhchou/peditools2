# Charts added in the session (growth_add_chart()): kept in `session`, merged with the built-in data by pt_data().

session <- new.env(parent = emptyenv())

# The tables' columns, as in the built-in data
table_columns <- c(
  "chart", "measure", "sex", "family", "x_label", "x_units", "measure_units", "x_min", "x_max", "z_method",
  "x_break", "n_rows", "on_website", "calculators", "sources_url"
)
x_labels <- c("age", "length", "height", "gestational age", "gestational age at birth")
# Relative tolerance for a coverage bound against the end row (as the PediTools engine)
coverage_tolerance <- 1e-6

#' Add your own charts for the session
#'
#' Makes charts of your own (e.g. parameter tables an author shared with you) available to every function of the
#' package until the end of the R session, with the same checks and methods as the built-in charts. Nothing is
#' saved: call it again in a new session (e.g. `growth_add_chart(read.csv("my_chart.csv"))` at the top of a
#' script).
#'
#' `params` is a data frame, or a list of data frames, in the layout of the PediTools parameter files, recognised by
#' their columns:
#' - **LMS** (the layout of [growth_table()]): `chart`, `measure`, `sex` (or `gender`; `"m"`, `"f"`, or `NA` for one
#'   table for both sexes), `x` (or `age`), `L`, `M`, `S`, and optionally `x_units` (or `age_units`) and
#'   `measure_units`.
#' - **ST3** (the layout of [growth_table()]): `chart`, `measure`, `sex`, `x`, `mu`, `sigma`, `nu`, `tau`, and
#'   optionally `x_units` and `measure_units`.
#' - **Equations** of a normal model (the layout of [growth_equations()]): `chart`, `measure`, `sex`,
#'   `x_from`, `param` (`mu` or `sigma`), `coef`, `power`, `log_power`, `male`, `x_scale`, `transform`, `y_scale`,
#'   and optionally `model` (`"normal"`), `x_units` and `measure_units`.
#'
#' A chart may combine an ST3 table and equations (parts used from their start, as INTERGROWTH-21st newborn size).
#'
#' `metadata` (optional) is a data frame with a row per chart (`measure` `NA`), and optionally rows for one measure
#' or one sex that override it: `chart`, `measure`, `sex`, `x_label` (default `"age"`), `x_min` and `x_max` (or
#' `age_min`, `age_max`; the published coverage, by default the table's first and last rows; required for
#' equations), `z_method` (`"lms"` (default) or `"sd23"` for LMS tables; `"cdf"` for the others), `x_break` (an x
#' between two rows where the table steps), `sources_url` (a page describing the chart's sources and methods), and
#' `x_units`, `measure_units` when `params` has none. These are the fields of [growth_charts()]; `on_website` and
#' `calculators` describe the PediTools calculators, so they are `FALSE` and `NA` for added charts.
#'
#' `references` (optional) is a data frame in the layout of [growth_references()]: `chart`, `measure` (`NA`: every
#' measure of the chart), `role` (`"clinical"` (default), `"lms_data"`, `"method"` or `"resource"`), `citation`
#' (required), and optionally `short` (default: the citation), `position` (default: the order given within each
#' role), `id`, `doi`, `pmid` and `url`.
#'
#' As for the built-in charts, the coverage may reach at most half a row step beyond a table's end row (use
#' `extrapolate = "linear"` to go further); rows must increase in x; M and S (sigma, nu, tau) must be positive.
#'
#' @param params A data frame of parameters, or a list of them (see Details).
#' @param metadata A data frame of chart information (see Details), or `NULL`.
#' @param references A data frame of the charts' publications (see Details), or `NULL`.
#' @param overwrite `TRUE` to replace a chart of the same id (built-in, or added before); `FALSE` (default): an
#'   error.
#' @return The added charts' rows of [growth_charts()], invisibly.
#' @seealso [growth_remove_chart()]; [growth_charts()] (column `added`).
#' @export
#' @examples
#' # An LMS table of your own (here the first rows of CDC's, as an example)
#' my <- growth_table("cdc_2000_infant", "weight", "f")[1:12, ]
#' my$chart <- "my_infant_chart"
#' meta <- data.frame(chart = "my_infant_chart", x_units = "months", measure_units = "kg")
#' refs <- data.frame(chart = "my_infant_chart", citation = "Author A. My chart. J Growth 2026;1:9.")
#' growth_add_chart(my, meta, refs)
#' growth_z(4.1, 1.2, "f", "my_infant_chart", "weight")
#' growth_references("my_infant_chart")
#' growth_remove_chart("my_infant_chart")
growth_add_chart <- function(params, metadata = NULL, references = NULL, overwrite = FALSE) {
  check_flag(overwrite, "overwrite")
  if (is.data.frame(params)) params <- list(params)
  if (!is.list(params) || !length(params) || !all(vapply(params, is.data.frame, logical(1)))) {
    stop("`params` must be a data frame or a list of data frames.", call. = FALSE)
  }
  if (!is.null(metadata) && !is.data.frame(metadata)) {
    stop("`metadata` must be a data frame or NULL.", call. = FALSE)
  }
  parsed <- list(lms = NULL, st3 = NULL, equations = NULL)
  for (d in params) {
    kind <- param_kind(d)
    parsed[[kind]] <- rbind(parsed[[kind]], normalise_params(d, kind))
  }
  charts <- unique(unlist(lapply(parsed, function(d) d$chart)))
  if (anyNA(charts) || any(!nzchar(charts))) stop("Every row needs a chart id.", call. = FALSE)
  if (!is.null(metadata)) metadata <- normalise_metadata(metadata, charts)

  existing <- intersect(charts, unique(pt_data()$tables$chart))
  if (length(existing) && !overwrite) {
    stop("Chart", if (length(existing) > 1) "s", " already available: ", paste(existing, collapse = ", "),
      ". Use `overwrite = TRUE` to replace ", if (length(existing) > 1) "them" else "it", ".",
      call. = FALSE
    )
  }

  built <- build_tables(parsed, metadata)
  built$references <- normalise_references(references, built$tables)
  for (part in session_parts) {
    old <- session[[part]]
    if (!is.null(old)) old <- old[!old$chart %in% charts, , drop = FALSE]
    session[[part]] <- rbind(old, built[[part]])
  }
  reset_cache()
  invisible(growth_charts(charts))
}

#' Remove added charts
#'
#' Removes charts added with [growth_add_chart()] in this session; a built-in chart they replaced is available
#' again.
#'
#' @param chart Chart ids added in the session.
#' @return The ids removed, invisibly.
#' @export
#' @examples
#' my <- growth_table("cdc_2000_infant", "weight", "f")[1:12, ]
#' my$chart <- "my_infant_chart"
#' growth_add_chart(my, data.frame(chart = "my_infant_chart", x_units = "months"))
#' growth_remove_chart("my_infant_chart")
growth_remove_chart <- function(chart) {
  if (!is.character(chart)) stop("`chart` must be a character vector of chart ids.", call. = FALSE)
  added <- unique(session$tables$chart)
  unknown <- setdiff(chart, added)
  if (length(unknown)) {
    stop("Not added in this session: ", paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  for (part in session_parts) {
    if (!is.null(session[[part]])) session[[part]] <- session[[part]][!session[[part]]$chart %in% chart, , drop = FALSE]
  }
  reset_cache()
  invisible(chart)
}

session_parts <- c("tables", "lms", "st3", "equations", "references")
reference_roles <- c("clinical", "lms_data", "method", "resource")

# The references of the added charts, in the layout of the built-in ones: one row per chart, measure and reference
normalise_references <- function(r, tables) {
  columns <- c("chart", "measure", "role", "position", "id", "short", "citation", "doi", "pmid", "url")
  if (is.null(r)) {
    return(NULL)
  }
  if (!is.data.frame(r)) stop("`references` must be a data frame or NULL.", call. = FALSE)
  r <- as.data.frame(r, stringsAsFactors = FALSE)
  extra <- setdiff(names(r), columns)
  if (length(extra)) stop("Unknown `references` columns: ", paste(extra, collapse = ", "), ".", call. = FALSE)
  if (!all(c("chart", "citation") %in% names(r))) {
    stop("`references` needs the columns `chart` and `citation`.", call. = FALSE)
  }
  for (column in setdiff(columns, names(r))) r[[column]] <- NA
  for (column in setdiff(columns, "position")) {
    r[[column]] <- as.character(r[[column]])
    r[[column]][!is.na(r[[column]]) & !nzchar(r[[column]])] <- NA_character_
  }
  if (anyNA(r$citation)) stop("Every reference needs a `citation`.", call. = FALSE)
  unknown <- setdiff(r$chart, tables$chart)
  if (length(unknown)) stop("`references` for charts not in `params`: ", paste(unknown, collapse = ", "), call. = FALSE)
  r$role[is.na(r$role)] <- "clinical"
  if (!all(r$role %in% reference_roles)) {
    stop("`references$role` must be one of ", paste(reference_roles, collapse = ", "), ".", call. = FALSE)
  }
  pairs <- unique(tables[c("chart", "measure")])
  bad <- !is.na(r$measure) & !table_key(r$chart, r$measure) %in% table_key(pairs$chart, pairs$measure)
  if (any(bad)) {
    stop("`references` for a measure the chart does not have: ", r$chart[bad][1], " / ", r$measure[bad][1],
      call. = FALSE
    )
  }
  r$short[is.na(r$short)] <- r$citation[is.na(r$short)]
  r$id[is.na(r$id)] <- paste0(r$chart[is.na(r$id)], "_ref", which(is.na(r$id)))
  # A reference for the chart (measure NA) applies to each of its measures
  out <- do.call(rbind, lapply(seq_len(nrow(r)), function(i) {
    measures <- if (is.na(r$measure[i])) pairs$measure[pairs$chart == r$chart[i]] else r$measure[i]
    row <- r[rep(i, length(measures)), columns]
    row$measure <- measures
    row
  }))
  given <- !is.na(out$position)
  out$position <- as.integer(out$position)
  out <- out[order(out$chart, out$measure, match(out$role, reference_roles), !given, out$position), ]
  group <- paste(out$chart, out$measure, out$role)
  out$position <- as.integer(stats::ave(seq_along(group), group, FUN = seq_along))
  rownames(out) <- NULL
  out[columns]
}

reset_cache <- function() {
  rm(list = ls(cache), envir = cache)
}

# Which layout a data frame of parameters has
param_kind <- function(d) {
  has <- function(columns) all(columns %in% names(d))
  if (has(c("L", "M", "S"))) {
    return("lms")
  }
  if (has(c("mu", "sigma", "nu", "tau"))) {
    return("st3")
  }
  if (has(c("x_from", "param", "coef"))) {
    return("equations")
  }
  stop("A data frame of `params` is not in a known layout: LMS (chart, measure, sex, x, L, M, S), ST3 (chart, ",
    "measure, sex, x, mu, sigma, nu, tau) or equations (chart, measure, sex, x_from, param, coef, power, log_power, ",
    "male, x_scale, transform, y_scale).",
    call. = FALSE
  )
}

# The columns of one layout, with the aliases age, age_units and gender (for x, x_units and sex)
normalise_params <- function(d, kind) {
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  alias <- c(age = "x", age_units = "x_units", gender = "sex")
  for (from in names(alias)) {
    if (from %in% names(d) && !alias[[from]] %in% names(d)) names(d)[names(d) == from] <- alias[[from]]
  }
  numeric <- switch(kind,
    lms = c("x", "L", "M", "S"),
    st3 = c("x", "mu", "sigma", "nu", "tau"),
    equations = c("x_from", "coef", "power", "log_power", "male", "x_scale")
  )
  text <- switch(kind,
    lms = c("chart", "measure", "sex"),
    st3 = c("chart", "measure", "sex"),
    equations = c("chart", "measure", "sex", "param", "transform", "y_scale")
  )
  missing <- setdiff(c(numeric, text), names(d))
  if (length(missing)) {
    stop("The ", kind, " parameters need the column", if (length(missing) > 1) "s", ": ",
      paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }
  if (kind == "equations") {
    if (!"model" %in% names(d)) d$model <- "normal"
    bad <- setdiff(unique(d$model), "normal")
    if (length(bad)) stop("Only normal models are supported in equations (not ", bad[1], ").", call. = FALSE)
    for (column in c("factor", "shift")) {
      if (column %in% names(d) && any(d[[column]] != if (column == "factor") 1 else 0, na.rm = TRUE)) {
        stop("Equations with `", column, "` are not supported.", call. = FALSE)
      }
    }
    text <- c(text, "model")
  }
  for (column in numeric) {
    if (!is.numeric(d[[column]]) || anyNA(d[[column]]) || any(!is.finite(d[[column]]))) {
      stop("`", column, "` must be finite numbers (", kind, " parameters).", call. = FALSE)
    }
  }
  for (column in c("x_units", "measure_units")) {
    if (!column %in% names(d)) d[[column]] <- NA_character_
  }
  for (column in c(text, "x_units", "measure_units")) d[[column]] <- as.character(d[[column]])
  d$sex[!is.na(d$sex) & !nzchar(d$sex)] <- NA_character_
  given <- !is.na(d$sex)
  d$sex[given] <- parse_sex(d$sex[given], strict = TRUE)
  d[c(text, numeric, "x_units", "measure_units")]
}

# The metadata, with the aliases age_min, age_max and age_units (for x_min, x_max and x_units)
normalise_metadata <- function(m, charts) {
  m <- as.data.frame(m, stringsAsFactors = FALSE)
  alias <- c(age_min = "x_min", age_max = "x_max", age_units = "x_units")
  for (from in names(alias)) {
    if (from %in% names(m) && !alias[[from]] %in% names(m)) names(m)[names(m) == from] <- alias[[from]]
  }
  if (!"chart" %in% names(m)) stop("`metadata` needs a `chart` column.", call. = FALSE)
  known <- c(
    "chart", "measure", "sex", "x_label", "x_min", "x_max", "z_method", "x_break", "x_units", "measure_units",
    "sources_url"
  )
  extra <- setdiff(names(m), c(known, "verified", "notes", "percentiles"))
  if (length(extra)) stop("Unknown `metadata` columns: ", paste(extra, collapse = ", "), ".", call. = FALSE)
  for (column in setdiff(known, names(m))) m[[column]] <- NA
  for (column in c("chart", "measure", "sex", "x_label", "z_method", "x_units", "measure_units", "sources_url")) {
    m[[column]] <- as.character(m[[column]])
    m[[column]][!is.na(m[[column]]) & !nzchar(m[[column]])] <- NA_character_
  }
  for (column in c("x_min", "x_max", "x_break")) {
    if (!is.numeric(m[[column]]) && !all(is.na(m[[column]]))) {
      stop("`metadata$", column, "` must be numeric.", call. = FALSE)
    }
    m[[column]] <- as.numeric(m[[column]])
  }
  unknown <- setdiff(m$chart, charts)
  if (length(unknown)) stop("`metadata` for charts not in `params`: ", paste(unknown, collapse = ", "), call. = FALSE)
  given <- !is.na(m$sex)
  m$sex[given] <- parse_sex(m$sex[given], strict = TRUE)
  bad <- !is.na(m$x_label) & !m$x_label %in% x_labels
  if (any(bad)) stop("`metadata$x_label` must be one of ", paste(x_labels, collapse = ", "), ".", call. = FALSE)
  m[known]
}

# A metadata field for one table: the most specific row with a value (chart, then measure, then sex)
meta_field <- function(m, chart, measure, sex, field) {
  if (is.null(m)) {
    return(NA)
  }
  applies <- m$chart == chart & (is.na(m$measure) | m$measure == measure) &
    (is.na(m$sex) | (!is.na(sex) & m$sex == sex))
  rows <- m[applies, , drop = FALSE]
  if (!nrow(rows)) {
    return(NA)
  }
  rank <- (!is.na(rows$measure)) * 2 + (!is.na(rows$sex))
  values <- rows[[field]][order(rank, decreasing = TRUE)]
  values <- values[!is.na(values)]
  if (length(values)) values[1] else NA
}

# The tables of the added charts, checked as the engine checks its own
build_tables <- function(parsed, metadata) {
  lms <- parsed$lms
  st3 <- parsed$st3
  eq <- parsed$equations
  # LMS: one table for both sexes when the rows are for both (sex NA), or identical for m and f
  if (!is.null(lms)) {
    for (k in unique(paste(lms$chart, lms$measure, sep = "|"))) {
      i <- paste(lms$chart, lms$measure, sep = "|") == k
      m <- lms[i & !is.na(lms$sex) & lms$sex == "m", c("x", "L", "M", "S")]
      f <- lms[i & !is.na(lms$sex) & lms$sex == "f", c("x", "L", "M", "S")]
      rownames(m) <- rownames(f) <- NULL
      if (any(i & is.na(lms$sex)) && any(i & !is.na(lms$sex))) {
        stop("LMS rows of ", sub("|", "/", k, fixed = TRUE), " mix rows for both sexes (sex NA) and for one sex.",
          call. = FALSE
        )
      }
      if (nrow(m) && identical(m[order(m$x), ], f[order(f$x), ])) {
        lms <- lms[!(i & !is.na(lms$sex) & lms$sex == "f"), ]
        lms$sex[paste(lms$chart, lms$measure, sep = "|") == k] <- NA_character_
      }
    }
  }
  keys <- unique(rbind(
    if (!is.null(lms)) data.frame(chart = lms$chart, measure = lms$measure, sex = lms$sex, family = "lms"),
    if (!is.null(st3)) data.frame(chart = st3$chart, measure = st3$measure, sex = st3$sex, family = "model"),
    if (!is.null(eq)) model_sexes(eq)
  ))
  both_kinds <- intersect(
    paste(keys$chart, keys$measure)[keys$family == "lms"],
    paste(keys$chart, keys$measure)[keys$family == "model"]
  )
  if (length(both_kinds)) {
    stop("A chart and measure has both LMS and ST3/equation parameters: ", both_kinds[1], ".", call. = FALSE)
  }
  # A model sexed by any of its parts: tables for m and f
  model <- keys[keys$family == "model", ]
  sexed <- unique(paste(model$chart, model$measure)[!is.na(model$sex)])
  unsexed <- model[!paste(model$chart, model$measure) %in% sexed, ]
  both_sexes <- do.call(rbind, lapply(sexed, function(k) {
    r <- model[paste(model$chart, model$measure) == k, ][1, ]
    data.frame(chart = r$chart, measure = r$measure, sex = c("m", "f"), family = "model")
  }))
  model <- unique(rbind(unsexed, both_sexes))
  keys <- rbind(keys[keys$family == "lms", ], model)

  tables <- NULL
  for (j in seq_len(nrow(keys))) {
    tables <- rbind(tables, check_table(keys[j, ], lms, st3, eq, metadata))
  }
  eq_out <- if (is.null(eq)) {
    NULL
  } else {
    data.frame(
      chart = eq$chart, measure = eq$measure, sex = eq$sex, x_from = eq$x_from, param = eq$param, coef = eq$coef,
      power = eq$power, log_power = eq$log_power, male = eq$male, x_scale = eq$x_scale, transform = eq$transform,
      y_scale = eq$y_scale, model = eq$model, stringsAsFactors = FALSE
    )
  }
  list(
    tables = tables,
    lms = if (is.null(lms)) NULL else lms[c("chart", "measure", "sex", "x", "L", "M", "S")],
    st3 = if (is.null(st3)) NULL else st3[c("chart", "measure", "sex", "x", "mu", "sigma", "nu", "tau")],
    equations = eq_out
  )
}

# The sexes of equation models: sex-specific when a row is for one sex or a term is for boys only
model_sexes <- function(eq) {
  out <- NULL
  for (k in unique(paste(eq$chart, eq$measure, sep = "|"))) {
    r <- eq[paste(eq$chart, eq$measure, sep = "|") == k, ]
    sexed <- any(!is.na(r$sex)) || any(r$male == 1)
    out <- rbind(out, data.frame(
      chart = r$chart[1], measure = r$measure[1], sex = if (sexed) c("m", "f") else NA_character_,
      family = "model"
    ))
  }
  out
}

# One table: its row of the tables, after the same checks as the built-in tables
check_table <- function(k, lms, st3, eq, metadata) {
  name <- paste0(k$chart, "/", k$measure, if (!is.na(k$sex)) paste0("/", k$sex))
  fail <- function(...) stop(name, ": ", ..., call. = FALSE)
  field <- function(f) meta_field(metadata, k$chart, k$measure, k$sex, f)
  same_sex <- function(s) if (is.na(k$sex)) is.na(s) else !is.na(s) & s == k$sex
  units_of <- function(d, column) {
    v <- unique(d[[column]][!is.na(d[[column]])])
    if (length(v) > 1) fail("more than one ", column, " (", paste(v, collapse = ", "), ").")
    if (length(v)) v else field(column)
  }

  if (k$family == "lms") {
    rows <- lms[lms$chart == k$chart & lms$measure == k$measure & same_sex(lms$sex), ]
    rows <- rows[order(rows$x), ]
    x_units <- units_of(rows, "x_units")
    measure_units <- units_of(rows, "measure_units")
    xs <- rows$x
    n <- length(xs)
    if (n < 2) fail("an LMS table needs at least 2 rows.")
    if (any(diff(xs) <= 0)) fail("x must be strictly increasing (each x once).")
    if (!all(valid_lms(rows$L, rows$M, rows$S))) fail("L must be finite, and M and S positive.")
    brk <- field("x_break")
    if (!is.na(brk)) {
      i <- findInterval(brk, xs)
      if (i < 1 || i >= n || !(brk > xs[i] + tol(xs[i])) || !(brk < xs[i + 1] - tol(xs[i + 1]))) {
        fail("the step x_break ", brk, " is not between two rows.")
      }
    }
    lo <- coverage_bound(field("x_min"), xs, FALSE, brk, x_units, fail)
    hi <- coverage_bound(field("x_max"), xs, TRUE, brk, x_units, fail)
    if (!(lo < hi)) fail("the coverage minimum is not below the maximum.")
    z_method <- field("z_method")
    if (is.na(z_method)) z_method <- "lms"
    if (!z_method %in% c("lms", "sd23")) fail("z_method must be \"lms\" or \"sd23\" for an LMS table.")
    meta <- list(x_min = lo, x_max = hi, x_break = brk, x_units = if (is.na(x_units)) "" else x_units)
    ends <- params_at(c(lo, hi), meta, rows, c("L", "M", "S"), "coverage")
    if (!all(valid_lms(ends$L, ends$M, ends$S))) fail("extrapolation to the coverage bounds gives invalid values.")
    if (z_method == "sd23" && !all(has_cutoffs(c(rows$L, ends$L), c(rows$M, ends$M), c(rows$S, ends$S)))) {
      fail("the 2 and 3 SD values do not all exist, as the \"sd23\" method needs.")
    }
    family <- "lms"
    n_rows <- n
  } else {
    of_table <- function(d) d[d$chart == k$chart & d$measure == k$measure & same_sex_or_both(d$sex, k$sex), ]
    s <- if (is.null(st3)) NULL else of_table(st3)
    e <- if (is.null(eq)) NULL else of_table(eq)
    both <- rbind(
      if (!is.null(s) && nrow(s)) s[c("x_units", "measure_units")],
      if (!is.null(e) && nrow(e)) e[c("x_units", "measure_units")]
    )
    x_units <- units_of(both, "x_units")
    measure_units <- units_of(both, "measure_units")
    if (!is.null(s) && nrow(s)) {
      s <- s[order(s$x), ]
      if (nrow(s) < 2) fail("an ST3 table needs at least 2 rows.")
      if (any(diff(s$x) <= 0)) fail("x must be strictly increasing (each x once).")
      if (!all(valid_st3(s$mu, s$sigma, s$nu, s$tau))) fail("mu must be finite, and sigma, nu and tau positive.")
    }
    if (!is.null(e) && nrow(e)) {
      if (!all(e$param %in% c("mu", "sigma"))) fail("the equations' param must be \"mu\" or \"sigma\".")
      if (!all(e$transform %in% c("identity", "exp", "sqrt"))) fail("transform must be identity, exp or sqrt.")
      if (!all(e$y_scale %in% c("identity", "log"))) fail("y_scale must be \"identity\" or \"log\".")
      if (any(e$log_power < 0 | e$log_power != floor(e$log_power))) fail("log_power must be a whole number >= 0.")
      if (any(e$x_scale <= 0)) fail("x_scale must be positive.")
      if (!all(e$male %in% c(0, 1))) fail("male must be 0 or 1.")
    }
    starts <- sort(c(if (!is.null(s) && nrow(s)) s$x[1], if (!is.null(e) && nrow(e)) unique(e$x_from)))
    lo <- field("x_min")
    hi <- field("x_max")
    if (is.na(lo) || is.na(hi)) {
      if (!is.null(e) && nrow(e)) fail("equations need the coverage: x_min and x_max in `metadata`.")
      lo <- if (is.na(lo)) s$x[1] else lo
      hi <- if (is.na(hi)) s$x[nrow(s)] else hi
    }
    if (!(lo < hi)) fail("the coverage minimum is not below the maximum.")
    # The parts must cover the coverage: the first starts at or before its minimum
    if (starts[1] > lo + tol(lo)) fail("the first part starts at ", starts[1], ", after the coverage minimum ", lo, ".")
    ends <- c(starts[-1], hi)
    if (any(starts >= ends)) fail("each part must start before the next one (and before the coverage maximum).")
    z_method <- field("z_method")
    if (is.na(z_method)) z_method <- "cdf"
    if (z_method != "cdf") fail("z_method must be \"cdf\" for ST3 and equation models.")
    meta <- data.frame(
      chart = k$chart, measure = k$measure, sex = k$sex, x_min = lo, x_max = hi,
      x_units = if (is.na(x_units)) "" else x_units, stringsAsFactors = FALSE
    )
    data <- list(st3 = if (is.null(s)) st3[0, ] else s, equations = if (is.null(e)) NULL else e)
    parts <- build_parts(data, meta)
    for (j in seq_along(parts)) {
      p <- parts[[j]]
      if (p$model == "st3") {
        if (p$rows$x[nrow(p$rows)] < ends[j] - tol(ends[j])) fail("the ST3 rows end before the end of their part.")
      } else {
        xs <- starts[j] + (ends[j] - starts[j]) * (0:200) / 200
        if (!nrow(p$mu) || !nrow(p$sigma)) fail("the equations part from ", starts[j], " needs both mu and sigma.")
        if (!all(valid_normal(equation_at(xs, p$mu), equation_at(xs, p$sigma)))) {
          fail("the equations give a sigma that is not positive (or no value) in the part from ", starts[j], ".")
        }
      }
    }
    kinds <- vapply(parts, function(p) p$model, character(1))
    family <- paste(unique(kinds), collapse = ", ")
    brk <- NA_real_
    n_rows <- if (!is.null(s)) nrow(s) else 0L
  }
  x_label <- field("x_label")
  data.frame(
    chart = k$chart, measure = k$measure, sex = k$sex, family = family,
    x_label = if (is.na(x_label)) "age" else x_label,
    x_units = if (is.na(x_units)) NA_character_ else x_units,
    measure_units = if (is.na(measure_units)) NA_character_ else measure_units,
    x_min = lo, x_max = hi, z_method = z_method, x_break = as.numeric(brk), n_rows = as.integer(n_rows),
    on_website = FALSE, calculators = NA_character_, sources_url = as.character(field("sources_url")),
    stringsAsFactors = FALSE
  )
}

same_sex_or_both <- function(s, sex) is.na(s) | (!is.na(sex) & s == sex)

# A coverage bound against the rows at one end, as for the built-in tables: at the step, at the end row (within
# the tolerance), or at most half the adjacent row spacing beyond it
coverage_bound <- function(bound, xs, upper, brk, x_units, fail) {
  n <- length(xs)
  end <- if (upper) xs[n] else xs[1]
  if (is.na(bound)) {
    return(end)
  }
  if (!is.na(brk) && abs(bound - brk) <= tol(brk)) {
    return(brk)
  }
  units <- if (is.na(x_units)) "" else x_units
  cover_tol <- max(bound_tol(end, units), coverage_tolerance * max(1, abs(end)))
  reach <- if (upper) bound - end else end - bound
  half <- (if (upper) xs[n] - xs[n - 1] else xs[2] - xs[1]) / 2
  if (reach < -cover_tol) {
    fail(
      "the coverage ", if (upper) "maximum" else "minimum", " ", bound, " is ",
      if (upper) "below the last row" else "above the first row", "."
    )
  }
  if (reach > half + cover_tol) {
    fail(
      "the coverage ", if (upper) "maximum" else "minimum", " ", bound, " is more than half a row step beyond ",
      "the ", if (upper) "last" else "first", " row (use extrapolate = \"linear\" to go further)."
    )
  }
  if (reach > cover_tol) bound else end
}
