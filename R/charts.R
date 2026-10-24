#' The available charts
#'
#' One row per chart, measure and sex: everything needed to use a chart's parameters. A chart with one table for
#' both sexes (e.g. `fenton_2003`, the INTERGROWTH-21st fetal standards) has one row with `sex` `NA`.
#'
#' @param chart Chart ids to keep; `NULL` (default) for all.
#' @return A data frame with columns:
#'   \describe{
#'     \item{chart, measure, sex}{The ids used by every function of the package; `sex` is `"m"`, `"f"`, or `NA`
#'       for a table used for both sexes.}
#'     \item{family}{How the distribution is given: `"lms"` (an LMS table, [growth_table()]), or the
#'       INTERGROWTH-21st parts in order of x: `"st3"` (a skew t table, [growth_table()]) and `"normal"`
#'       (equations for the mean and SD, [growth_equations()]).}
#'     \item{x_label}{What x is: `"age"`, `"length"`, `"height"`, `"gestational age"` or
#'       `"gestational age at birth"`.}
#'     \item{x_units, measure_units}{The units of x and of the measurement.}
#'     \item{x_min, x_max}{The coverage: the range of x the chart is published for. It may reach beyond the
#'       first or last row by up to half a row step, for tables whose rows are the midpoints of bands.}
#'     \item{z_method}{The Z-score method the chart's authors prescribe: `"lms"`, `"sd23"` (WHO's restricted LMS
#'       method beyond +-3 SD) or `"cdf"` (the inverse normal of the distribution function, INTERGROWTH-21st).}
#'     \item{x_break}{An x between two rows where the table steps (e.g. 730.5 days, where WHO's tables change
#'       from lying length to standing height), never interpolated across; `NA` for none.}
#'     \item{n_rows}{The number of rows of the table in [growth_table()] (0 for equations only).}
#'     \item{on_website}{Whether a PediTools calculator uses the chart.}
#'     \item{calculators}{The PediTools calculator ids that use it, separated by commas; `NA` for none.}
#'     \item{sources_url}{The chart's section of the PediTools Sources and methods page (for a chart with no
#'       calculator, the section on its method).}
#'     \item{added}{Whether the chart was added in the session ([growth_add_chart()]).}
#'   }
#' @seealso [growth_references()] for each chart's publications.
#' @export
#' @examples
#' charts <- growth_charts()
#' head(charts[c("chart", "measure", "sex", "x_units", "x_min", "x_max")])
#'
#' growth_charts("who_2006_infant")
#'
#' # Charts not used by a PediTools calculator
#' unique(charts$chart[!charts$on_website])
growth_charts <- function(chart = NULL) {
  tables <- pt_data()$tables
  if (!is.null(chart)) {
    check_charts(chart, tables)
    tables <- tables[tables$chart %in% chart, , drop = FALSE]
  }
  rownames(tables) <- NULL
  tables
}

#' A chart's parameter table
#'
#' The rows of the LMS tables (L, M, S) and of the INTERGROWTH-21st ST3 tables (mu, sigma, nu, tau), exactly as
#' published, for any number of charts, measures and sexes (crossed). x and the measurement are in the units given
#' by [growth_charts()]. Charts given by equations instead (the INTERGROWTH-21st `"normal"` parts) are in
#' [growth_equations()].
#'
#' @param chart Chart ids.
#' @param measure Measures; `NULL` (default) for every measure of the charts. Every chart must have every measure
#'   given.
#' @param sex `"m"`, `"f"`, `"male"` or `"female"`; `NULL` (default) for both. A table used for both sexes is
#'   always included (with `sex` `NA`).
#' @return A data frame with columns `chart`, `measure`, `sex` (`NA`: both sexes), `x`, and the parameters: `L`,
#'   `M`, `S` for LMS tables, `mu`, `sigma`, `nu`, `tau` for ST3 tables (only the columns used by the rows
#'   returned).
#' @seealso [growth_charts()] for the units and coverage.
#' @export
#' @examples
#' head(growth_table("cdc_2000_infant", "weight", "f"))
#'
#' # The ST3 rows of the INTERGROWTH-21st newborn size standards (from 231 days)
#' head(growth_table("intergrowth_nbs", "weight", "m"))
growth_table <- function(chart, measure = NULL, sex = NULL) {
  tables <- select_tables(chart, measure, sex)
  data <- pt_data()
  lms <- rows_of(data$lms, tables)
  st3 <- rows_of(data$st3, tables)
  ids <- c("chart", "measure", "sex", "x")
  columns <- c(ids, if (nrow(lms) || !nrow(st3)) c("L", "M", "S"), if (nrow(st3)) c("mu", "sigma", "nu", "tau"))
  fill <- function(d) {
    d[setdiff(columns, names(d))] <- rep(list(rep(NA_real_, nrow(d))), length(setdiff(columns, names(d))))
    d[columns]
  }
  out <- rbind(fill(lms), fill(st3))
  out <- out[order(match(
    table_key(out$chart, out$measure, out$sex),
    table_key(tables$chart, tables$measure, tables$sex)
  ), out$x), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' A chart's equations
#'
#' The terms of the INTERGROWTH-21st equations, for the parts of a chart given by equations (`family` `"normal"`
#' in [growth_charts()]), exactly as published (as in the `gigs` R package 0.5.2), for any number of charts,
#' measures and sexes (crossed). Charts and parts given by tables are in [growth_table()].
#'
#' A parameter (`param`, `mu` or `sigma`) of the part starting at `x_from` is `transform` applied to the sum of its
#' terms `coef * t^power * log(t)^log_power`, with `t = x / x_scale`; a term with `male` 1 applies to boys only.
#' The distribution is normal on the `y_scale` of the measurement (`"identity"` or `"log"`).
#'
#' @inheritParams growth_table
#' @return A data frame with columns `chart`, `measure`, `sex` (`NA`: both sexes), `x_from`, `param`, `coef`,
#'   `power`, `log_power`, `male`, `x_scale`, `transform`, `y_scale` and `model`; no rows for charts given by
#'   tables.
#' @export
#' @examples
#' growth_equations("intergrowth_png", "weight")
growth_equations <- function(chart, measure = NULL, sex = NULL) {
  tables <- select_tables(chart, measure, sex)
  eq <- pt_data()$equations
  same_table <- table_key(eq$chart, eq$measure) %in% table_key(tables$chart, tables$measure)
  sex_wanted <- if (is.null(sex)) TRUE else is.na(eq$sex) | eq$sex %in% parse_sex(sex, strict = TRUE)
  out <- eq[same_table & sex_wanted, , drop = FALSE]
  out <- out[order(match(table_key(out$chart, out$measure), table_key(tables$chart, tables$measure))), ,
    drop = FALSE
  ]
  rownames(out) <- NULL
  out
}
