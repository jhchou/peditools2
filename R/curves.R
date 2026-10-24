#' Percentile and Z-score curves
#'
#' The measurement at each percentile (or Z-score) along each chart's x, for plotting with any library: crossed over
#' every chart, measure, sex, curve and x. The values are those of [growth_y()], by the same methods. As on the
#' website's charts, a curve breaks where the table steps (lying length to standing height), where the
#' INTERGROWTH-21st parts change, and where a value is undefined (the `segment` column).
#'
#' @inheritParams growth_table
#' @inheritParams growth_z
#' @param p Probabilities of the curves (the percentile / 100). Default: the 3rd, 10th, 50th, 90th and 97th
#'   percentiles.
#' @param z Z-scores of the curves, instead of `p`.
#' @param labels The curves' labels (the `curve` column): `NULL` (default) for `"P3"`, `"P50"`, ... (or `"Z-2"`,
#'   `"Z0"`, `"Z+2"`, ...); a character vector with a label for each value of `p` (or `z`), in their order; or a
#'   function of `p` (or `z`) returning them, e.g. `function(p) paste0(100 * p, "th")`. Labels must be unique.
#' @param x The x of the points, the same for every table; `NULL` (default): `n` + 1 points evenly over each table's
#'   coverage. Points outside the range allowed by `extrapolate` are `NA`.
#' @param n The number of intervals of the default grid.
#' @return A data frame, one row per point: `chart`, `measure`, `sex` (`NA` for a table for both sexes), `curve` (a
#'   label: `"P3"`, `"P50"`, ... or `"Z-2"`, `"Z0"`, ...), `p`, `z`, `x`, `y` (`NA` where undefined or outside the
#'   allowed range), `segment` (a curve's pieces: a line connects only points of the same segment) and
#'   `beyond_coverage` (with `extrapolate = "linear"`).
#' @seealso [growth_plot()] to draw them with ggplot2.
#' @export
#' @examples
#' curves <- growth_curves("who_2006_infant", "weight", "f")
#' head(curves)
#'
#' # Z-score lines at given ages, with labels of your own
#' growth_curves("cdc_2000_infant", "length", "m",
#'   z = -2:2, labels = c("-2 SD", "-1 SD", "median", "+1 SD", "+2 SD"), x = c(0, 12, 24)
#' )
#'
#' # With base graphics
#' curves <- growth_curves("cdc_2000_bmi", "bmi", "m", p = c(0.05, 0.5, 0.85, 0.95))
#' plot(y ~ x, curves, type = "n", xlab = "age (months)", ylab = "BMI (kg/m2)")
#' for (k in split(curves, curves$curve)) lines(k$x, k$y)
growth_curves <- function(chart, measure, sex = NULL, p = c(0.03, 0.1, 0.5, 0.9, 0.97), z = NULL, labels = NULL,
                          x = NULL, n = 300, extrapolate = "coverage", sd23 = NULL, cdc_bmi_ext = NULL) {
  tables <- select_tables(chart, measure, sex)
  use_z <- !is.null(z)
  levels <- if (use_z) z else p
  if (!is.numeric(levels) || !length(levels) || anyNA(levels)) {
    stop("`", if (use_z) "z" else "p", "` must be numbers.", call. = FALSE)
  }
  if (!use_z && any(levels <= 0 | levels >= 1)) stop("`p` must be strictly between 0 and 1.", call. = FALSE)
  if (!is.null(x) && (!is.numeric(x) || !length(x))) stop("`x` must be numeric.", call. = FALSE)
  if (!is.numeric(n) || length(n) != 1 || n < 1) stop("`n` must be a positive number.", call. = FALSE)

  # The x of each table: the grid, plus both sides of a step or a part boundary within it
  grids <- lapply(seq_len(nrow(tables)), function(i) {
    t <- tables[i, ]
    xs <- if (is.null(x)) seq(t$x_min, t$x_max, length.out = n + 1) else sort(unique(x))
    edges <- table_edges(t)
    edges <- edges[edges > min(xs) & edges <= max(xs)]
    sort(unique(c(xs, edges, edges - 1e-6 * pmax(1, abs(edges)))))
  })
  sizes <- lengths(grids)
  k <- length(levels)
  idx <- rep(rep(seq_len(nrow(tables)), sizes), each = 1)
  pts <- data.frame(table = rep(idx, times = k), x = rep(unlist(grids), times = k), level = rep(levels,
    each = length(idx)
  ))
  t <- tables[pts$table, ]
  out <- if (use_z) {
    evaluate(pts$x, t$sex, t$chart, t$measure,
      z = pts$level, extrapolate = extrapolate, sd23 = sd23,
      cdc_bmi_ext = cdc_bmi_ext
    )
  } else {
    evaluate(pts$x, t$sex, t$chart, t$measure,
      p = pts$level, extrapolate = extrapolate, sd23 = sd23,
      cdc_bmi_ext = cdc_bmi_ext
    )
  }
  label <- curve_labels(levels, use_z, labels)
  curve <- label[match(pts$level, levels)]
  res <- data.frame(
    chart = t$chart, measure = t$measure, sex = t$sex, curve = curve,
    p = if (use_z) stats::pnorm(pts$level) else pts$level, z = if (use_z) pts$level else stats::qnorm(pts$level),
    x = pts$x, y = ifelse(out$status == "ok", out$y, NA_real_), beyond_coverage = out$beyond_coverage,
    stringsAsFactors = FALSE
  )
  res$segment <- curve_segments(res, tables[pts$table, ])
  res <- res[order(pts$table, match(curve, label), res$x), ]
  rownames(res) <- NULL
  res[c("chart", "measure", "sex", "curve", "p", "z", "x", "y", "segment", "beyond_coverage")]
}

# The label of each curve: the default (P3, Z-2, ...), given, or from a function of the levels
curve_labels <- function(levels, use_z, labels) {
  out <- if (is.null(labels)) {
    if (use_z) {
      paste0("Z", ifelse(levels > 0, "+", ""), format(levels, drop0trailing = TRUE, trim = TRUE))
    } else {
      paste0("P", format(100 * levels, drop0trailing = TRUE, trim = TRUE))
    }
  } else if (is.function(labels)) {
    labels(levels)
  } else {
    labels
  }
  what <- if (use_z) "z" else "p"
  if (!is.character(out) && !is.numeric(out) || length(out) != length(levels) || anyNA(out)) {
    stop("`labels` must give one label for each value of `", what, "` (", length(levels), "), with no NA.",
      call. = FALSE
    )
  }
  out <- as.character(out)
  if (anyDuplicated(out[!duplicated(levels)])) stop("`labels` must be unique.", call. = FALSE)
  out
}

# Where a table's curves break: its step (x_break), or the starts of its INTERGROWTH-21st parts after the first
table_edges <- function(t) {
  if (t$family == "lms") {
    return(if (is.na(t$x_break)) numeric(0) else t$x_break)
  }
  starts <- vapply(model_parts(table_key(t$chart, t$measure, t$sex), t), function(p) p$from, numeric(1))
  starts[starts > t$x_min]
}

# The segment number of each point: a new segment after an edge, after a gap (NA) and where the coverage ends
curve_segments <- function(res, tables) {
  key <- paste(res$chart, res$measure, res$sex, res$curve)
  out <- integer(nrow(res))
  for (k in unique(key)) {
    i <- which(key == k)
    i <- i[order(res$x[i])]
    edges <- table_edges(tables[i[1], ])
    side <- findInterval(res$x[i], edges - 1e-7 * pmax(1, abs(edges)))
    gap <- is.na(res$y[i])
    breaks <- c(TRUE, diff(side) != 0 | gap[-1] != gap[-length(i)] | diff(res$beyond_coverage[i]) != 0)
    out[i] <- cumsum(breaks)
  }
  out
}
