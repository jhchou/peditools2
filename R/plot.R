#' Plot growth charts with ggplot2
#'
#' Draws percentile (or Z-score) curves of one or more charts, optionally with measurements from a data frame, and
#' returns a ggplot object (add `+ ggplot2::theme(...)`, `+ ggplot2::labs(...)` and so on). Needs the ggplot2
#' package.
#'
#' - **Several measures** are drawn one above the other, each with its own y axis; **both sexes** side by side.
#' - **Comparing charts or methods**: several `chart`s, or `sd23` or `cdc_bmi_ext` given as `c(FALSE, TRUE)`, are
#'   drawn in colours, one per chart and method. The charts compared must have the same x units.
#' - Curves beyond the published coverage (with `extrapolate = "linear"`) are dashed.
#' - **Measurements**: `data` with the x and y columns named by `x_col` and `y_col` (for several measures, `y_col`
#'   names a column per measure: `c(weight = "wt", length = "len")`), in the charts' units. `id_col` joins each
#'   person's points by a line; `sex_col` puts each point on its sex's chart when both sexes are drawn.
#'
#' @inheritParams growth_curves
#' @param data A data frame of measurements to plot, or `NULL`.
#' @param x_col,y_col The names of the columns of `data` with x and the measurement (see Details).
#' @param id_col The name of a column of `data` identifying each person (their points are joined), or `NULL`.
#' @param sex_col The name of a column of `data` with each point's sex (`"m"`, `"f"`, `"male"`, `"female"`), or
#'   `NULL`: needed when both sexes are drawn.
#' @param xlim The range of x to draw, or `NULL` for each chart's coverage.
#' @param labels The labels at the right end of the curves: as in [growth_curves()] (`NULL` for `"P3"`, ...; a
#'   label for each value of `p` or `z`; or a function of them), or `FALSE` for none.
#' @param sd23,cdc_bmi_ext As in [growth_z()]; `c(FALSE, TRUE)` draws both, to compare.
#' @return A ggplot object.
#' @seealso [growth_curves()] for the curves as data.
#' @export
#' @examplesIf requireNamespace("ggplot2", quietly = TRUE)
#' growth_plot("who_2006_infant", "weight", "f")
#'
#' # A child's measurements, on weight and length
#' d <- data.frame(
#'   age = c(1, 3, 6, 9, 12), wt = c(4.1, 5.6, 7.0, 7.9, 8.6),
#'   len = c(54, 60, 65.5, 70, 74)
#' )
#' growth_plot("who_2006_infant", c("weight", "length"), "f",
#'   data = d, x_col = "age",
#'   y_col = c(weight = "wt", length = "len")
#' )
#'
#' # CDC and WHO compared, Z-score lines
#' growth_plot(c("cdc_2000_infant", "who_2006_infant"), "weight", "m",
#'   z = c(-2, 0, 2), xlim = c(0, 24)
#' )
#'
#' # Labels of your own, or none
#' growth_plot("who_2006_infant", "length", "m",
#'   p = c(0.03, 0.5, 0.97), labels = function(p) paste0(100 * p, "th")
#' )
#' growth_plot("who_2006_infant", "length", "m", labels = FALSE)
#'
#' # WHO's restricted method beyond +-3 SD, on and off
#' growth_plot("who_2006_infant", "weight", "m", z = c(-4, -3, 3, 4), sd23 = c(FALSE, TRUE))
growth_plot <- function(chart, measure, sex = NULL, data = NULL, x_col = NULL, y_col = NULL, id_col = NULL,
                        sex_col = NULL, p = c(0.03, 0.1, 0.5, 0.9, 0.97), z = NULL, labels = NULL, xlim = NULL,
                        n = 300,
                        extrapolate = "coverage", sd23 = NULL, cdc_bmi_ext = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("growth_plot() needs the ggplot2 package: install.packages(\"ggplot2\"). ",
      "growth_curves() gives the curves as data for any other plotting library.",
      call. = FALSE
    )
  }
  tables <- select_tables(chart, measure, sex)
  units <- unique(tables$x_units)
  if (length(units) > 1) {
    stop("The charts have different x units (", paste(units, collapse = ", "), "): plot them separately.",
      call. = FALSE
    )
  }
  if (!is.null(xlim) && (!is.numeric(xlim) || length(xlim) != 2 || anyNA(xlim) || xlim[1] >= xlim[2])) {
    stop("`xlim` must be two increasing numbers.", call. = FALSE)
  }

  # The curves of each chart and method
  methods <- expand.grid(
    sd23 = if (is.null(sd23)) NA else sd23, ext = if (is.null(cdc_bmi_ext)) NA else cdc_bmi_ext,
    KEEP.OUT.ATTRS = FALSE
  )
  curves <- NULL
  for (i in seq_len(nrow(methods))) {
    s <- if (is.na(methods$sd23[i])) NULL else methods$sd23[i]
    e <- if (is.na(methods$ext[i])) NULL else methods$ext[i]
    x <- if (is.null(xlim)) NULL else seq(xlim[1], xlim[2], length.out = n + 1)
    k <- suppressWarnings(growth_curves(unique(tables$chart), unique(tables$measure),
      sex = sex, p = p, z = z, labels = if (isFALSE(labels)) NULL else labels, x = x, n = n,
      extrapolate = extrapolate, sd23 = s, cdc_bmi_ext = e
    ))
    k$method <- paste(c(
      if (!is.null(s) && length(sd23) > 1) paste0("SD23 ", if (s) "on" else "off"),
      if (!is.null(e) && length(cdc_bmi_ext) > 1) paste0("extended BMI ", if (e) "on" else "off")
    ), collapse = ", ")
    curves <- rbind(curves, k)
  }
  curves <- curves[!is.na(curves$y), ]
  if (!nrow(curves)) stop("No curve has values in this range.", call. = FALSE)
  several_charts <- length(unique(curves$chart)) > 1
  several_methods <- any(nzchar(curves$method))
  curves$series <- trimws(paste(
    if (several_charts) curves$chart else "",
    ifelse(nzchar(curves$method), if (several_charts) paste0("(", curves$method, ")") else curves$method, "")
  ))
  curves$group <- paste(curves$series, curves$sex, curves$measure, curves$curve, curves$segment)
  curves$line <- ifelse(curves$beyond_coverage, "beyond the coverage", "published coverage")

  meta <- unique(tables[c("measure", "measure_units")])
  measure_label <- function(m) paste0(m, " (", meta$measure_units[match(m, meta$measure)], ")")
  sex_label <- function(s) ifelse(is.na(s), "Both sexes", ifelse(s == "m", "Boys", "Girls"))
  # The measures in the order given
  measure_levels <- measure_label(unique(tables$measure[order(match(tables$measure, measure))]))
  curves$facet_measure <- factor(measure_label(curves$measure), levels = measure_levels)
  curves$facet_sex <- sex_label(curves$sex)

  colour <- several_charts || several_methods
  plot <- ggplot2::ggplot(curves, ggplot2::aes(x = x, y = y, group = group)) +
    ggplot2::theme_bw()
  plot <- if (colour) {
    plot + ggplot2::geom_line(ggplot2::aes(colour = series, linetype = line)) + ggplot2::labs(colour = NULL)
  } else {
    plot + ggplot2::geom_line(ggplot2::aes(linetype = line), colour = "grey35")
  }
  plot <- plot + ggplot2::scale_linetype_manual(
    values = c("published coverage" = "solid", "beyond the coverage" = "dashed"),
    guide = if (any(curves$beyond_coverage)) "legend" else "none", name = NULL
  )

  # The curves' labels at their right end
  if (!isFALSE(labels)) {
    ends <- curves[order(curves$x, decreasing = TRUE), ]
    # One label per curve and chart (methods of one chart end close together: one label for them)
    ends <- ends[!duplicated(paste(if (several_charts) ends$chart, ends$sex, ends$measure, ends$curve)), ]
    plot <- plot + ggplot2::geom_text(
      data = ends, ggplot2::aes(label = curve), hjust = -0.15, size = 3, colour = "grey30",
      show.legend = FALSE
    ) + ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.08)))
  }

  # The measurements
  if (!is.null(data)) {
    points <- plot_points(data, x_col, y_col, id_col, sex_col, unique(tables$measure), sex)
    points$facet_measure <- factor(measure_label(points$measure), levels = measure_levels)
    points$facet_sex <- sex_label(points$sex)
    if (!is.null(id_col)) {
      plot <- plot + ggplot2::geom_line(data = points, ggplot2::aes(x = x, y = y, group = id), colour = "black")
    }
    plot <- plot + ggplot2::geom_point(data = points, ggplot2::aes(x = x, y = y, group = NULL), colour = "black")
  }

  rows <- length(unique(curves$facet_measure)) > 1
  cols <- length(unique(curves$facet_sex)) > 1
  if (rows || cols) {
    plot <- plot + ggplot2::facet_grid(
      rows = if (rows) ggplot2::vars(facet_measure), cols = if (cols) ggplot2::vars(facet_sex),
      scales = "free_y"
    )
  }
  x_label <- unique(tables$x_label)
  plot + ggplot2::labs(
    x = paste0(paste(x_label, collapse = " / "), " (", units, ")"),
    y = if (rows) NULL else measure_label(unique(curves$measure)),
    title = if (!cols && !is.null(sex) && !all(is.na(curves$sex))) unique(curves$facet_sex)
  )
}

# The measurements of `data` in long form: x, y, measure, sex, id
plot_points <- function(data, x_col, y_col, id_col, sex_col, measures, sex) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.", call. = FALSE)
  if (is.null(x_col) || is.null(y_col)) stop("Give `x_col` and `y_col` with `data`.", call. = FALSE)
  if (length(measures) > 1 && (is.null(names(y_col)) || !all(measures %in% names(y_col)))) {
    stop("With several measures, `y_col` names a column for each: c(", paste0(measures, " = \"...\"", collapse = ", "),
      ").",
      call. = FALSE
    )
  }
  if (is.null(names(y_col))) names(y_col) <- measures
  columns <- c(x_col, unname(y_col[names(y_col) %in% measures]), id_col, sex_col)
  missing <- setdiff(columns, names(data))
  if (length(missing)) stop("Columns not in `data`: ", paste(missing, collapse = ", "), ".", call. = FALSE)
  both <- is.null(sex) || length(unique(parse_sex(sex, strict = TRUE))) > 1
  if (both && is.null(sex_col)) {
    stop("Both sexes are drawn: give `sex_col`, the column of each point's sex (or choose `sex`).", call. = FALSE)
  }
  out <- NULL
  for (m in measures[measures %in% names(y_col)]) {
    out <- rbind(out, data.frame(
      x = as.numeric(data[[x_col]]), y = as.numeric(data[[y_col[[m]]]]), measure = m,
      sex = if (is.null(sex_col)) parse_sex(sex, strict = TRUE)[1] else parse_sex(data[[sex_col]]),
      id = if (is.null(id_col)) NA_character_ else as.character(data[[id_col]]), stringsAsFactors = FALSE
    ))
  }
  out[!is.na(out$x) & !is.na(out$y), ]
}

#' @importFrom utils globalVariables
NULL

utils::globalVariables(c("x", "y", "group", "series", "line", "curve", "id", "facet_measure", "facet_sex"))
