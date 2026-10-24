test_that("growth_curves() gives growth_y() at each point, crossed", {
  k <- growth_curves(c("cdc_2000_infant", "who_2006_infant"), c("weight", "length"), "f",
    p = c(0.1, 0.5), x = c(0, 6, 12)
  )
  expect_identical(nrow(k), 2L * 2L * 2L * 3L)
  expect_setequal(unique(k$curve), c("P10", "P50"))
  for (i in seq_len(nrow(k))) {
    expect_equal(k$y[i], growth_y(p = k$p[i], x = k$x[i], sex = "f", chart = k$chart[i], measure = k$measure[i]))
  }
  z <- growth_curves("cdc_2000_bmi", "bmi", "m", z = c(-2, 0, 2.5), x = 150)
  expect_identical(z$curve, c("Z-2", "Z0", "Z+2.5"))
  expect_equal(z$y, growth_y(c(-2, 0, 2.5), 150, "m", "cdc_2000_bmi", "bmi")) # the extended BMI above P95
})

test_that("the default grid covers each table's coverage, and both sexes", {
  k <- growth_curves("fenton_2003", "weight", n = 10)
  expect_true(all(is.na(k$sex)))
  expect_equal(range(k$x), c(22, 50 + 6 / 7))
  expect_identical(nrow(k), 5L * 11L)
  both <- growth_curves("who_2006_infant", "weight", p = 0.5, n = 4)
  expect_setequal(both$sex, c("m", "f"))
})

test_that("curves break at the step and at the INTERGROWTH-21st parts", {
  k <- growth_curves("who_expanded", "length_height", "m", p = 0.5, n = 20)
  before <- k[k$x < 730.5, ]
  after <- k[k$x >= 730.5, ]
  expect_identical(unique(before$segment), 1L)
  expect_identical(unique(after$segment), 2L)
  expect_true(any(abs(before$x - 730.5) < 0.01)) # the last point before the step
  w <- growth_table("who_expanded", "length_height", "m")
  expect_identical(after$y[after$x == 730.5], growth_y(0, 730.5, "m", "who_expanded", "length_height"))
  expect_identical(w$M[w$x == 731], after$y[after$x == 730.5])
  n <- growth_curves("intergrowth_nbs", "weight", "f", p = 0.5, n = 12)
  expect_identical(unique(n$segment[n$x < 231]), 1L)
  expect_identical(unique(n$segment[n$x >= 231]), 2L)
})

test_that("outside and beyond the coverage", {
  k <- growth_curves("fenton_2003", "weight", p = 0.5, x = c(20, 30, 52))
  expect_identical(is.na(k$y), c(TRUE, FALSE, TRUE))
  lin <- growth_curves("fenton_2003", "weight", p = 0.5, x = c(20, 30, 52), extrapolate = "linear")
  expect_identical(lin$beyond_coverage, c(TRUE, FALSE, TRUE))
  expect_identical(lin$segment, 1:3)
  expect_error(growth_curves("fenton_2003", "weight", p = 1.2), "strictly between 0 and 1")
})

test_that("growth_plot() builds ggplot objects", {
  skip_if_not_installed("ggplot2")
  build <- function(p) {
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))
  }
  build(growth_plot("who_2006_infant", "weight", "f"))
  d <- data.frame(
    age = c(1, 3, 6, 1, 4), wt = c(4.1, 5.6, 7, 4.5, 6.6), len = c(54, 60, 65.5, 55, 63),
    sex = c("f", "f", "f", "m", "m"), id = c("a", "a", "a", "b", "b")
  )
  p <- growth_plot("who_2006_infant", c("weight", "length"),
    data = d, x_col = "age",
    y_col = c(weight = "wt", length = "len"), id_col = "id", sex_col = "sex"
  )
  build(p)
  expect_identical(levels(p$data$facet_measure), c("weight (kg)", "length (cm)"))
  build(growth_plot(c("cdc_2000_infant", "who_2006_infant"), "weight", "m", z = c(-2, 0, 2), xlim = c(0, 24)))
  build(growth_plot("who_2006_infant", "weight", "m", z = c(-4, 4), sd23 = c(FALSE, TRUE)))
  build(growth_plot("fenton_2003", "weight", xlim = c(18, 56), extrapolate = "linear"))
  build(growth_plot("intergrowth_nbs", "weight", "m"))
  expect_error(growth_plot("who_2006_infant", "weight", data = d, x_col = "age", y_col = "wt"), "sex_col")
  expect_error(
    growth_plot("who_2006_infant", c("weight", "length"), "f", data = d, x_col = "age", y_col = "wt"),
    "names a column for each"
  )
  expect_error(growth_plot("who_2006_infant", "weight", "f", data = d, x_col = "age", y_col = "w"), "not in `data`")
  expect_error(growth_plot(c("who_2006_infant", "who_expanded"), "weight", "f"), "different x units")
})

test_that("curve labels: default, given, from a function, or none in the plot", {
  x <- c(0, 12)
  expect_setequal(growth_curves("who_2006_infant", "weight", "f", x = x)$curve, c("P3", "P10", "P50", "P90", "P97"))
  k <- growth_curves("who_2006_infant", "weight", "f", z = c(-2, 0, 2), labels = c("-2 SD", "median", "+2 SD"), x = x)
  expect_identical(unique(k$curve), c("-2 SD", "median", "+2 SD"))
  expect_identical(k$curve[k$z == 0], c("median", "median"))
  th <- function(p) paste0(100 * p, "th")
  f <- growth_curves("who_2006_infant", "weight", "f", p = c(0.1, 0.9), labels = th, x = x)
  expect_identical(unique(f$curve), c("10th", "90th"))
  expect_error(growth_curves("who_2006_infant", "weight", "f", p = c(0.1, 0.9), labels = "a"), "one label for each")
  expect_error(growth_curves("who_2006_infant", "weight", "f", p = c(0.1, 0.9), labels = c("a", "a")), "unique")
  expect_error(growth_curves("who_2006_infant", "weight", "f", labels = FALSE), "one label for each")
  skip_if_not_installed("ggplot2")
  p <- growth_plot("who_2006_infant", "weight", "f", z = c(-2, 2), labels = c("low", "high"))
  texts <- p$layers[vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1))]
  expect_setequal(texts[[1]]$data$curve, c("low", "high"))
  none <- growth_plot("who_2006_infant", "weight", "f", labels = FALSE)
  expect_false(any(vapply(none$layers, function(l) inherits(l$geom, "GeomText"), logical(1))))
  expect_no_error(ggplot2::ggplot_build(none))
})
