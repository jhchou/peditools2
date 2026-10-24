# The cross-check against the PediTools engine: tests/testthat/fixtures/engine.csv.gz, written by the engine's export
# in the PediTools development repository for every table of the package's charts: a grid of x
# over each table (the coverage bounds, the end rows, between a bound and its end row, around the step, rows and
# midpoints spread over the table, random x), the parameters at x and the engine's measurement at each Z of the grid.

read_fixture <- function() {
  utils::read.csv(gzfile(testthat::test_path("fixtures", "engine.csv.gz")), stringsAsFactors = FALSE, na.strings = "")
}
fixture_z <- c(m5 = -5, m3.5 = -3.5, m3 = -3, m2 = -2, `0` = 0, `2` = 2, `3` = 3, `3.5` = 3.5, `5` = 5)
sd23_z <- fixture_z[c("m5", "m3.5", "3.5", "5")]

test_that("the cross-check file covers every table", {
  f <- read_fixture()
  charts <- growth_charts()
  # Tables for both sexes appear once per sex in the engine
  expect_setequal(unique(paste(f$chart, f$measure)), unique(paste(charts$chart, charts$measure)))
  expect_gt(nrow(f), 4000)
})

lms_rows <- function() {
  f <- read_fixture()
  f[f$part == "lms", ]
}

test_that("LMS parameters at x equal the engine's", {
  f <- lms_rows()
  at <- growth_params(f$x, f$sex, f$chart, f$measure)
  expect_identical(at$status, rep("ok", nrow(f)))
  expect_equal(at$L, f$L, tolerance = 1e-12)
  expect_equal(at$M, f$M, tolerance = 1e-12)
  expect_equal(at$S, f$S, tolerance = 1e-12)
})

test_that("measurements at Z equal the engine's (each chart's published method)", {
  f <- lms_rows()
  for (k in names(fixture_z)) {
    expected <- f[[paste0("y_", k)]]
    y <- suppressWarnings(growth_y(fixture_z[[k]], f$x, f$sex, f$chart, f$measure, cdc_bmi_ext = FALSE))
    expect_identical(is.na(y), is.na(expected), label = paste("NA at Z", fixture_z[[k]]))
    expect_equal(y, expected, tolerance = 1e-11, label = paste("y at Z", fixture_z[[k]]))
  }
})

test_that("Z-scores of the engine's measurements give the Z back", {
  f <- lms_rows()
  for (k in names(fixture_z)) {
    y <- f[[paste0("y_", k)]]
    has <- !is.na(y)
    z <- growth_z(y[has], f$x[has], f$sex[has], f$chart[has], f$measure[has], cdc_bmi_ext = FALSE)
    expect_equal(z, rep(fixture_z[[k]], sum(has)), tolerance = 1e-9, label = paste("Z", fixture_z[[k]]))
  }
})

test_that("SD23 forced on every LMS table equals the engine's restricted LMS method", {
  f <- lms_rows()
  for (k in names(sd23_z)) {
    expected <- f[[paste0("ysd23_", k)]]
    y <- suppressWarnings(growth_y(sd23_z[[k]], f$x, f$sex, f$chart, f$measure, sd23 = TRUE, cdc_bmi_ext = FALSE))
    expect_identical(is.na(y), is.na(expected), label = paste("NA at Z", sd23_z[[k]]))
    expect_equal(y, expected, tolerance = 1e-11, label = paste("SD23 y at Z", sd23_z[[k]]))
    has <- !is.na(expected)
    z <- growth_z(expected[has], f$x[has], f$sex[has], f$chart[has], f$measure[has],
      sd23 = TRUE,
      cdc_bmi_ext = FALSE
    )
    expect_equal(z, rep(sd23_z[[k]], sum(has)), tolerance = 1e-9)
  }
})

test_that("the CDC extended BMI: P95 and sigma equal the engine's; the method by its formula", {
  f <- lms_rows()
  f <- f[f$chart == "cdc_2000_bmi", ]
  expect_gt(nrow(f), 20)
  d <- growth_detail(z = 0, x = f$x, sex = f$sex, chart = f$chart, measure = f$measure)
  expect_equal(d$p95, f$p95, tolerance = 1e-12)
  expect_equal(d$ext_sigma, f$ext_sigma, tolerance = 1e-12)
  # From the 95th percentile: BMI = P95 + sigma * qnorm((p - 0.9) / 0.1), and back
  for (z in c(stats::qnorm(0.95), 2, 2.5, 3, 4)) {
    expected <- f$p95 + f$ext_sigma * stats::qnorm(10 * stats::pnorm(z, lower.tail = FALSE), lower.tail = FALSE)
    y <- growth_y(z, f$x, f$sex, f$chart, f$measure)
    expect_equal(y, expected, tolerance = 1e-12)
    expect_equal(growth_z(y, f$x, f$sex, f$chart, f$measure), rep(z, nrow(f)), tolerance = 1e-9)
    p <- 0.9 + 0.1 * stats::pnorm((y - f$p95) / f$ext_sigma)
    expect_equal(growth_p(y, f$x, f$sex, f$chart, f$measure), p, tolerance = 1e-14)
  }
  # Below the 95th percentile: plain LMS, as the engine
  expect_equal(growth_y(1, f$x, f$sex, f$chart, f$measure), growth_y(1, f$x, f$sex, f$chart, f$measure,
    cdc_bmi_ext = FALSE
  ))
})

# Behaviour the website does not have, checked against its definition on every LMS table

test_that("extrapolate = \"linear\": beyond the coverage, the line through the two end rows", {
  charts <- growth_charts()
  charts <- charts[charts$family == "lms", ]
  for (i in seq_len(nrow(charts))) {
    ch <- charts[i, ]
    sex <- if (is.na(ch$sex)) "m" else ch$sex
    t <- growth_table(ch$chart, ch$measure, sex)
    # A coverage that starts at the step: the rows on its side of the step
    if (!is.na(ch$x_break) && isTRUE(all.equal(ch$x_min, ch$x_break))) t <- t[t$x > ch$x_break, ]
    span <- ch$x_max - ch$x_min
    below <- ch$x_min - span / 10
    above <- ch$x_max + span / 10
    at <- growth_params(c(below, above), sex, ch$chart, ch$measure, extrapolate = "linear")
    n <- nrow(t)
    line <- function(a, b, x, v) t[[v]][a] + (t[[v]][b] - t[[v]][a]) * (x - t$x[a]) / (t$x[b] - t$x[a])
    for (v in c("L", "M", "S")) {
      expected <- c(line(1, 2, below, v), line(n - 1, n, above, v))
      ok <- at$status == "ok"
      expect_equal(at[[v]][ok], expected[ok], tolerance = 1e-12, label = paste(ch$chart, ch$measure, sex, v))
    }
    expect_true(all(at$beyond_coverage[at$status == "ok"]))
    expect_true(all(at$status %in% c("ok", "undefined")))
    # The default and "none" refuse them
    expect_identical(
      suppressWarnings(growth_params(c(below, above), sex, ch$chart, ch$measure))$status,
      c("outside", "outside")
    )
  }
})

test_that("extrapolate = \"none\": only between the first and last rows", {
  charts <- growth_charts()
  charts <- charts[charts$family == "lms", ]
  checked <- 0
  for (i in seq_len(nrow(charts))) {
    ch <- charts[i, ]
    sex <- if (is.na(ch$sex)) "m" else ch$sex
    t <- growth_table(ch$chart, ch$measure, sex)
    ends <- c(ch$x_min, ch$x_max)
    beyond_rows <- c(ch$x_min < min(t$x) - 1e-6, ch$x_max > max(t$x) + 1e-6)
    if (!any(beyond_rows)) next
    checked <- checked + 1
    x <- ends[beyond_rows]
    expect_identical(growth_params(x, sex, ch$chart, ch$measure)$status, rep("ok", length(x)))
    expect_identical(growth_params(x, sex, ch$chart, ch$measure, extrapolate = "none")$status,
      rep("outside", length(x)),
      label = paste(ch$chart, ch$measure)
    )
  }
  expect_gt(checked, 0)
})

# INTERGROWTH-21st: the ST3 day tables and the equations

model_rows <- function() {
  f <- read_fixture()
  f[f$part != "lms", ]
}

test_that("INTERGROWTH-21st parameters at x equal the engine's, part by part", {
  f <- model_rows()
  expect_gt(nrow(f), 500)
  at <- growth_params(f$x, f$sex, f$chart, f$measure)
  expect_identical(at$status, rep("ok", nrow(f)))
  expect_identical(at$part, f$part)
  expect_equal(at$mu, f$mu, tolerance = 1e-13)
  expect_equal(at$sigma, f$sigma, tolerance = 1e-13)
  st3 <- f$part == "st3"
  expect_equal(at$nu[st3], f$nu[st3], tolerance = 1e-13)
  expect_equal(at$tau[st3], f$tau[st3], tolerance = 1e-13)
})

test_that("INTERGROWTH-21st measurements at Z equal the engine's, and give the Z back", {
  f <- model_rows()
  for (k in names(fixture_z)) {
    expected <- f[[paste0("y_", k)]]
    y <- growth_y(fixture_z[[k]], f$x, f$sex, f$chart, f$measure)
    expect_equal(y, expected, tolerance = 1e-10, label = paste("y at Z", fixture_z[[k]]))
    z <- growth_z(expected, f$x, f$sex, f$chart, f$measure)
    expect_equal(z, rep(fixture_z[[k]], nrow(f)), tolerance = 1e-9, label = paste("Z", fixture_z[[k]]))
  }
})

test_that("SD23 forced on INTERGROWTH-21st charts: the line beyond +-3 from their own 2 and 3 SD values", {
  f <- model_rows()
  y3 <- f$y_3
  y2 <- f$y_2
  ym3 <- f$y_m3
  ym2 <- f$y_m2
  expect_equal(growth_y(4, f$x, f$sex, f$chart, f$measure, sd23 = TRUE), y3 + (y3 - y2), tolerance = 1e-10)
  low <- ym3 - 0.5 * (ym2 - ym3)
  ok <- low > 0
  expect_equal(growth_y(-3.5, f$x, f$sex, f$chart, f$measure, sd23 = TRUE)[ok], low[ok], tolerance = 1e-10)
  expect_equal(growth_z(y3 + 0.25 * (y3 - y2), f$x, f$sex, f$chart, f$measure, sd23 = TRUE), rep(3.25, nrow(f)),
    tolerance = 1e-9
  )
  # Within +-3 SD: unchanged
  expect_equal(growth_y(1.5, f$x, f$sex, f$chart, f$measure, sd23 = TRUE), growth_y(1.5, f$x, f$sex, f$chart,
    f$measure,
    sd23 = FALSE
  ))
})

test_that("INTERGROWTH-21st with extrapolate = \"linear\": the equations beyond the coverage, the ST3 rows' line", {
  # Postnatal growth: the equations themselves at 66 weeks (coverage to 64)
  e <- growth_equations("intergrowth_png", "weight", "m")
  eq <- function(terms, x) {
    t <- x / terms$x_scale[1]
    v <- 0
    for (k in seq_len(nrow(terms))) {
      v <- v + terms$coef[k] * t^terms$power[k] * (if (terms$log_power[k] > 0) log(t)^terms$log_power[k] else 1)
    }
    switch(terms$transform[1],
      exp = exp(v),
      sqrt = sqrt(v),
      v
    )
  }
  at <- growth_params(66, "m", "intergrowth_png", "weight", extrapolate = "linear")
  expect_identical(c(at$status, at$beyond_coverage), c("ok", "TRUE"))
  expect_equal(at$mu, eq(e[e$param == "mu" & (is.na(e$sex) | e$sex == "m"), ], 66), tolerance = 1e-12)
  expect_equal(at$sigma, eq(e[e$param == "sigma", ], 66), tolerance = 1e-12)
  expect_warning(growth_z(6, 66, "m", "intergrowth_png", "weight"), "outside")
  # Newborn size beyond 300 days: the line through the last two ST3 rows
  t <- growth_table("intergrowth_nbs", "weight", "f")
  n <- nrow(t)
  at <- growth_params(305, "f", "intergrowth_nbs", "weight", extrapolate = "linear")
  expect_equal(at$mu, t$mu[n] + (t$mu[n] - t$mu[n - 1]) * (305 - t$x[n]) / (t$x[n] - t$x[n - 1]), tolerance = 1e-12)
  expect_true(at$extrapolated)
  # "none" is the coverage
  expect_identical(
    growth_params(c(168, 300), "f", "intergrowth_nbs", "weight", extrapolate = "none")$status,
    c("ok", "ok")
  )
})

test_that("growth_st3_z() and growth_st3_y() follow the ST3 formulas", {
  mu <- 1.905973
  sigma <- 0.4027349
  nu <- 1.095261
  tau <- 17.05299
  f <- function(y) {
    ifelse(y < mu, 2 * stats::pt(nu * (y - mu) / sigma, tau) / (1 + nu^2),
      1 - 2 * nu^2 * stats::pt((y - mu) / (sigma * nu), tau, lower.tail = FALSE) / (1 + nu^2)
    )
  }
  y <- c(1, 1.5, 1.9, 2.3, 2.8)
  expect_equal(growth_st3_z(y, mu, sigma, nu, tau), stats::qnorm(f(y)), tolerance = 1e-12)
  expect_equal(growth_st3_z(growth_st3_y(-4:4, mu, sigma, nu, tau), mu, sigma, nu, tau), -4:4, tolerance = 1e-10)
  # With nu = 1 it is a t distribution centred on mu
  expect_equal(growth_st3_y(0, mu, sigma, 1, tau), mu)
  expect_true(is.na(growth_st3_z(2, mu, -1, nu, tau)))
})
