# Reference values: the PediTools engine, 2026-10-24, printed to 17 significant digits

engine_z <- read.csv(text = "chart,measure,sex,x,y,z
who_2006_infant,weight,m,6,7.9000000000000004,-0.039180540868014334
who_2006_infant,weight,m,6,3.5,-6.1857545369606584
who_2006_infant,weight,m,6,12.5,4.4088620212375123
who_2006_infant,length,f,8.3000000000000007,70,0.34958737118849204
fenton_2003,weight,m,22.199999999999999,0.5,0.57660636971380796
fenton_2003,weight,m,50.799999999999997,4,-1.9206875118512865
who_expanded,length_height,m,730.39999999999998,88,0.064885223072098264
who_expanded,length_height,m,730.5,88,0.28453826302592478
who_expanded,length_height,m,730.70000000000005,88,0.28453826302592478
who_canada_2014_2_19,bmi,m,24,16,-0.015169995129217757
who_canada_2014_2_19,bmi,m,24.010000000000002,16,-0.015169995129217757
cdc_2000_infant,weight,f,0,3.2999999999999998,-0.20380210465543552
cdc_2000_bmi,bmi,m,150,20,0.66459807458031905
who_2006_infant,weight,m,-0.0001,3.3999999999999999,0.10912474665759195
zemel_2015_wt_for_len,weight,m,91,13,-0.64750851647646557
who_wt_for_len,weight,f,72.349999999999994,8.0999999999999996,-0.71978076820843162", stringsAsFactors = FALSE)

test_that("growth_z() equals the engine: interpolation, SD23, coverage, the step, bound tolerance", {
  expect_equal(growth_z(engine_z$y, engine_z$x, engine_z$sex, engine_z$chart, engine_z$measure), engine_z$z,
    tolerance = 1e-12
  )
})

test_that("growth_y() equals the engine", {
  expect_equal(
    growth_y(
      c(-3.5, 4, 2, -1.2), c(6, 6, 30.3, 100.5), c("m", "m", "f", "f"),
      c("who_2006_infant", "who_2006_infant", "fenton_2003", "cdc_2000_pedi"),
      c("weight", "weight", "length", "height")
    ),
    c(5.3305825412233983, 12.050010824735056, 45.090315726842839, 122.67892280142276),
    tolerance = 1e-12
  )
})

test_that("the CDC extended BMI method equals the engine, and inverts", {
  d <- growth_detail(c(30, 25, 45), c(150, 60, 239.5), c("m", "f", "f"), "cdc_2000_bmi", "bmi")
  expect_equal(d$p95, c(24.673862699115134, 18.240413538241842, 31.730686311727343), tolerance = 1e-12)
  expect_equal(d$ext_sigma, c(5.445924999999999, 2.6619000000000002, 7.8037647569444433), tolerance = 1e-12)
  expect_equal(d$p, c(98.359638665065802, 99.944476079535804, 99.554696401165913) / 100, tolerance = 1e-12)
  expect_equal(d$z, c(2.1344345404505192, 3.2609290038973033, 2.6156387824592748), tolerance = 1e-10)
  expect_identical(d$method, rep("cdc_bmi_ext", 3))
  expect_equal(growth_y(d$z, d$x, c("m", "f", "f"), "cdc_2000_bmi", "bmi"), d$y, tolerance = 1e-12)
  expect_equal(growth_y(p = d$p, x = d$x, sex = c("m", "f", "f"), chart = "cdc_2000_bmi", measure = "bmi"), d$y,
    tolerance = 1e-12
  )
  # Below P95: plain LMS; at P95 both methods meet
  below <- growth_detail(20, 150, "m", "cdc_2000_bmi", "bmi")
  expect_identical(below$method, "lms")
  expect_equal(below$z, 0.66459807458031905, tolerance = 1e-12)
  expect_equal(growth_z(d$p95[1], 150, "m", "cdc_2000_bmi", "bmi"), stats::qnorm(0.95), tolerance = 1e-12)
  expect_equal(growth_y(p = 0.95, x = 150, sex = "m", chart = "cdc_2000_bmi", measure = "bmi"), d$p95[1],
    tolerance = 1e-12
  )
  # Off: plain LMS above P95 too
  off <- growth_detail(45, 239.5, "f", "cdc_2000_bmi", "bmi", cdc_bmi_ext = FALSE)
  expect_identical(off$method, "lms")
  expect_equal(off$z, growth_lms_z(45, off$L, off$M, off$S), tolerance = 1e-14)
  expect_false(isTRUE(all.equal(off$z, d$z[3])))
})

test_that("the legacy peditools package's test values still hold", {
  x <- c(9.835308, 10.161536, 10.458854, 9.043262, 9.366594, 9.666089) # medians
  age <- c(10.5, 11.5, 12.5, 10.5, 11.5, 12.5)
  sex <- c("m", "m", "m", "f", "f", "f")
  expect_equal(round(growth_z(x, age, sex, "cdc_2000_infant", "weight"), 4), rep(0, 6))
  x10 <- c(8.542195484, 8.833485623, 9.098245709, 7.868436369, 8.166068895, 8.441459662)
  expect_equal(round(growth_p(x10, age, sex, "cdc_2000_infant", "weight"), 4), rep(0.1, 6))
})

test_that("rows are used as published, and interpolated linearly between them", {
  t <- growth_table("cdc_2000_infant", "weight", "f")
  at <- growth_params(t$x[3:4], "f", "cdc_2000_infant", "weight")
  expect_identical(at$L, t$L[3:4])
  mid <- growth_params((t$x[3] + t$x[4]) / 2, "f", "cdc_2000_infant", "weight")
  expect_equal(mid$M, (t$M[3] + t$M[4]) / 2, tolerance = 1e-14)
  # The step: lying length to day 730, standing height from 730.5
  w <- growth_table("who_expanded", "length_height", "m")
  expect_identical(growth_params(730.49, "m", "who_expanded", "length_height")$M, w$M[w$x == 730])
  expect_identical(growth_params(730.5, "m", "who_expanded", "length_height")$M, w$M[w$x == 731])
})

test_that("extrapolate: coverage, none, linear", {
  # Fenton 2003: coverage from 22 weeks, first row at 22.5
  expect_identical(growth_params(22.2, NA, "fenton_2003", "weight")$extrapolated, TRUE)
  expect_warning(z <- growth_z(0.5, 22.2, NA, "fenton_2003", "weight", extrapolate = "none"), "1 value NA: x outside")
  expect_true(is.na(z))
  expect_warning(z <- growth_z(0.5, 21, NA, "fenton_2003", "weight"), "outside")
  expect_true(is.na(z))
  lin <- growth_detail(0.5, 21, NA, "fenton_2003", "weight", extrapolate = "linear")
  expect_identical(c(lin$status, lin$beyond_coverage, lin$extrapolated), c("ok", "TRUE", "TRUE"))
  t <- growth_table("fenton_2003", "weight")
  slope <- (t$M[2] - t$M[1]) / (t$x[2] - t$x[1])
  expect_equal(lin$M, t$M[1] + slope * (21 - t$x[1]), tolerance = 1e-14)
  # Within 5 minutes outside a bound: at the bound
  expect_identical(growth_detail(3.4, -1e-4, "m", "who_2006_infant", "weight")$status, "ok")
  expect_identical(growth_detail(3.4, -3e-4, "m", "who_2006_infant", "weight")$status, "outside")
  # A coverage starting at the step: linear below it uses the rows after the step
  b <- growth_table("who_canada_2014_2_19", "bmi", "m")
  after <- b[b$x > 24, ][1:2, ]
  below <- growth_params(23, "m", "who_canada_2014_2_19", "bmi", extrapolate = "linear")
  expect_equal(below$M, after$M[1] + (after$M[2] - after$M[1]) / (after$x[2] - after$x[1]) * (23 - after$x[1]),
    tolerance = 1e-14
  )
  expect_identical(growth_detail(16, 23.99, "m", "who_canada_2014_2_19", "bmi")$status, "outside")
  # Per-sex coverage: Zemel weight-for-length girls to 90 cm, boys to 93
  expect_warning(growth_z(13, 91, "f", "zemel_2015_wt_for_len", "weight"), "outside")
  expect_false(is.na(growth_z(13, 91, "m", "zemel_2015_wt_for_len", "weight")))
})

test_that("sd23 switches the restricted method", {
  expect_equal(growth_z(3.5, 6, "m", "who_2006_infant", "weight"), -6.1857545369606584, tolerance = 1e-12)
  plain <- growth_z(3.5, 6, "m", "who_2006_infant", "weight", sd23 = FALSE)
  lms <- growth_params(6, "m", "who_2006_infant", "weight")
  expect_equal(plain, growth_lms_z(3.5, lms$L, lms$M, lms$S), tolerance = 1e-14)
  # Forced on a chart that does not prescribe it
  cdc <- growth_params(6, "m", "cdc_2000_infant", "weight")
  expect_equal(growth_z(3.5, 6, "m", "cdc_2000_infant", "weight", sd23 = TRUE),
    growth_lms_z(3.5, cdc$L, cdc$M, cdc$S, sd23 = TRUE),
    tolerance = 1e-14
  )
  # Within +-3 SD: the same either way
  expect_identical(
    growth_z(7.9, 6, "m", "who_2006_infant", "weight", sd23 = TRUE),
    growth_z(7.9, 6, "m", "who_2006_infant", "weight", sd23 = FALSE)
  )
  expect_error(growth_z(30, 150, "m", "cdc_2000_bmi", "bmi", sd23 = TRUE), "conflicts with the CDC extended")
  expect_false(is.na(growth_z(30, 150, "m", "cdc_2000_bmi", "bmi", sd23 = TRUE, cdc_bmi_ext = FALSE)))
  expect_error(growth_z(8, 6, "m", "cdc_2000_infant", "weight", cdc_bmi_ext = TRUE), "defined only for cdc_2000_bmi")
  expect_error(growth_z(8, 6, "m", "cdc_2000_infant", "weight", sd23 = NA), "NULL, TRUE or FALSE")
})

test_that("round trips: y -> z -> y on every method", {
  y <- c(3.5, 7.9, 12.5)
  z <- growth_z(y, 6, "m", "who_2006_infant", "weight")
  expect_equal(growth_y(z, 6, "m", "who_2006_infant", "weight"), y, tolerance = 1e-12)
  p <- growth_p(y, 6, "m", "who_2006_infant", "weight")
  expect_equal(growth_y(p = p, x = 6, sex = "m", chart = "who_2006_infant", measure = "weight"), y, tolerance = 1e-9)
})

test_that("vectorised over sex, chart, measure and x; recycling", {
  z <- growth_z(
    c(8, 70, 7.5), c(9, 9, 8.1), c("f", "male", "F"), c("cdc_2000_infant", "who_2006_infant", "cdc_2000_infant"),
    c("weight", "length", "weight")
  )
  expect_identical(z, c(
    growth_z(8, 9, "f", "cdc_2000_infant", "weight"), growth_z(70, 9, "m", "who_2006_infant", "length"),
    growth_z(7.5, 8.1, "f", "cdc_2000_infant", "weight")
  ))
  expect_length(growth_y(-2:2, 12, "m", "who_2006_infant", "weight"), 5)
  expect_error(growth_z(1:3, 1:2, "m", "who_2006_infant", "weight"), "length 1 or a common length")
  expect_identical(growth_z(numeric(0), 1, "m", "who_2006_infant", "weight"), numeric(0))
})

test_that("missing, invalid and unknown inputs", {
  expect_silent(z <- growth_z(c(8, NA, 8), c(9, 9, NA), "f", "cdc_2000_infant", "weight"))
  expect_identical(is.na(z), c(FALSE, TRUE, TRUE))
  expect_warning(z <- growth_z(8, 9, "x", "cdc_2000_infant", "weight"), "Unknown sex code")
  expect_true(is.na(z))
  expect_warning(z <- growth_z(c(-1, 0), 9, "f", "cdc_2000_infant", "weight"), "2 values NA: a measurement")
  expect_true(all(is.na(z)))
  expect_identical(growth_detail(c(-1, 8), 9, "f", "cdc_2000_infant", "weight")$status, c("invalid", "ok"))
  expect_error(growth_z(8, 9, "f", "cdc_2000_infnat", "weight"), "Unknown chart: cdc_2000_infnat")
  expect_error(growth_z(8, 9, "f", c("cdc_2000_infant", "cdc_2000_bmi"), "weight"), "cdc_2000_bmi / weight")
  expect_true(is.na(growth_z(8, 9, "f", NA_character_, "weight")))
  expect_error(growth_y(x = 9, sex = "f", chart = "cdc_2000_infant", measure = "weight"), "exactly one of")
  expect_warning(
    growth_y(p = c(0, 1, 0.5), x = 9, sex = "f", chart = "cdc_2000_infant", measure = "weight"),
    "2 values NA"
  )
  # One table for both sexes: any sex, NA too
  expect_identical(
    growth_z(1.2, 30, NA, "fenton_2003", "weight"),
    growth_z(1.2, 30, "m", "fenton_2003", "weight")
  )
})

test_that("an undefined LMS value is NA with its status", {
  # L > 0: no value at a very low Z
  t <- growth_table("cdc_2000_infant", "weight", "f")
  l <- t$L[1]
  expect_gt(l, 0)
  zlow <- -1 / (l * t$S[1]) - 1
  expect_identical(growth_detail(
    z = zlow, x = 0, sex = "f", chart = "cdc_2000_infant", measure = "weight",
    sd23 = FALSE
  )$status, "undefined")
  expect_true(is.na(growth_lms_y(zlow, t$L[1], t$M[1], t$S[1])))
})

test_that("growth_lms_z() and growth_lms_y() follow the formulas", {
  expect_equal(growth_lms_z(10, 0.5, 9, 0.1), ((10 / 9)^0.5 - 1) / (0.5 * 0.1))
  expect_equal(growth_lms_z(10, 0, 9, 0.1), log(10 / 9) / 0.1)
  expect_equal(growth_lms_y(1.5, -0.2, 9, 0.1), 9 * (1 + -0.2 * 0.1 * 1.5)^(1 / -0.2))
  expect_equal(growth_lms_y(1.5, 0, 9, 0.1), 9 * exp(0.1 * 1.5))
  # Restricted: beyond +3, a line with the spread SD3 - SD2
  sd3 <- growth_lms_y(3, 0.5, 9, 0.1)
  sd2 <- growth_lms_y(2, 0.5, 9, 0.1)
  expect_equal(growth_lms_z(sd3 + 0.5 * (sd3 - sd2), 0.5, 9, 0.1, sd23 = TRUE), 3.5)
  expect_equal(growth_lms_y(3.5, 0.5, 9, 0.1, sd23 = TRUE), sd3 + 0.5 * (sd3 - sd2))
  expect_true(is.na(growth_lms_z(-1, 0.5, 9, 0.1)))
  expect_true(is.na(growth_lms_z(10, 0.5, -9, 0.1)))
  expect_error(growth_lms_z(10, 0.5, 9, 0.1, sd23 = NA), "TRUE or FALSE")
})

test_that("INTERGROWTH-21st: values below 0 on the identity scale, as the engine", {
  # Fetal anterior ventricle: a normal model on the identity scale
  at <- growth_params(140, NA, "intergrowth_fet_brain", "anterior_ventricle")
  expect_identical(at$y_scale, "identity")
  z <- -at$mu / at$sigma - 1
  d <- growth_detail(z = z, x = 140, sex = NA, chart = "intergrowth_fet_brain", measure = "anterior_ventricle")
  expect_identical(d$status, "ok")
  expect_equal(d$y, -at$sigma, tolerance = 1e-12)
  expect_equal(growth_z(d$y, 140, NA, "intergrowth_fet_brain", "anterior_ventricle"), z, tolerance = 1e-12)
  # LMS: not positive is invalid
  expect_identical(growth_detail(-1, 9, "f", "cdc_2000_infant", "weight")$status, "invalid")
})
