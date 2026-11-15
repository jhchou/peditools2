# growth_height_velocity(): the cross-check against the PediTools engine (fixtures/height_velocity.csv.gz,
# written by the engine's export: both sexes, every onset, intervals of 4 to 30 months, mid-ages 1.6 to 19 years,
# velocities from -0.4 to 9.4 cm/year), and the formulas worked by hand.

test_that("every value equals the engine's", {
  f <- utils::read.csv(gzfile(testthat::test_path("fixtures", "height_velocity.csv.gz")),
    stringsAsFactors = FALSE, na.strings = ""
  )
  expect_gt(nrow(f), 3000)
  cases <- unique(f[c("sex", "age1_days", "height1", "age2_days", "height2", "onset")])
  r <- growth_height_velocity(cases$sex, cases$age1_days, cases$height1, cases$age2_days, cases$height2, cases$onset)
  key <- function(d) paste(d$sex, d$age1_days, d$height1, d$age2_days, d$height2, d$onset, d$group)
  expect_equal(nrow(r), nrow(f))
  r <- r[match(key(f), key(r)), ]
  expect_false(anyNA(r$row))
  for (v in c("velocity", "gain", "interval_days", "interval_months", "mid_age_days", "z", "p")) {
    expect_identical(is.na(r[[v]]), is.na(f[[v]]), label = paste("NA in", v))
    expect_equal(r[[v]], f[[v]], tolerance = 1e-9, label = v)
  }
  expect_identical(r$chart, f$chart)
  expect_identical(r$status, f$status)
  expect_identical(r$interval_note, f$interval_note)
  # the grid reaches each case
  expect_setequal(unique(f$status), c("ok", "under_2_years", "short_interval", "not_positive", "outside"))
  expect_setequal(unique(f$interval_note), c(NA, "under_11_months", "under_12_months", "over_13_months"))
})

test_that("the formulas, by hand", {
  y <- function(v) growth_convert(v, "years", "days")
  r <- growth_height_velocity("m", y(9), 130, y(10), 135.5, onset = "average")
  expect_identical(r$group, c("all", "average"))
  expect_identical(r$chart, c("kelly_2014", "kelly_2014_average"))
  expect_equal(r$velocity, c(5.5, 5.5))
  expect_equal(r$z, c(0, 0), tolerance = 1e-12) # M 5.5 at mid-age 9.5 in both tables
  expect_equal(r$p, c(0.5, 0.5), tolerance = 1e-12)
  expect_identical(r$annual, c(TRUE, TRUE))
  # Between rows: L, M, S interpolated linearly (girls 9.75 and 10.0 years)
  r <- growth_height_velocity("f", y(9.4), 131.2, y(10.4), 137.9)
  lms <- c(0.83, 6.3, 0.22) + 0.6 * (c(0.81, 6.4, 0.22) - c(0.83, 6.3, 0.22))
  expect_equal(r$z, ((6.7 / lms[2])^lms[1] - 1) / (lms[1] * lms[3]), tolerance = 1e-9)
  expect_equal(r$z, growth_z(6.7, 9.9, "f", "kelly_2014", "height_velocity"), tolerance = 1e-12)
})

test_that("the interval rules and statuses", {
  y <- function(v) growth_convert(v, "years", "days")
  mo <- function(v) v * 30.4375
  expect_identical(growth_height_velocity("m", y(1.5), 80, y(2.6), 90)$status, "under_2_years")
  r <- growth_height_velocity("m", y(8), 125, y(8) + mo(5.9), 127)
  expect_identical(r$status, "short_interval")
  expect_true(is.na(r$velocity))
  r <- growth_height_velocity("f", 3000, 125, 3000 + c(332, 333, 363, 364, 365, 398, 399, mo(24)), 131)
  expect_identical(r$interval_note, c(
    "under_11_months", "under_12_months", "under_12_months", NA, NA, NA, "over_13_months",
    "over_13_months"
  ))
  expect_identical(r$annual, c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_false(anyNA(r$z)) # the SDS outside 11 to 13 months too, as on the website
  r <- growth_height_velocity("m", y(15), 170, y(16), c(170, 169.7))
  expect_identical(r$status, c("not_positive", "not_positive"))
  expect_equal(r$velocity, c(0, -0.3))
  expect_true(all(is.na(r$z)))
  r <- growth_height_velocity("m", y(3), 95, y(4), 102, onset = "earlier") # mid-age 3.5: before every table
  expect_identical(r$status, c("outside", "outside"))
  r <- growth_height_velocity("m", y(17.1), 176, y(18.1), 176.4, onset = "average") # 17.6: past the average table
  expect_identical(r$status, c("ok", "outside"))
  expect_warning(r <- growth_height_velocity(c("m", "x", "f"), 3000, 130, c(3400, 3400, 2900), 134), "Unknown sex")
  expect_identical(r$status, c("ok", "invalid", "invalid"))
})

test_that("the limits, old vs new (2026-11-15)", {
  # old: months of 30.4375 days (6 months 182.625 days; 11, 12 and 13 months at 334.8, 365.25 and 395.7 days)
  r <- growth_height_velocity("f", 3000, 125, 3000 + c(179, 180, 182, 334, 365, 396, 397), 131)
  expect_identical(r$status[1:3], c("short_interval", "ok", "ok")) # old: 180 and 182 days short_interval
  expect_identical(r$interval_note[3:7], c("under_11_months", "under_12_months", NA, NA, NA))
  # old: 334 days under_11_months (now under_12_months), 365 days under_12_months, 396 and 397 days over_13_months
})

test_that("the arguments", {
  expect_error(growth_height_velocity("m", 3000, 130, 3400, 134, onset = "early"), "`onset`")
  expect_error(growth_height_velocity("m", "3000", 130, 3400, 134), "`age1_days`")
  r <- growth_height_velocity(c("m", "f"), 3000, 130, 3400, 134, onset = c(NA, "later"))
  expect_identical(r$row, c(1L, 2L, 2L))
  expect_identical(r$group, c("all", "all", "later"))
})
