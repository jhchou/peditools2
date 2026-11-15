# growth_increments(): the cross-check against the PediTools engine (fixtures/increments.csv.gz, written by the
# engine's export: both sexes, visits from birth to past 24 months on and off WHO's target ages, intervals of 20 to
# 250 days, gains from losses to large, with and without a GA at birth), and WHO's worked examples.

test_that("every value equals the engine's", {
  f <- utils::read.csv(gzfile(testthat::test_path("fixtures", "increments.csv.gz")),
    stringsAsFactors = FALSE, na.strings = ""
  )
  expect_gt(nrow(f), 5000)
  cases <- unique(f[c(
    "sex", "age1_days", "age2_days", "weight1", "weight2", "head1", "head2", "length1", "length2",
    "ga_days"
  )])
  r <- growth_increments(cases$sex, cases$age1_days, cases$age2_days,
    weight1 = cases$weight1, weight2 = cases$weight2, head1 = cases$head1, head2 = cases$head2,
    length1 = cases$length1, length2 = cases$length2, ga_weeks = 0, ga_days = cases$ga_days
  )
  expect_identical(nrow(r), nrow(f))
  # the same order: per case, age, measure and result
  case_of <- match(
    do.call(paste, f[names(cases)]),
    do.call(paste, cases)
  )
  expect_identical(r$row, case_of)
  expect_identical(r$age, f$age)
  expect_identical(r$measure, f$measure)
  expect_identical(r$kind, f$kind)
  expect_identical(r$interval, f$interval)
  expect_identical(r$between_lengths, f$between_lengths)
  expect_identical(r$status, f$status)
  for (v in c("start_months", "end_months", "length_months", "gain", "gain_used", "z", "p")) {
    expect_identical(is.na(r[[v]]), is.na(f[[v]]), label = paste("NA in", v))
    expect_equal(r[[v]], f[[v]], tolerance = 1e-9, label = v)
  }
  # the grid reaches each case
  expect_setequal(unique(f$kind), c(NA, "published", "tabulated", "prorated", "interpolated"))
  expect_setequal(unique(f$status), c("ok", "no_z", "outside", "no_interval", "before_due"))
})

test_that("WHO's worked examples (2009, Ch. 6)", {
  t <- function(m) peditools2:::incr_target_days(m)
  cases <- list(
    list("m", 4, 6, weight1 = 7, weight2 = 9.2, z = 3.58), list("m", 16, 18, weight1 = 11, weight2 = 10.5, z = -3.27),
    list("m", 11, 13, weight1 = 9.5, weight2 = 10.7, z = 2.27),
    list("f", 9, 12, length1 = 70, length2 = 77.5, z = 3.80),
    list("f", 15, 18, length1 = 78, length2 = 78.5, z = -3.26), list("f", 3, 6, length1 = 59, length2 = 67, z = 1.90)
  )
  for (k in cases) {
    args <- c(list(k[[1]], t(k[[2]]), t(k[[3]])), k[setdiff(names(k), c("", "z"))])
    r <- do.call(growth_increments, args)
    pub <- r[r$kind == "published" & r$start_months == k[[2]] & r$end_months == k[[3]], ]
    expect_identical(nrow(pub), 1L)
    expect_equal(round(pub$z, 2), k$z)
    # at WHO's target ages the interpolated result is the same row
    expect_equal(r$z[r$kind == "interpolated"], pub$z, tolerance = 1e-12)
  }
})

test_that("measures, ages and statuses", {
  # a measure at one visit only is not assessed; none at both is invalid
  r <- growth_increments("m", 122, 183, weight1 = 7, weight2 = 9.2, length1 = 64)
  expect_identical(unique(r$measure), "weight")
  expect_warning(r <- growth_increments(c("m", "x"), 122, c(183, 183), weight1 = 7, weight2 = c(NA, 9)), "Unknown sex")
  expect_identical(r$status, c("invalid", "invalid"))
  expect_identical(growth_increments("m", 183, 122, weight1 = 7, weight2 = 9)$status, "invalid")
  # born at 28 weeks: corrected ages 84 days less; the earlier visit at 61 days is before the due date
  r <- growth_increments("f", 61, 122, weight1 = 3, weight2 = 4.4, ga_weeks = 28)
  expect_identical(unique(r$age), c("chronological", "corrected"))
  expect_identical(r$status[r$age == "corrected"], "before_due")
  # 37 weeks: no corrected age
  r <- growth_increments("f", 61, 122, weight1 = 5, weight2 = 6.2, ga_weeks = 37)
  expect_identical(unique(r$age), "chronological")
  # past 24 months
  expect_identical(growth_increments("f", 700, 760, weight1 = 10, weight2 = 10.4)$status, "outside")
  # length in the second year: a gain of 0 has no Z (WHO's distribution reaches 0 before -3 SD)
  r <- growth_increments("f", 670, 731, length1 = 85, length2 = 85)
  expect_identical(r$status[r$kind == "published"], "no_z")
  # a prorated suggestion: 11 months to 13 months 20 days, 600 g, prorated to the 3-month interval's 91 days
  r <- growth_increments("m", 335, 416, weight1 = 9, weight2 = 9.6)
  s <- r[r$kind == "prorated", ]
  expect_identical(c(s$start_months, s$end_months), c(11, 14))
  expect_equal(s$gain_used, 600 * 91 / 81, tolerance = 1e-12)
})

test_that("the arguments", {
  expect_error(growth_increments("m", "122", 183, weight1 = 7, weight2 = 9), "`age1_days`")
  expect_error(growth_increments("m", 122, 183, weight1 = 7, weight2 = 9, ga_weeks = -1), "must not be negative")
})
