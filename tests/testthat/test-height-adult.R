# growth_height_adult() and growth_bone_age(): the cross-check against the PediTools engine
# (fixtures/height_adult.csv.gz and bone_age.csv, written by the engine's export: both sexes, ages 3.5 to 19 years,
# bone ages from 50 months behind to 48 ahead, Khamis-Roche inside and outside 4 to 17.5 years), Bayley-Pinneau's
# worked examples, and by hand.

test_that("every value equals the engine's", {
  f <- utils::read.csv(gzfile(testthat::test_path("fixtures", "height_adult.csv.gz")),
    stringsAsFactors = FALSE, na.strings = ""
  )
  expect_gt(nrow(f), 500)
  bp <- f[f$method == "bayley_pinneau", ]
  kr <- f[f$method == "khamis_roche", ]
  r <- growth_height_adult(bp$sex, bp$age_days, bp$height, bone_age_months = bp$bone_age_months)
  expect_identical(r$method, rep("bayley_pinneau", nrow(bp)))
  for (v in c("predicted", "lead_months", "percent")) {
    expect_identical(is.na(r[[v]]), is.na(bp[[v]]), label = paste("NA in", v))
    expect_equal(r[[v]], bp[[v]], tolerance = 1e-9, label = v)
  }
  expect_identical(r$table_kind, bp$table_kind)
  expect_identical(r$table_used, bp$table_used)
  expect_identical(r$substituted, as.logical(bp$substituted))
  expect_identical(r$status, bp$status)
  expect_true(all(c("average", "accelerated", "retarded") %in% bp$table_used))
  expect_true(any(bp$substituted %in% TRUE))
  expect_true(any(bp$status == "outside"))

  r <- growth_height_adult(kr$sex, kr$age_days, kr$height, weight = kr$weight, father = kr$father, mother = kr$mother)
  expect_identical(r$method, rep("khamis_roche", nrow(kr)))
  for (v in c("predicted", "predicted_in", "mad_in", "p90_in")) {
    expect_identical(is.na(r[[v]]), is.na(kr[[v]]), label = paste("NA in", v))
    expect_equal(r[[v]], kr[[v]], tolerance = 1e-9, label = v)
  }
  expect_identical(r$status, kr$status)
  expect_true(all(c("ok", "outside") %in% kr$status))
})

test_that("bone ages are read as the engine reads them", {
  f <- utils::read.csv(testthat::test_path("fixtures", "bone_age.csv"),
    stringsAsFactors = FALSE, na.strings = "", strip.white = FALSE
  )
  f$text[is.na(f$text)] <- ""
  months <- suppressWarnings(growth_bone_age(f$text))
  expect_identical(months, as.integer(f$months))
  expect_warning(growth_bone_age("11.6"), "years and months")
  expect_identical(growth_bone_age(c("11-6", NA, "")), c(138L, NA, NA))
})

test_that("Bayley-Pinneau's worked examples (1952, p. 431)", {
  y <- function(v) growth_convert(v, "years", "days")
  # a 13-year-old boy, bone age 13-3, 60 in: Table IIB, 89.0%, 67.4 in; at 51.5 in, 57.9 in
  r <- growth_height_adult("m", y(13), c(60, 51.5) * 2.54, bone_age_months = 13 * 12 + 3)
  expect_identical(r$table_used, c("average", "average"))
  expect_identical(r$percent, c(89, 89))
  expect_identical(sprintf("%.1f", r$predicted / 2.54), c("67.4", "57.9"))
  # a 14-year-old boy, 67.5 in, bone age 15-6: Table IID (accelerated), 69.5 in
  r <- growth_height_adult("m", y(14), 67.5 * 2.54, bone_age_months = 15 * 12 + 6)
  expect_identical(c(r$table_used, sprintf("%.1f", r$predicted / 2.54)), c("accelerated", "69.5"))
  # a 12-year-old girl, bone age 10-3, 55 in: Table IIIE (retarded), 62.2 in
  r <- growth_height_adult("f", y(12), 55 * 2.54, bone_age_months = 10 * 12 + 3)
  expect_identical(c(r$table_used, sprintf("%.1f", r$predicted / 2.54)), c("retarded", "62.2"))
})

test_that("the formulas and statuses, by hand", {
  y <- function(v) growth_convert(v, "years", "days")
  # Khamis-Roche, a boy of 10.0: the 1995 erratum's row, inches and pounds
  r <- growth_height_adult("m", y(10), 55 * 2.54, weight = 70 * 0.45359237, father = 70 * 2.54, mother = 66 * 2.54)
  expect_equal(r$predicted_in, -11.0380 + 0.97135 * 55 - 0.039981 * 70 + 0.45932 * 68, tolerance = 1e-12)
  expect_equal(r$predicted, r$predicted_in * 2.54, tolerance = 1e-12)
  # between columns, linear: boys average 10-1, a third of the way from 10-0 (78.4) to 10-3 (79.1)
  r <- growth_height_adult("m", y(10), 140, bone_age_months = 121)
  expect_equal(r$percent, 78.4 + 0.7 / 3, tolerance = 1e-12)
  # boys retarded past 13-0: the average table, flagged
  r <- growth_height_adult("m", y(15), 160, bone_age_months = 162)
  expect_identical(list(r$table_kind, r$table_used, r$substituted, r$percent), list("retarded", "average", TRUE, 90.2))
  # both methods, Khamis-Roche first (as on the website); partial Khamis-Roche inputs are invalid
  r <- growth_height_adult("m", y(10), 140, bone_age_months = 120, weight = 32, father = 178, mother = 165)
  expect_identical(r$method, c("khamis_roche", "bayley_pinneau"))
  expect_identical(growth_height_adult("m", y(10), 140, weight = 32)$status, "invalid")
  expect_identical(growth_height_adult("m", y(3), 95, weight = 14, father = 178, mother = 165)$status, "outside")
  expect_identical(growth_height_adult("m", y(10), 140, bone_age_months = 120.5)$status, "invalid")
  expect_identical(nrow(growth_height_adult("m", y(10), 140)), 0L)
})
