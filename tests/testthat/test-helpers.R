test_that("growth_convert() uses the engine's exact factors", {
  expect_identical(growth_convert(1, "lb", "kg"), 0.45359237)
  expect_identical(growth_convert(16, "oz", "lb"), 1)
  expect_identical(growth_convert(1, "in", "cm"), 2.54)
  expect_identical(growth_convert(1, "ft", "in"), 12)
  expect_identical(growth_convert(30.4375, "days", "months"), 1)
  expect_identical(growth_convert(1, "years", "months"), 12)
  expect_identical(growth_convert(14, "days", "weeks"), 2)
  expect_identical(growth_convert(1500, "g", "kg"), 1.5)
  expect_identical(growth_convert(c(1, NA), "kg", "kg"), c(1, NA))
  # As the engine: value * factor(from) / factor(to)
  expect_identical(growth_convert(200, "days", "months"), 200 * 1 / 30.4375)
  expect_equal(growth_convert(growth_convert(3.2, "kg", "lb"), "lb", "kg"), 3.2)
  expect_identical(growth_convert(c(7, 30.4375), "days", c("weeks", "months")), c(1, 1))
  expect_error(growth_convert(1, "kg", "cm"), "Cannot convert kg to cm")
  expect_error(growth_convert(1, "stone", "kg"), "Unknown unit: \"stone\"")
})

test_that("growth_age_days() counts whole calendar days", {
  expect_identical(growth_age_days("2024-02-10", c("2024-02-29", "2024-03-01", "2025-02-10")), c(19L, 20L, 366L))
  expect_identical(growth_age_days(as.Date("2026-03-28"), as.Date("2026-03-30")), 2L) # across a DST change
  expect_identical(growth_age_days("2026-01-10", "2026-01-01"), -9L)
  expect_warning(d <- growth_age_days("2026-01-01", c("2026-02-30", "01/02/2026", NA)), "2 `date` values not a date")
  expect_identical(d, c(NA_integer_, NA_integer_, NA_integer_))
  expect_error(growth_age_days(Sys.time(), "2026-01-01"), "date-times are not accepted")
})

test_that("corrected age and PMA as the engine (due date 280 days)", {
  # Born at 28 3/7 weeks (199 days), 100 days old: corrected 100 - 81 = 19 days, PMA 299 days
  expect_identical(growth_corrected_age(100, 28, 3), 19)
  expect_identical(growth_pma(100, 28, 3, units = "days"), 299)
  expect_identical(growth_pma(100, 28, 3), 299 / 7)
  expect_identical(growth_corrected_age(100, 28, 3, units = "months"), 19 / 30.4375)
  # Before the due date: negative
  expect_identical(growth_corrected_age(10, 30), 10 - 70)
  # GA in days only
  expect_identical(growth_corrected_age(100, 0, 199), 19)
  # Vectorised
  expect_identical(growth_pma(c(0, 7, 14), 30, 0), c(30, 31, 32))
  expect_error(growth_pma(10, -1), "must not be negative")
  expect_error(growth_pma(10, 30, units = "hours"), "`units` must be one of")
})

test_that("the helpers feed the calculations", {
  age <- growth_age_days("2025-06-01", "2026-03-01")
  x <- growth_convert(age, "days", growth_charts("who_2006_infant")$x_units[1])
  expect_equal(x, 273 / 30.4375)
  expect_false(is.na(growth_z(8.5, x, "f", "who_2006_infant", "weight")))
  # A preterm chart in weeks from GA at birth and age
  expect_false(is.na(growth_z(1.4, growth_pma(30, 26, 4), "m", "fenton_2003", "weight")))
})
