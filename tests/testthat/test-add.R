# Charts added in the session: each test removes what it added

# Removes the given charts if they were added (also after a failed test)
cleanup <- function(ids) {
  charts <- growth_charts()
  growth_remove_chart(intersect(ids, charts$chart[charts$added]))
}

copy_of <- function(chart, measure, id) {
  t <- growth_table(chart, measure)
  t$chart <- id
  t
}

test_that("an added LMS chart gives the same results as the built-in one", {
  on.exit(cleanup("my_cdc"))
  added <- growth_add_chart(
    copy_of("cdc_2000_infant", "weight", "my_cdc"),
    data.frame(chart = "my_cdc", x_units = "months", measure_units = "kg")
  )
  expect_identical(added$sex, c("f", "m"))
  expect_true(all(added$added))
  expect_false(any(growth_charts("cdc_2000_infant")$added))
  x <- c(0, 4.7, 9.5, 35.9)
  y <- c(3.3, 6.4, 8.1, 14)
  expect_identical(
    growth_z(y, x, "f", "my_cdc", "weight"),
    growth_z(y, x, "f", "cdc_2000_infant", "weight")
  )
  expect_identical(nrow(growth_references("my_cdc")), 0L)
  expect_true(is.na(growth_charts("my_cdc")$sources_url[1]))
})

test_that("the layout with age, age_units and gender, one table for both sexes, and the coverage rules", {
  on.exit(cleanup("my_fenton"))
  t <- growth_table("fenton_2003", "weight")
  csv <- data.frame(
    chart = "my_fenton", age = t$x, age_units = "weeks", gender = NA, measure = "weight",
    measure_units = "kg", L = t$L, M = t$M, S = t$S
  )
  # Published from 22 weeks, half a row step before the first row (22.5)
  added <- growth_add_chart(csv, data.frame(chart = "my_fenton", age_min = 22, age_max = 50 + 6 / 7))
  expect_true(is.na(added$sex))
  expect_identical(
    growth_z(0.5, 22.2, "m", "my_fenton", "weight"),
    growth_z(0.5, 22.2, NA, "fenton_2003", "weight")
  )
  # More than half a row step beyond the first row: not a coverage
  expect_error(
    growth_add_chart(csv, data.frame(chart = "my_fenton", age_min = 21.9), overwrite = TRUE),
    "more than half a row step"
  )
  expect_error(
    growth_add_chart(csv, data.frame(chart = "my_fenton", age_min = 23), overwrite = TRUE),
    "above the first row"
  )
})

test_that("an added chart with one sex has no table for the other", {
  on.exit(cleanup("my_girls"))
  t <- copy_of("who_2006_infant", "length", "my_girls")
  growth_add_chart(t[t$sex == "f", ], data.frame(chart = "my_girls", x_units = "months", measure_units = "cm"))
  expect_false(is.na(growth_z(70, 9, "f", "my_girls", "length")))
  expect_warning(z <- growth_z(70, 9, "m", "my_girls", "length"), "no value")
  expect_true(is.na(z))
})

test_that("metadata: SD23, the step, labels", {
  on.exit(cleanup(c("my_who", "my_daily")))
  growth_add_chart(copy_of("who_2006_infant", "weight", "my_who"), data.frame(chart = "my_who", z_method = "sd23"))
  expect_identical(
    growth_z(3.5, 6, "m", "my_who", "weight"),
    growth_z(3.5, 6, "m", "who_2006_infant", "weight")
  )
  daily <- copy_of("who_expanded", "length_height", "my_daily")
  growth_add_chart(daily, data.frame(chart = "my_daily", x_break = 730.5, x_units = "days"))
  expect_identical(
    growth_z(88, c(730.4, 730.6), "m", "my_daily", "length_height"),
    growth_z(88, c(730.4, 730.6), "m", "who_expanded", "length_height")
  )
  expect_error(
    growth_add_chart(daily, data.frame(chart = "my_daily", x_break = 731), overwrite = TRUE),
    "not between two rows"
  )
  expect_error(
    growth_add_chart(daily, data.frame(chart = "my_daily", x_label = "weight"), overwrite = TRUE),
    "must be one of age"
  )
})

test_that("ST3 and equations: an added INTERGROWTH-21st newborn chart gives the built-in results", {
  on.exit(cleanup(c("my_nbs", "my_png")))
  n <- copy_of("intergrowth_nbs", "weight", "my_nbs")
  e <- growth_equations("intergrowth_nbs", "weight")
  e$chart <- "my_nbs"
  meta <- data.frame(chart = "my_nbs", x_min = 168, x_max = 300, x_units = "days", measure_units = "kg")
  added <- growth_add_chart(list(n, e), meta)
  expect_identical(unique(added$family), "normal, st3")
  x <- c(168, 200, 230, 231, 260, 300)
  y <- c(0.5, 1.2, 1.8, 1.9, 2.6, 3.5)
  for (sex in c("m", "f")) {
    expect_equal(growth_z(y, x, sex, "my_nbs", "weight"), growth_z(y, x, sex, "intergrowth_nbs", "weight"),
      tolerance = 1e-14
    )
  }
  # Equations need their coverage
  p <- growth_equations("intergrowth_png", "weight")
  p$chart <- "my_png"
  expect_error(growth_add_chart(p), "need the coverage")
  growth_add_chart(p, data.frame(chart = "my_png", x_min = 27, x_max = 64))
  expect_equal(growth_y(0, 40, "f", "my_png", "weight"), growth_y(0, 40, "f", "intergrowth_png", "weight"))
  # A narrower coverage is fine; one starting before the first part is not
  growth_add_chart(p, data.frame(chart = "my_png", x_min = 28, x_max = 64), overwrite = TRUE)
  expect_warning(growth_z(1, 27.5, "f", "my_png", "weight"), "outside")
  expect_error(
    growth_add_chart(p, data.frame(chart = "my_png", x_min = 26, x_max = 64), overwrite = TRUE),
    "after the coverage minimum"
  )
})

test_that("built-in charts are protected; overwrite replaces, removing restores", {
  t <- growth_table("cdc_2000_infant", "weight")
  t$M <- t$M * 2
  expect_error(growth_add_chart(t), "already available: cdc_2000_infant")
  before <- growth_y(0, 9, "f", "cdc_2000_infant", "weight")
  growth_add_chart(t, overwrite = TRUE)
  expect_equal(growth_y(0, 9, "f", "cdc_2000_infant", "weight"), 2 * before)
  expect_identical(unique(growth_charts("cdc_2000_infant")$measure), "weight") # the whole chart is replaced
  growth_remove_chart("cdc_2000_infant")
  expect_identical(growth_y(0, 9, "f", "cdc_2000_infant", "weight"), before)
  expect_setequal(unique(growth_charts("cdc_2000_infant")$measure), c("head_circ", "length", "weight"))
  expect_error(growth_remove_chart("cdc_2000_bmi"), "Not added in this session")
})

test_that("invalid parameters are refused with a clear message", {
  t <- copy_of("cdc_2000_infant", "weight", "my_bad")
  expect_error(growth_add_chart(t[c(1, 1, 2), ]), "strictly increasing")
  bad <- t
  bad$M[3] <- -1
  expect_error(growth_add_chart(bad), "M and S positive")
  expect_error(growth_add_chart(data.frame(chart = "x", a = 1)), "not in a known layout")
  expect_error(growth_add_chart(t[, setdiff(names(t), "measure")]), "need the column")
  expect_error(growth_add_chart(t, data.frame(chart = "other")), "charts not in `params`")
  expect_error(growth_add_chart(t, data.frame(chart = "my_bad", z_method = "cdf")), "z_method")
  e <- growth_equations("intergrowth_png", "weight")
  e$chart <- "my_bad"
  e$model <- "lms"
  expect_error(growth_add_chart(e), "Only normal models")
  expect_false("my_bad" %in% growth_charts()$chart)
})

test_that("added charts carry their own metadata: references and a sources page", {
  on.exit(cleanup("my_cdc"))
  refs <- data.frame(
    chart = "my_cdc", measure = c(NA, "weight", NA),
    role = c("clinical", "method", NA),
    citation = c("Author A. A chart. J Growth 2026;1:1-10.", "Author B. A method. Stat J 2025;2:3-4.", "Author C."),
    doi = c("10.1000/example", NA, NA), stringsAsFactors = FALSE
  )
  t <- rbind(copy_of("cdc_2000_infant", "weight", "my_cdc"), copy_of("cdc_2000_infant", "length", "my_cdc"))
  growth_add_chart(t,
    metadata = data.frame(chart = "my_cdc", sources_url = "https://example.org/my_cdc"),
    references = refs
  )
  expect_identical(unique(growth_charts("my_cdc")$sources_url), "https://example.org/my_cdc")
  r <- growth_references("my_cdc")
  # The chart's references for each measure; the method for weight only
  expect_identical(nrow(r), 5L)
  expect_identical(r$role[r$measure == "weight"], c("clinical", "clinical", "method"))
  expect_identical(r$position[r$measure == "weight"], c(1L, 2L, 1L))
  expect_identical(r$short[r$role == "method"], r$citation[r$role == "method"])
  expect_identical(unique(r$sources_url), "https://example.org/my_cdc")
  expect_identical(unique(r$doi[!is.na(r$doi)]), "10.1000/example")
  # Replacing the chart replaces its references; built-in references are untouched
  growth_add_chart(t, overwrite = TRUE)
  expect_identical(nrow(growth_references("my_cdc")), 0L)
  expect_gt(nrow(growth_references("cdc_2000_infant")), 0)
  expect_error(
    growth_add_chart(t, references = data.frame(chart = "my_cdc", role = "other", citation = "x"), overwrite = TRUE),
    "role` must be one of"
  )
  expect_error(
    growth_add_chart(t, references = data.frame(chart = "my_cdc", measure = "bmi", citation = "x"), overwrite = TRUE),
    "a measure the chart does not have"
  )
  expect_error(growth_add_chart(t, references = data.frame(chart = "my_cdc"), overwrite = TRUE), "`citation`")
})
