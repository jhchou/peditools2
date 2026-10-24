test_that("growth_charts() describes every table", {
  charts <- growth_charts()
  expect_true(nrow(charts) > 0)
  expect_false(anyDuplicated(paste(charts$chart, charts$measure, charts$sex)) > 0)
  expect_true(all(charts$sex %in% c("m", "f", NA)))
  expect_true(all(charts$family %in% c("lms", "st3", "normal", "normal, st3")))
  expect_true(all(charts$z_method %in% c("lms", "sd23", "cdf")))
  expect_true(all(charts$x_min < charts$x_max))
  expect_true(all(nzchar(charts$x_units) & nzchar(charts$measure_units)))
  expect_identical(charts$on_website, !is.na(charts$calculators))
  expect_true(all(startsWith(charts$sources_url, "https://peditools.org/growth_engine/sources.php#")))
  # A chart's sexes: either one table for both, or one for each
  per_chart <- tapply(charts$sex, paste(charts$chart, charts$measure), function(s) {
    identical(s, NA_character_) || setequal(s, c("m", "f"))
  })
  expect_true(all(per_chart))
})

test_that("growth_charts() reports the published method and coverage", {
  who <- growth_charts("who_2006_infant")
  expect_identical(who$z_method[who$measure == "weight"], c("sd23", "sd23"))
  expect_identical(unique(who$z_method[who$measure == "length"]), "lms")

  fenton <- growth_charts("fenton_2003")
  expect_true(all(is.na(fenton$sex)))
  expect_equal(fenton$x_min, rep(22, 3))
  expect_equal(fenton$x_max, rep(50 + 6 / 7, 3), tolerance = 1e-8)

  # Zemel weight-for-length: each sex has its own coverage
  zemel <- growth_charts("zemel_2015_wt_for_len")
  expect_identical(zemel$sex, c("f", "m"))
  expect_false(identical(zemel$x_max[1], zemel$x_max[2]))

  # WHO's length/height table steps at 730.5 days
  who <- growth_charts("who_expanded")
  expect_equal(unique(who$x_break[who$measure == "length_height"]), 730.5)

  expect_true(all(is.na(growth_charts("intergrowth_fet")$sex)))
  nbs <- growth_charts("intergrowth_nbs")
  expect_identical(unique(nbs$family[nbs$measure == "weight"]), "normal, st3")
})

test_that("growth_charts() checks chart ids", {
  expect_error(growth_charts("cdc_2000_infnat"), "Unknown chart: cdc_2000_infnat")
  expect_error(growth_charts(1), "character")
})

test_that("growth_table() returns the published rows", {
  t <- growth_table("cdc_2000_infant", "weight", "f")
  expect_named(t, c("chart", "measure", "sex", "x", "L", "M", "S"))
  expect_identical(unique(t$sex), "f")
  expect_identical(
    unlist(t[t$x == 0, c("L", "M", "S")], use.names = FALSE),
    c(1.509187507, 3.39918645, 0.142106724)
  )
  expect_identical(t$x, sort(t$x))

  w <- growth_table("who_2006_infant", "weight", "male")
  expect_identical(unlist(w[w$x == 0, c("L", "M", "S")], use.names = FALSE), c(0.3487, 3.3464, 0.14602))

  # One table for both sexes: included for either sex, once
  f <- growth_table("fenton_2003", "weight", "m")
  expect_true(all(is.na(f$sex)))
  expect_identical(nrow(f), growth_charts("fenton_2003")$n_rows[1])
  expect_identical(nrow(growth_table("fenton_2003", "weight")), nrow(f))
})

test_that("growth_table() has the ST3 rows of INTERGROWTH-21st", {
  t <- growth_table("intergrowth_nbs", "weight", "m")
  expect_named(t, c("chart", "measure", "sex", "x", "mu", "sigma", "nu", "tau"))
  expect_identical(
    unlist(t[t$x == 231, c("mu", "sigma", "nu", "tau")], use.names = FALSE),
    c(1.905973, 0.4027349, 1.095261, 17.05299)
  )
  # Mixed families: the union of the columns
  both <- growth_table(c("intergrowth_nbs", "cdc_2000_infant"), "weight", "m")
  expect_named(both, c("chart", "measure", "sex", "x", "L", "M", "S", "mu", "sigma", "nu", "tau"))
  expect_identical(unique(both$chart), c("cdc_2000_infant", "intergrowth_nbs"))
  # Equations only: no rows
  expect_identical(nrow(growth_table("intergrowth_png", "weight")), 0L)
})

test_that("growth_table() is crossed and checks every pair", {
  t <- growth_table(c("cdc_2000_infant", "who_2006_infant"), c("weight", "length"))
  expect_setequal(
    unique(paste(t$chart, t$measure, t$sex)),
    as.vector(outer(
      c("cdc_2000_infant", "who_2006_infant"),
      as.vector(outer(c("weight", "length"), c("m", "f"), paste)), paste
    ))
  )
  expect_error(
    growth_table(c("cdc_2000_infant", "cdc_2000_bmi"), "weight"),
    "Unknown chart/measure \\(1\\):\n  cdc_2000_bmi / weight"
  )
  expect_error(growth_table("cdc_2000_infant", "weight", "x"), "Unknown sex code")
})

test_that("growth_equations() returns the INTERGROWTH-21st terms", {
  e <- growth_equations("intergrowth_png", "weight")
  expect_identical(nrow(e), 7L)
  expect_identical(e$coef[1:3], c(2.591277, -0.01155, -2201.705))
  expect_true(all(e$model == "normal"))
  # Shared terms (sex NA) come with either sex
  nbs <- growth_equations("intergrowth_nbs", "weight", "f")
  expect_true(all(is.na(nbs$sex) | nbs$sex == "f"))
  expect_true(any(is.na(nbs$sex)))
  expect_identical(nrow(growth_equations("cdc_2000_infant")), 0L)
})
