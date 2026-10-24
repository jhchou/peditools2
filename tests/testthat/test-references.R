test_that("every chart and measure has its publication", {
  charts <- growth_charts()
  refs <- growth_references()
  clinical <- unique(paste(refs$chart, refs$measure)[refs$role == "clinical"])
  expect_setequal(clinical, unique(paste(charts$chart, charts$measure)))
  expect_true(all(refs$role %in% c("clinical", "lms_data", "method", "resource")))
  expect_true(all(nzchar(refs$citation) & nzchar(refs$short)))
  expect_false(anyNA(refs$url))
  expect_false(anyNA(refs$sources_url))
})

test_that("growth_references() filters by chart and measure", {
  r <- growth_references("who_2006_infant", "weight")
  expect_identical(unique(r$chart), "who_2006_infant")
  expect_true("method" %in% r$role) # the SD23 method for weight
  expect_false("method" %in% growth_references("who_2006_infant", "length")$role)
  expect_identical(unique(growth_references(measure = "arm_circ")$measure), "arm_circ")
  expect_error(growth_references("who_2006_infant", "bmi"), "Unknown chart/measure")
})

test_that("growth_data_version() gives the date and commit", {
  v <- growth_data_version()
  expect_named(v, c("date", "commit"))
  expect_match(v[["date"]], "^\\d{4}-\\d{2}-\\d{2}$")
})
