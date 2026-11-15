# growth_target_height(): the cross-check against the PediTools engine (tests/testthat/fixtures/target_height.csv.gz,
# written by the engine's export: every method, both sexes and references, parents' heights and children's ages and
# heights), and the formulas worked by hand.

all_methods <- c("tanner", "hermanussen_cole", "wright_cheetham", "jgz", "luo")

test_that("every value equals the engine's", {
  f <- utils::read.csv(gzfile(testthat::test_path("fixtures", "target_height.csv.gz")),
    stringsAsFactors = FALSE, na.strings = ""
  )
  expect_gt(nrow(f), 500)
  cases <- unique(f[c("sex", "father", "mother", "reference", "age_days", "height")])
  with_child <- !is.na(cases$age_days)
  r <- rbind(
    growth_target_height(cases$sex[!with_child], cases$father[!with_child], cases$mother[!with_child],
      cases$reference[!with_child],
      methods = all_methods
    ),
    growth_target_height(cases$sex[with_child], cases$father[with_child], cases$mother[with_child],
      cases$reference[with_child],
      methods = all_methods, age_days = cases$age_days[with_child], height = cases$height[with_child]
    )
  )
  key <- function(d) paste(d$sex, d$father, d$mother, d$reference, d$age_days, d$height, d$method)
  r <- r[match(key(f), key(r)), ]
  expect_false(anyNA(r$row))
  for (v in c(
    "target", "target_z", "target_p", "low", "high", "low_aap", "high_aap", "mph_z", "mph_cm", "child_z",
    "child_p", "diff_z"
  )) {
    expect_identical(is.na(r[[v]]), is.na(f[[v]]), label = paste("NA in", v))
    expect_equal(r[[v]], f[[v]], tolerance = 1e-9, label = v)
  }
  expect_identical(r$child_chart, f$child_chart)
  expect_identical(r$child_status, f$child_status)
  # the grid reaches each case: under 2 years, each WHO table, past each reference's end
  expect_setequal(unique(f$child_status), c(NA, "ok", "under_2_years", "outside"))
  expect_setequal(unique(f$child_chart), c(NA, "cdc_2000_pedi", "who_2007_5_19", "who_expanded"))
})

test_that("the formulas, by hand", {
  r <- growth_target_height(c("m", "f"), 180, 165, methods = all_methods)
  m <- r[r$sex == "m", ]
  g <- r[r$sex == "f", ]
  rownames(m) <- m$method
  rownames(g) <- g$method
  expect_equal(m["tanner", "target"], (180 + 165 + 13) / 2)
  expect_equal(g["tanner", "target"], (180 + 165 - 13) / 2)
  expect_equal(g["tanner", c("low", "high", "low_aap", "high_aap")],
    data.frame(low = 166 - 8.5, high = 166 + 8.5, low_aap = 166 - 10.2, high_aap = 166 + 10.2, row.names = "tanner"),
    ignore_attr = TRUE
  )
  expect_equal(m["jgz", "target"], 44.5 + 0.376 * 180 + 0.411 * 165)
  expect_equal(g["jgz", "target"], 47.1 + 0.334 * 180 + 0.364 * 165)
  expect_equal(m["jgz", "high"] - m["jgz", "target"], 11)
  expect_equal(g["jgz", "high"] - g["jgz", "target"], 10)
  expect_equal(m["luo", "target"], 45.99 + 0.78 * 172.5)
  expect_equal(g["luo", "target"], 37.85 + 0.75 * 172.5)
  # Hermanussen-Cole: the parents' SDS on CDC 2000 at 20 years, the father's on the male table
  zf <- growth_z(180, 240, "m", "cdc_2000_pedi", "height")
  zm <- growth_z(165, 240, "f", "cdc_2000_pedi", "height")
  expect_equal(g["hermanussen_cole", "mph_z"], (zf + zm) / 2)
  expect_equal(g["hermanussen_cole", "target_z"], 0.72 * (zf + zm) / 2)
  expect_equal(g["hermanussen_cole", "target"], growth_y(0.72 * (zf + zm) / 2, 240, "f", "cdc_2000_pedi", "height"))
  expect_equal(g["hermanussen_cole", "low"], growth_y(0.72 * (zf + zm) / 2 - 1.64, 240, "f", "cdc_2000_pedi", "height"))
  # Wright-Cheetham: the mid-parental height +-7 cm as an SDS, halved
  expect_equal(m["wright_cheetham", "mph_cm"], 172.5 + 7)
  zw <- growth_z(172.5 + 7, 240, "m", "cdc_2000_pedi", "height")
  expect_equal(m["wright_cheetham", "target_z"], 0.5 * zw)
  expect_equal(m["wright_cheetham", "high"], growth_y(0.5 * zw + 1.4, 240, "m", "cdc_2000_pedi", "height"))
  expect_equal(r$target_p, stats::pnorm(r$target_z))
})

test_that("the child: from 2 years, on the reference's charts", {
  r <- growth_target_height("f", 180, 165, "who", age_days = c(700, 1000, 3000, 7000), height = c(80, 95, 130, 170))
  r <- r[r$method == "tanner", ]
  expect_identical(r$child_status, c("under_2_years", "ok", "ok", "outside"))
  expect_identical(r$child_chart, c(NA, "who_expanded", "who_2007_5_19", NA))
  expect_equal(r$child_z[3], growth_z(130, growth_convert(3000, "days", "months"), "f", "who_2007_5_19", "height"))
  expect_equal(r$diff_z, r$child_z - r$target_z)
  expect_true(all(is.na(growth_target_height("m", 180, 165)$child_status)))
})

test_that("the defaults, the order and the errors", {
  expect_identical(growth_target_height("m", 180, 165)$method, c("tanner", "hermanussen_cole"))
  expect_identical(growth_target_height("m", 180, 165, methods = c("luo", "tanner"))$method, c("tanner", "luo"))
  expect_identical(growth_target_height(c("m", "f", "m"), 180, 165)$row, rep(1:3, each = 2))
  expect_error(growth_target_height("m", 180, 165, methods = "mean"), "methods")
  expect_error(growth_target_height("m", 180, 165, reference = "uk90"), "reference")
  expect_error(growth_target_height("m", 180, 165, age_days = 1000), "both")
  expect_warning(r <- growth_target_height("x", 180, 165), "Unknown sex")
  expect_true(all(is.na(r$target)))
})
