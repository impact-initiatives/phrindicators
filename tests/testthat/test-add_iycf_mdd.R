# Tests for add_iycf_mdd

test_that("add_iycf_mdd() computes dietary diversity scores and the 5-of-8 threshold", {
  df <- make_iycf_data(
    age_months = c(6, 6, 24),
    iycf_4 = c("yes", "yes", "yes"),
    iycf_7b = c("yes", "yes", "yes"),
    iycf_7n = c("yes", "no", "yes"),
    iycf_6b = c("yes", "yes", "yes"),
    iycf_7l = c("yes", "yes", "yes")
  )

  suppressWarnings(out <- suppressMessages(add_iycf_mdd(df)))

  expect_equal(out$iycf_mdd_score, c(5, 4, 5))
  expect_equal(out$iycf_mdd_cat, c(1, 0, NA))
})

test_that("add_iycf_mdd() preserves zero scores when all groups are absent in eligible children", {
  df <- make_iycf_data(age_months = 12)

  suppressWarnings(out <- suppressMessages(add_iycf_mdd(df)))

  expect_equal(out$iycf_mdd_score, 0)
  expect_equal(out$iycf_mdd_cat, 0)
})

test_that("add_iycf_mdd() errors on empty or incomplete datasets", {
  expect_error(add_iycf_mdd(make_iycf_data(n = 0)))
  expect_error(add_iycf_mdd(tibble::tibble(age_months = 12, iycf_4 = "yes")))
})
