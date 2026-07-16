# Tests for add_iycf_ebf2d

test_that("add_iycf_ebf2d() treats code 2 as exclusive breastfeeding in the first two days", {
  df <- make_iycf_data(
    age_months = c(0, 23, 23, 24, NA),
    iycf_3 = c(2, 1, 2, 2, 2)
  )

  out <- suppressMessages(add_iycf_ebf2d(df))

  expect_equal(out$iycf_ebf2d, c(1, 0, 1, NA, NA))
})

test_that("add_iycf_ebf2d() errors on empty datasets", {
  expect_error(add_iycf_ebf2d(make_iycf_data(n = 0)))
})

test_that("add_iycf_ebf2d() errors when required columns are missing", {
  df <- tibble::tibble(age_months = c(1, 2))

  expect_error(add_iycf_ebf2d(df))
})
