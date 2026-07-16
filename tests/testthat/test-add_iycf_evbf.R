# Tests for add_iycf_evbf

test_that("add_iycf_evbf() flags ever-breastfed children within the eligible age range", {
  df <- make_iycf_data(
    age_months = c(0, 23, 24, NA),
    iycf_1 = c("yes", "no", "yes", "yes")
  )

  out <- suppressMessages(add_iycf_evbf(df))

  expect_equal(out$iycf_evbf, c(1, 0, NA, NA))
})

test_that("add_iycf_evbf() errors on empty datasets", {
  df <- make_iycf_data(n = 0)

  expect_error(add_iycf_evbf(df))
})

test_that("add_iycf_evbf() errors when required columns are missing", {
  df <- tibble::tibble(age_months = c(1, 2))

  expect_error(add_iycf_evbf(df))
})
