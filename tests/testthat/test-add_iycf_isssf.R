# Tests for add_iycf_isssf

test_that("add_iycf_isssf() identifies solid or semi-solid food introduction at 6-8 months", {
  df <- make_iycf_data(
    age_months = c(6, 8, 8, 9, 5),
    iycf_7a = c("yes", "no", "no", "yes", "yes"),
    iycf_7b = c("no", "yes", "no", "no", "no")
  )

  out <- add_iycf_isssf(df)

  expect_equal(out$iycf_isssf, c(1, 1, 0, NA, NA))
})

test_that("add_iycf_isssf() warns when optional food columns are missing but still computes", {
  df <- make_iycf_data(age_months = c(6, 8), iycf_7a = c("yes", "no")) |>
    dplyr::select(age_months, iycf_7a)

  expect_warning(out <- add_iycf_isssf(df))
  expect_equal(out$iycf_isssf, c(1, 0))
})

test_that("add_iycf_isssf() errors without food columns or on empty data", {
  expect_error(add_iycf_isssf(tibble::tibble(age_months = c(6, 7))))
  expect_error(add_iycf_isssf(make_iycf_data(n = 0)))
})
