# Tests for add_iycf_cbf

test_that("add_iycf_cbf() applies the 12-23 month eligibility window", {
  df <- make_iycf_data(
    age_months = c(11, 12, 23, 24, NA),
    iycf_4 = c("yes", "yes", "no", "yes", "yes")
  )

  out <- suppressMessages(add_iycf_cbf(df))

  expect_equal(out$iycf_cbf, c(NA, 1, 0, NA, NA))
})

test_that("add_iycf_cbf() errors on empty datasets", {
  expect_error(add_iycf_cbf(make_iycf_data(n = 0)))
})

test_that("add_iycf_cbf() errors when required columns are missing", {
  df <- tibble::tibble(iycf_4 = c("yes", "no"))

  expect_error(add_iycf_cbf(df))
})
