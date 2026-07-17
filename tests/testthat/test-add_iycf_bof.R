# Tests for add_iycf_bof

test_that("add_iycf_bof() flags bottle feeding for children under 24 months", {
  df <- make_iycf_data(
    age_months = c(0, 23, 23, 24, NA),
    iycf_5 = c("yes", "no", "yes", "yes", "yes")
  )

  out <- suppressMessages(add_iycf_bof(df))

  expect_equal(out$iycf_bof, c(1, 0, 1, NA, NA))
})

test_that("add_iycf_bof() warns before overwriting an existing indicator column", {
  df <- make_iycf_data(age_months = 6, iycf_5 = "yes", iycf_bof = 99)

  expect_warning(add_iycf_bof(df))
})

test_that("add_iycf_bof() errors on empty or incomplete datasets", {
  expect_error(add_iycf_bof(make_iycf_data(n = 0)))
  expect_error(add_iycf_bof(tibble::tibble(age_months = 1:2)))
})
