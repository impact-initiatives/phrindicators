# Tests for add_iycf_ebf

test_that("add_iycf_ebf() requires breastfeeding with no other foods or liquids for children 0-5 months", {
  df <- make_iycf_data(
    age_months = c(0, 5, 5, 6),
    iycf_4 = c("yes", "yes", "no", "yes"),
    iycf_6a = c("no", "yes", "no", "no")
  )

  out <- suppressMessages(add_iycf_ebf(df))

  expect_equal(out$iycf_ebf, c(1, 0, 0, NA))
})

test_that("add_iycf_ebf() warns when only partial food and liquid histories are available", {
  df <- make_iycf_data(age_months = 2, iycf_4 = "yes", iycf_6a = "no", iycf_7a = "no") |>
    dplyr::select(age_months, iycf_4, iycf_6a, iycf_7a)

  expect_warning(out <- add_iycf_ebf(df))
  expect_equal(out$iycf_ebf, 1)
})

test_that("add_iycf_ebf() errors without at least one food and one liquid column or on empty data", {
  expect_error(add_iycf_ebf(tibble::tibble(age_months = 2, iycf_4 = "yes")))
  expect_error(add_iycf_ebf(make_iycf_data(n = 0)))
})
