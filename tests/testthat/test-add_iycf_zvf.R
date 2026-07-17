# Tests for add_iycf_zvf

test_that("add_iycf_zvf() identifies zero fruit and vegetable consumption", {
  df <- make_iycf_data(
    age_months = c(6, 23, 23, 5),
    iycf_7c = c("no", "yes", NA, "no"),
    iycf_7e = c("no", "no", "no", "no"),
    iycf_7f = c("no", "no", "no", "no"),
    iycf_7g = c("no", "no", "no", "no"),
    iycf_7h = c("no", "no", "no", "no")
  )

  out <- suppressMessages(add_iycf_zvf(df))

  expect_equal(out$iycf_zvf, c(1, 0, NA, NA))
})

test_that("add_iycf_zvf() warns before overwriting an existing output column", {
  df <- make_iycf_data(age_months = 12, iycf_zvf = 99)

  expect_warning(add_iycf_zvf(df))
})

test_that("add_iycf_zvf() errors on empty or incomplete datasets", {
  expect_error(add_iycf_zvf(make_iycf_data(n = 0)))
  expect_error(add_iycf_zvf(tibble::tibble(age_months = 12, iycf_7c = "no")))
})
