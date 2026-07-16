# Tests for add_iycf_mixmf

test_that("add_iycf_mixmf() identifies mixed milk feeding among breastfed infants 0-5 months", {
  df <- make_iycf_data(
    age_months = c(0, 5, 5, 6, NA),
    iycf_4 = c("yes", "yes", "no", "yes", "yes"),
    iycf_6b = c("yes", "no", "yes", "yes", "yes"),
    iycf_6c = c("no", "no", "no", "no", "no")
  )

  out <- add_iycf_mixmf(df)

  expect_equal(out$iycf_mixmf, c(1, 0, 0, NA, NA))
})

test_that("add_iycf_mixmf() errors on empty datasets", {
  expect_error(add_iycf_mixmf(make_iycf_data(n = 0)))
})

test_that("add_iycf_mixmf() errors when required columns are missing", {
  df <- tibble::tibble(age_months = c(1, 2), iycf_4 = c("yes", "no"))

  expect_error(add_iycf_mixmf(df))
})
