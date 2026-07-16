# Tests for add_iycf_swb

test_that("add_iycf_swb() flags sweet beverage consumption for children 6-23 months", {
  df <- make_iycf_data(
    age_months = c(6, 23, 23, 5),
    iycf_6e = c("yes", "no", "no", "yes"),
    iycf_6f = c("no", "no", NA, "no")
  )

  out <- suppressMessages(add_iycf_swb(df))

  expect_equal(out$iycf_swb, c(1, 0, NA, NA))
})

test_that("add_iycf_swb() warns before overwriting an existing column", {
  df <- make_iycf_data(age_months = 12, iycf_swb = 99)

  expect_warning(add_iycf_swb(df))
})

test_that("add_iycf_swb() errors on empty or incomplete datasets", {
  expect_error(add_iycf_swb(make_iycf_data(n = 0)))
  expect_error(add_iycf_swb(tibble::tibble(age_months = 12, iycf_6e = "yes")))
})
