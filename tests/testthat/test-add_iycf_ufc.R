# Tests for add_iycf_ufc

test_that("add_iycf_ufc() flags unhealthy food consumption for children 6-23 months", {
  df <- make_iycf_data(
    age_months = c(6, 23, 23, 5),
    iycf_7p = c("yes", "no", "no", "yes"),
    iycf_7q = c("no", "yes", "no", "no")
  )

  out <- add_iycf_ufc(df)

  expect_equal(out$iycf_ufc, c(1, 1, 0, NA))
})

test_that("add_iycf_ufc() returns NA when unhealthy food inputs are missing", {
  df <- make_iycf_data(age_months = 10, iycf_7p = NA, iycf_7q = "no")

  out <- add_iycf_ufc(df)

  expect_true(is.na(out$iycf_ufc))
})

test_that("add_iycf_ufc() errors on empty or incomplete datasets", {
  expect_error(add_iycf_ufc(make_iycf_data(n = 0)))
  expect_error(add_iycf_ufc(tibble::tibble(age_months = 12, iycf_7p = "yes")))
})
