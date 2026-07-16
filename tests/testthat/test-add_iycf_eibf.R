# Tests for add_iycf_eibf

test_that("add_iycf_eibf() marks codes 1 and 2 as early initiation", {
  df <- make_iycf_data(
    age_months = c(0, 23, 23, 23, 24),
    iycf_2 = c(1, 2, 3, NA, 1)
  )

  out <- add_iycf_eibf(df)

  expect_equal(out$iycf_eibf, c(1, 1, 0, NA, NA))
})

test_that("add_iycf_eibf() coerces non-numeric ages to NA outputs", {
  df <- make_iycf_data(age_months = c("4", "unknown"), iycf_2 = c(1, 1))

  out <- add_iycf_eibf(df)

  expect_equal(out$iycf_eibf, c(1, NA))
})

test_that("add_iycf_eibf() errors on empty or incomplete datasets", {
  expect_error(add_iycf_eibf(make_iycf_data(n = 0)))
  expect_error(add_iycf_eibf(tibble::tibble(age_months = 1:2)))
})
