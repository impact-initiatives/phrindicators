# Tests for add_iycf_eff

test_that("add_iycf_eff() flags egg or flesh food consumption for children 6-23 months", {
  df <- make_iycf_data(
    age_months = c(6, 23, 23, 5, 24),
    iycf_7i = c("yes", "no", "no", "yes", "yes"),
    iycf_7l = c("no", "no", "yes", "no", "no")
  )

  out <- add_iycf_eff(df)

  expect_equal(out$iycf_eff, c(1, 0, 1, NA, NA))
})

test_that("add_iycf_eff() returns NA when any required food variable is missing for a row", {
  df <- make_iycf_data(age_months = 12, iycf_7i = NA)

  out <- add_iycf_eff(df)

  expect_true(is.na(out$iycf_eff))
})

test_that("add_iycf_eff() errors on empty or incomplete datasets", {
  expect_error(add_iycf_eff(make_iycf_data(n = 0)))
  expect_error(add_iycf_eff(tibble::tibble(age_months = 12, iycf_7i = "yes")))
})
