# Tests for add_iycf_mmf

test_that("add_iycf_mmf() applies meal-frequency thresholds by age and breastfeeding status", {
  df <- make_iycf_data(
    age_months = c(6, 8, 9, 12, 12, 5),
    iycf_4 = c("yes", "yes", "yes", "no", "no", "yes"),
    iycf_6b_num = c(0, 0, 0, 1, 1, 0),
    iycf_6c_num = c(0, 0, 0, 1, 1, 0),
    iycf_6d_num = c(0, 0, 0, 1, 0, 0),
    iycf_8 = c(2, 1, 3, 1, 0, 2)
  )

  out <- add_iycf_mmf(df)

  expect_equal(out$iycf_mmf, c(1, 0, 1, 1, 0, NA))
})

test_that("add_iycf_mmf() returns NA when numeric feeding counts are missing", {
  df <- make_iycf_data(age_months = 12, iycf_4 = "no", iycf_6b_num = NA, iycf_8 = 2)

  out <- add_iycf_mmf(df)

  expect_true(is.na(out$iycf_mmf))
})

test_that("add_iycf_mmf() errors on empty or incomplete datasets", {
  expect_error(add_iycf_mmf(make_iycf_data(n = 0)))
  expect_error(add_iycf_mmf(tibble::tibble(age_months = 12, iycf_4 = "yes", iycf_8 = 2)))
})
