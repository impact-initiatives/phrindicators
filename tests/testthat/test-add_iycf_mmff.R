# Tests for add_iycf_mmff

test_that("add_iycf_mmff() counts dairy feeds for non-breastfed children 6-23 months", {
  df <- make_iycf_data(
    age_months = c(6, 23, 12, 24),
    iycf_4 = c("no", "no", "yes", "no"),
    iycf_6b_num = c(1, 1, 1, 1),
    iycf_6c_num = c(1, 0, 1, 1),
    iycf_6d_num = c(0, 0, 1, 1),
    iycf_7a_num = c(0, 0, 1, 1)
  )

  out <- suppressMessages(add_iycf_mmff(df))

  expect_equal(out$iycf_mmff, c(1, 0, 0, NA))
})

test_that("add_iycf_mmff() returns NA when a dairy count is missing", {
  df <- make_iycf_data(age_months = 10, iycf_4 = "no", iycf_6b_num = NA)

  out <- suppressMessages(add_iycf_mmff(df))

  expect_true(is.na(out$iycf_mmff))
})

test_that("add_iycf_mmff() errors on empty or incomplete datasets", {
  expect_error(add_iycf_mmff(make_iycf_data(n = 0)))
  expect_error(add_iycf_mmff(tibble::tibble(age_months = 12, iycf_4 = "no", iycf_6b_num = 1)))
})
